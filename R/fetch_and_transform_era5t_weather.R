#' Fetch and Transform ERA5T Near Real-Time Weather Data
#'
#' Downloads daily ERA5(T) weather data from ECMWF's Climate Data Store (CDS) for a given
#' month, transforms it to match the continental raster template, and saves the result as a
#' parquet file. ERA5T is the near real-time extension of ERA5, available with approximately a
#' 5-day lag compared to the approximately 5-week lag of NASA POWER MERRA-2, enabling monthly
#' forecast runs. The output schema matches nasa_weather_transformed parquets so that downstream
#' anomaly and prediction targets can consume either source without modification.
#'
#' Uses the same CDS credentials (ECMWF_USERID, ECMWF_TOKEN) as the ECMWF seasonal forecasts
#' already present in this pipeline. The ERA5/ERA5T license must be accepted interactively at
#' https://cds.climate.copernicus.eu/datasets/derived-era5-single-levels-daily-statistics
#' before the first download will succeed.
#'
#' @param months_to_process Character. Year-month in "YYYY-MM" format.
#' @param continent_raster_template Wrapped SpatRaster used for spatial alignment.
#' @param local_folder Character. Directory for output parquet and temporary NetCDF files.
#' @param basename_template Character. Glue template for the output parquet filename.
#' @param overwrite Logical. Re-download and reprocess even if the output file already exists.
#' @param ... Ignored; present so that a targets dependency (e.g. era5t_weather_transformed_AWS)
#'   can be passed positionally without triggering an "unused argument" error.
#'
#' @return Character. File path to the transformed parquet, or NULL on failure.
#' @export
fetch_and_transform_era5t_weather <- function(
  months_to_process
, continent_raster_template
, local_folder      = "data/era5t_weather_transformed"
, basename_template = "era5t_weather_transformed_{months_to_process}.parquet"
, overwrite         = FALSE
, ...
) {

  continent_raster_template <- terra::unwrap(continent_raster_template)

  stopifnot(length(months_to_process) == 1)

  # Derive date range for the requested month
  start_date <- lubridate::ymd(paste0(months_to_process, "-01"))
  end_date   <- lubridate::ceiling_date(start_date, "month") - lubridate::days(1)

  # ERA5T has approximately a 5-day lag; requesting dates too close to today will fail
  era5t_cutoff <- Sys.Date() - 5
  if (end_date > era5t_cutoff) end_date <- era5t_cutoff

  if (end_date < start_date) {
    message(glue::glue("ERA5T data for {months_to_process} not yet available (5-day lag applied), skipping"))
    return(NULL)
  }

  year  <- lubridate::year(start_date)
  month <- format(start_date, "%m")
  days  <- format(seq(start_date, end_date, by = "day"), "%d")

  message(glue::glue("Processing ERA5T weather data for month {months_to_process}"))

  # Check for an existing output file before downloading anything
  transformed_file         <- file.path(local_folder, glue::glue(basename_template))
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  existing_data           <- error_safe_read_parquet(transformed_file)

  if (!is.null(existing_data) && !overwrite) {
    message(glue::glue("{basename(transformed_file)} already exists, has rows, and overwrite is not TRUE, skipping"))
    return(transformed_file)
  }

  # Authenticate with CDS using the same credentials as the ECMWF seasonal forecasts
  ecmwfr::wf_set_key(user = Sys.getenv("ECMWF_USERID"), key = Sys.getenv("ECMWF_TOKEN"))

  # Build Africa bounding box in CDS format: N, W, S, E.
  # Names must be stripped: named numeric vectors serialize to a JSON object {"N":...}
  # but the CDS API requires a JSON array [N, W, S, E] — the new CDS (post-April 2025)
  # returns HTTP 500 for the object form whereas the old CDS accepted either.
  bbox <- terra::ext(continent_raster_template)
  area <- unname(round(c(
    N = as.numeric(bbox$ymax)
  , W = as.numeric(bbox$xmin)
  , S = as.numeric(bbox$ymin)
  , E = as.numeric(bbox$xmax)
  ), 1))

  # One CDS request per variable so each zip contains exactly one NetCDF file.
  # Three requests are needed: temperature and dewpoint both use daily_mean while
  # precipitation requires daily_sum, so they cannot share a single request anyway.
  nc_t2m_file    <- file.path(local_folder, glue::glue("era5t_t2m_{year}_{month}.nc"))
  nc_d2m_file    <- file.path(local_folder, glue::glue("era5t_d2m_{year}_{month}.nc"))
  nc_precip_file <- file.path(local_folder, glue::glue("era5t_precip_{year}_{month}.nc"))

  download_era5t_nc(
    variable     = "2m_temperature"
  , statistic    = "daily_mean"
  , year         = year, month = month, days = days, area = area
  , local_folder = local_folder, filename = nc_t2m_file, overwrite = overwrite)

  download_era5t_nc(
    variable     = "2m_dewpoint_temperature"
  , statistic    = "daily_mean"
  , year         = year, month = month, days = days, area = area
  , local_folder = local_folder, filename = nc_d2m_file, overwrite = overwrite)

  ## ERA5 total_precipitation daily_sum gives the 24-hour total in m/day
  download_era5t_nc(
    variable     = "total_precipitation"
  , statistic    = "daily_sum"
  , year         = year, month = month, days = days, area = area
  , local_folder = local_folder, filename = nc_precip_file, overwrite = overwrite)

  ## ERA5T NetCDF variable names follow ECMWF short-name convention but can vary by format
  t2m_var <- detect_era5t_nc_variable(nc_t2m_file,    c("t2m", "VAR_2T", "2m_temperature"))
  d2m_var <- detect_era5t_nc_variable(nc_d2m_file,    c("d2m", "VAR_2D", "2m_dewpoint_temperature"))
  tp_var  <- detect_era5t_nc_variable(nc_precip_file, c("tp",  "VAR_TP", "total_precipitation"))

  ## Load multi-layer rasters; each layer corresponds to one calendar day
  temp_rast   <- terra::rast(nc_t2m_file,    subds = t2m_var)
  dew_rast    <- terra::rast(nc_d2m_file,    subds = d2m_var)
  precip_rast <- terra::rast(nc_precip_file, subds = tp_var)

  terra::crs(temp_rast)   <- "EPSG:4326"
  terra::crs(dew_rast)    <- "EPSG:4326"
  terra::crs(precip_rast) <- "EPSG:4326"

  ## Derive dates from the known request range; terra::time() is unreliable for ERA5T NetCDFs
  ## because the time dimension encoding varies and is not always parsed correctly
  dates_in_month <- seq(start_date, end_date, by = "day")

  ## Guard against CDS returning a different number of days than requested
  if (terra::nlyr(temp_rast) != length(dates_in_month)) {
    stop(glue::glue(
      "Temperature raster has {terra::nlyr(temp_rast)} layers but expected {length(dates_in_month)} ",
      "days for {months_to_process}. CDS may have returned incomplete data."
    ))
  }

  ## Process each day: convert units, derive relative humidity, resample to template
  era5t_weather <- purrr::map_df(seq_along(dates_in_month), function(i) {

    date <- dates_in_month[i]

    ## Single-day layers for each variable
    t_lyr <- temp_rast[[i]]
    d_lyr <- dew_rast[[i]]
    p_lyr <- precip_rast[[i]]

    ## ERA5 temperatures are in Kelvin; convert to Celsius
    t_c <- t_lyr - 273.15
    d_c <- d_lyr - 273.15

    ## ERA5 daily_sum precipitation is in m/day; convert to mm/day and clip float negatives
    p_mm <- terra::clamp(p_lyr * 1000, lower = 0)

    ## Compute relative humidity via the Magnus formula, matching the approach used for
    ## ECMWF seasonal forecasts in transform_ecmwf_forecasts.R
    sat_vp  <- exp((17.625 * t_c) / (243.04 + t_c))
    act_vp  <- exp((17.625 * d_c) / (243.04 + d_c))
    rh_rast <- terra::clamp(100 * act_vp / sat_vp, lower = 0, upper = 100)

    ## Resample all three fields to the continental raster template
    t_c     <- transform_raster(t_c,     continent_raster_template)
    rh_rast <- transform_raster(rh_rast, continent_raster_template)
    p_mm    <- transform_raster(p_mm,    continent_raster_template)

    names(t_c)     <- "temperature"
    names(rh_rast) <- "relative_humidity"
    names(p_mm)    <- "precipitation"

    terra::as.data.frame(c(t_c, rh_rast, p_mm), xy = TRUE) |>
      dplyr::mutate(date = date)
  })

  if (nrow(era5t_weather) == 0) {
    message(glue::glue("No ERA5T data produced for {months_to_process}"))
    return(NULL)
  }

  ## Add date-part columns to match the schema of nasa_weather_transformed parquets
  era5t_weather <- era5t_weather |>
    dplyr::mutate(
      year  = as.integer(lubridate::year(date))
    , month = as.integer(lubridate::month(date))
    , day   = as.integer(lubridate::day(date))
    , doy   = as.integer(lubridate::yday(date))
    ) |>
    dplyr::select(x, y, date, year, month, day, doy, dplyr::everything())

  arrow::write_parquet(era5t_weather, transformed_file, compression = "gzip", compression_level = 5)

  ## Verify the written file can be read back before reporting success
  if (is.null(error_safe_read_parquet(transformed_file))) {
    file.remove(transformed_file)
    stop(glue::glue("{basename(transformed_file)} could not be read after writing."))
  }

  ## Remove temporary NetCDF downloads
  purrr::walk(c(nc_t2m_file, nc_d2m_file, nc_precip_file), ~if (file.exists(.x)) file.remove(.x))

  transformed_file

}

#' Download One ERA5T Variable Group from CDS
#'
#' Issues a single CDS API request against the derived-era5-single-levels-daily-statistics
#' dataset and saves the result as a NetCDF file. Returns early if the file already exists.
#'
#' @param variable Character scalar. A single CDS long-form variable name.
#' @param statistic Character. One of "daily_mean", "daily_sum", "daily_min", "daily_max".
#' @param year Numeric. Year to download.
#' @param month Character. Zero-padded month string (e.g. "01").
#' @param days Character vector. Zero-padded day values (e.g. c("01", "02", ...)).
#' @param area Unnamed numeric vector. Bounding box in order N, W, S, E.
#' @param local_folder Character. Directory in which to save the NetCDF file.
#' @param filename Character. Full path for the output NetCDF file.
#' @param overwrite Logical. Re-download even if the file exists.
#' @return Invisibly returns filename.
download_era5t_nc <- function(variable, statistic, year, month, days,
                              area, local_folder, filename, overwrite = FALSE) {

  stopifnot(length(variable) == 1)

  if (file.exists(filename) && !overwrite) {
    message(glue::glue("{basename(filename)} already exists, skipping CDS download"))
    return(invisible(filename))
  }

  # CDS API request structure for pre-aggregated daily ERA5(T) data on single pressure levels.
  # See: https://cds.climate.copernicus.eu/datasets/derived-era5-single-levels-daily-statistics
  # download_format = "unarchived" is required for the new CDS API (post-April 2025):
  # the old CDS returned a zip regardless of this parameter; the new CDS respects it and
  # returns the NetCDF directly, while "zip" causes HTTP 500 for this dataset.
  # CDS API expects JSON arrays for multi-select parameters even when a single value is sent.
  # ecmwfr v2.x does not auto-wrap scalars the way the Python cdsapi client does, so
  # wrapping in list() forces jsonlite to emit ["value"] instead of "value".
  # product_type, daily_statistic, time_zone, frequency, and data_format are single-select
  # and remain as scalars; variable, year, and month are multi-select and need arrays.
  request <- list(
    dataset_short_name = "derived-era5-single-levels-daily-statistics"
  , product_type       = "reanalysis"
  , variable           = variable
  , year               = as.character(year)
  , month              = month
  , day                = days
  , daily_statistic    = statistic
  , time_zone          = "UTC+00:00"
  , frequency          = "1_hourly"
  , area               = area
  , data_format        = "netcdf"
  , download_format    = "unarchived"
  , target             = basename(filename))

  ecmwfr::wf_request(
    request  = request
  , user     = Sys.getenv("ECMWF_USERID")
  , path     = local_folder
    ## Setting a shorter time out to test if there are individual file issues
     ## or if timeout is due to throttle from too many requests. Timeout
     ## printout uninformative
  , time_out = 300
  , verbose  = TRUE)

  if (!file.exists(filename)) {
    stop(glue::glue("ERA5T CDS download failed: {basename(filename)} not found after request"))
  }

  invisible(filename)

}

#' Detect the Internal Variable Name in an ERA5T NetCDF File
#'
#' ERA5T NetCDF files downloaded from CDS may use either the ECMWF CF short name (e.g. "t2m")
#' or a longer form depending on download settings. This helper tries a prioritised list of
#' candidate names and returns the first match found in the file.
#'
#' @param nc_file Character. Path to the NetCDF file.
#' @param candidates Character vector. Variable names to try, in priority order.
#' @return Character scalar. The first matching variable name found in the file.
detect_era5t_nc_variable <- function(nc_file, candidates) {

  available <- terra::varnames(terra::rast(nc_file))
  match     <- base::intersect(candidates, available)

  if (length(match) == 0) {
    stop(glue::glue(
      "None of [{paste(candidates, collapse = ', ')}] found in {basename(nc_file)}. ",
      "Available variables: [{paste(available, collapse = ', ')}]. ",
      "Update the candidates list or check the CDS download format."
    ))
  }

  match[1]
}
