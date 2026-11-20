#' Fetch and Transform ERA5T Near Real-Time Weather Data from ARCO-ERA5
#'
#' Same output as fetch_and_transform_era5t_weather(), but builds the daily fields from the
#' hourly ERA5/ERA5T data in Google's public ARCO-ERA5 Zarr store instead of requesting the
#' derived-era5-single-levels-daily-statistics product from the CDS. The CDS queue for that
#' product grew to multi-hour waits per request in late 2026; ARCO-ERA5 needs no account,
#' has no queue, and reproduces the CDS daily statistics to float precision (daily means of
#' hours 00-23 UTC; precipitation sums of the hourly accumulations stamped 01:00 on the day
#' through 00:00 on the following day).
#'
#' Data are read with GDAL's multidimensional API (sf::gdal_utils("mdimtranslate"))
#'
#' @param months_to_process Character. Year-month in "YYYY-MM" format.
#' @param continent_raster_template Wrapped SpatRaster used for spatial alignment.
#' @param local_folder Character. Directory for output parquet and temporary daily rasters.
#' @param basename_template Character. Glue template for the output parquet filename.
#' @param overwrite Logical. Re-download and reprocess even if the output file already exists.
#' @param arco_url Character. URL of the ARCO-ERA5 hourly 0.25 degree Zarr store.
#' @param ... Ignored; present so that a targets dependency (e.g. era5t_weather_transformed_AWS)
#'   can be passed positionally without triggering an "unused argument" error.
#'
#' @return Character. File path to the transformed parquet, or NULL on failure.
#' @export

fetch_and_transform_era5t_weather_arco <- function(
  months_to_process
, continent_raster_template
, local_folder      = "data/era5t_weather_transformed"
, basename_template = "era5t_weather_transformed_{months_to_process}.parquet"
, overwrite         = FALSE
, ...
  ## After ... so an unnamed targets dependency can never be matched to it by position
, arco_url          = "https://storage.googleapis.com/gcp-public-data-arco-era5/ar/full_37-1h-0p25deg-chunk-1.zarr-v3"
) {

  ## Unwrap the template and make sure exactly one month is handled per branch
  continent_raster_template <- terra::unwrap(continent_raster_template)
  stopifnot(length(months_to_process) == 1)

  ## Derive date range for the requested month
  start_date        <- lubridate::ymd(paste0(months_to_process, "-01"))
  last_day_of_month <- lubridate::ceiling_date(start_date, "month") - lubridate::days(1)

  ## Latest day ARCO can supply in full. Precipitation for day D needs the 00:00 hour of D + 1,
   ## so the last complete day is one day before the store's last ERA5T date
  era5t_cutoff <- get_arco_era5t_stop_date(arco_url) - 1
  end_date     <- min(last_day_of_month, era5t_cutoff)

  ## Nothing to do if the month starts after the newest available data
  if (end_date < start_date) {
    message(glue::glue("ARCO-ERA5T data for {months_to_process} not yet available (store ends {era5t_cutoff}), skipping"))
    return(NULL)
  }

  message(glue::glue("Processing ARCO-ERA5T weather data for month {months_to_process} through {end_date}"))

  ## Check for an existing output file before downloading anything
  transformed_file        <- file.path(local_folder, glue::glue(basename_template))
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  existing_data           <- error_safe_read_parquet(transformed_file)

  ## Skip the month when the existing parquet already reaches the newest available day
  if (!is.null(existing_data) && !overwrite) {
    max_parquet_date <- existing_data |>
      dplyr::summarise(max_date = max(date, na.rm = TRUE)) |>
      dplyr::collect() |>
      dplyr::pull(max_date) |>
      as.Date()

    if (max_parquet_date >= end_date) {
      message(glue::glue("{basename(transformed_file)} is up to date through {end_date}, skipping"))
      return(transformed_file)
    }

    message(glue::glue(
      "{basename(transformed_file)} covers through {max_parquet_date} but ARCO-ERA5T now provides ",
      "data through {end_date}; reprocessing to extend coverage."
    ))
  }

  ## Africa bounding box from the template, in N, W, S, E order
  bbox <- terra::ext(continent_raster_template)
  area <- unname(round(c(
    as.numeric(bbox$ymax)
  , as.numeric(bbox$xmin)
  , as.numeric(bbox$ymin)
  , as.numeric(bbox$xmax)
  ), 1))

  ## One daily GeoTIFF per variable; kept until the parquet is written so an interrupted run
   ## can reuse variables that already finished
  dates_in_month <- seq(start_date, end_date, by = "day")
  ym_tag         <- format(start_date, "%Y_%m")
  daily_files    <- c(
    t2m = file.path(local_folder, glue::glue("era5t_arco_t2m_{ym_tag}.tif"))
  , d2m = file.path(local_folder, glue::glue("era5t_arco_d2m_{ym_tag}.tif"))
  , tp  = file.path(local_folder, glue::glue("era5t_arco_tp_{ym_tag}.tif")))

  ## ARCO array name and daily statistic for each variable
  arco_specs <- list(
    t2m = list(array = "2m_temperature",          statistic = "mean")
  , d2m = list(array = "2m_dewpoint_temperature", statistic = "mean")
  , tp  = list(array = "total_precipitation",     statistic = "sum"))

  ## Build each daily raster unless a complete one is already on disk
  for (v in names(daily_files)) {
    if (!overwrite && arco_daily_file_is_complete(daily_files[[v]], length(dates_in_month))) {
      message(glue::glue("{basename(daily_files[[v]])} from a previous run covers all {length(dates_in_month)} days, reusing it"))
      next
    }
    build_arco_daily_raster(
      arco_url   = arco_url
    , array      = arco_specs[[v]]$array
    , statistic  = arco_specs[[v]]$statistic
    , start_date = start_date
    , end_date   = end_date
    , area       = area
    , out_file   = daily_files[[v]])
  }

  ## Load the daily stacks; each layer is one calendar day
  temp_rast   <- terra::rast(daily_files[["t2m"]])
  dew_rast    <- terra::rast(daily_files[["d2m"]])
  precip_rast <- terra::rast(daily_files[["tp"]])

  ## Convert units, derive relative humidity and resample each day to the template.
  ## Identical to fetch_and_transform_era5t_weather() so both sources give the same parquet
  era5t_weather <- purrr::map_df(seq_along(dates_in_month), function(i) {

    ## Kelvin to Celsius
    t_c <- temp_rast[[i]] - 273.15
    d_c <- dew_rast[[i]]  - 273.15

    ## Metres per day to mm per day, clipping float negatives
    p_mm <- terra::clamp(precip_rast[[i]] * 1000, lower = 0)

    ## Relative humidity via the Magnus formula
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
      dplyr::mutate(date = dates_in_month[i])
  })

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

  ## Remove the temporary daily rasters
  purrr::walk(daily_files, ~if (file.exists(.x)) file.remove(.x))

  transformed_file

}

#' Read the Last ERA5T Date Available in ARCO-ERA5
#'
#' The store's root attributes record how far the preliminary ERA5T data extend
#' (valid_time_stop_era5t). Hours beyond that date exist in the array but are NaN.
#'
#' @param arco_url Character. URL of the ARCO-ERA5 Zarr store.
#'
#' @return Date. Last date with ERA5T data.
get_arco_era5t_stop_date <- function(arco_url) {

  ## Root attributes are a small JSON file at the top of the store
  attrs <- jsonlite::fromJSON(paste0(arco_url, "/.zattrs"))
  as.Date(attrs$valid_time_stop_era5t)

}

#' Build One Variable's Daily Raster Stack from ARCO-ERA5 Hourly Data
#'
#' Pulls the hourly values for the month and area, then aggregates them to days the same way
#' the CDS daily statistics do: a mean over hours 00-23 UTC, or for accumulated variables a sum
#' over the hours stamped 01:00 on the day through 00:00 on the next day (each hourly
#' precipitation value is the accumulation over the hour ending at its time stamp).
#'
#' @param arco_url Character. URL of the ARCO-ERA5 Zarr store.
#' @param array Character. ARCO array name (e.g. "2m_temperature").
#' @param statistic Character. "mean" or "sum".
#' @param start_date,end_date Date. First and last day to build.
#' @param area Numeric vector. Bounding box in order N, W, S, E.
#' @param out_file Character. Output GeoTIFF path; one layer per day.
#'
#' @return Character. out_file, invisibly.
build_arco_daily_raster <- function(arco_url, array, statistic, start_date, end_date, area, out_file) {

  ## Accumulated variables are shifted one hour later so each day ends at 00:00 of the next
  hour_shift <- if (statistic == "sum") 1 else 0
  n_days     <- as.integer(end_date - start_date) + 1

  ## ARCO time coordinate is hours since 1900-01-01 UTC
  first_hour <- as.numeric(difftime(as.POSIXct(start_date, tz = "UTC"), as.POSIXct("1900-01-01", tz = "UTC"), units = "hours")) + hour_shift
  last_hour  <- first_hour + n_days * 24 - 1

  message(glue::glue("Fetching {array} hourly fields from ARCO-ERA5 for {start_date} to {end_date}"))
  hourly <- fetch_arco_hourly(arco_url, array, first_hour, last_hour, area)

  ## Every hour must be present and filled; NaN hours mean the store has not reached them yet
  if (terra::nlyr(hourly) != n_days * 24) {
    stop(glue::glue("ARCO-ERA5 returned {terra::nlyr(hourly)} hours of {array}, expected {n_days * 24}"))
  }
  filled <- terra::global(hourly, "notNA")$notNA
  if (any(filled == 0)) {
    stop(glue::glue("ARCO-ERA5 {array} has {sum(filled == 0)} empty hours between {start_date} and {end_date}"))
  }

  ## Collapse each block of 24 hours into one day
  day_index <- rep(seq_len(n_days), each = 24)
  daily     <- terra::tapp(hourly, index = day_index, fun = statistic)
  names(daily) <- format(seq(start_date, end_date, by = "day"))

  ## Write to a temporary name and rename, so an interrupted write never looks complete
  tmp_file <- paste0(out_file, ".part.tif")
  terra::writeRaster(daily, tmp_file, overwrite = TRUE)
  file.rename(tmp_file, out_file)

  ## Delete the hourly temp files (about 1 GB per variable-month) now that the daily stack is saved
  unlink(c(terra::sources(hourly), terra::sources(daily)))

  invisible(out_file)

}

#' Check Whether a Daily ARCO Raster from a Previous Run Can Be Reused
#'
#' @param daily_file Character. Path to a daily GeoTIFF.
#' @param n_days Integer. Number of days the month needs.
#'
#' @return Logical. TRUE when the file opens and has one layer per day.
arco_daily_file_is_complete <- function(daily_file, n_days) {

  ## Unreadable or missing files are treated as incomplete
  if (!file.exists(daily_file)) return(FALSE)
  tryCatch(terra::nlyr(terra::rast(daily_file)) == n_days, error = function(e) FALSE)

}

#' Fetch Hourly ARCO-ERA5 Fields for an Area
#'
#' ARCO-ERA5 stores longitude as 0-360, so an area crossing the prime meridian is read as two
#' strips (the western strip shifted back by -360) and merged.
#'
#' @param arco_url Character. URL of the ARCO-ERA5 Zarr store.
#' @param array Character. ARCO array name.
#' @param first_hour,last_hour Numeric. Inclusive range in hours since 1900-01-01 UTC.
#' @param area Numeric vector. Bounding box in order N, W, S, E.
#'
#' @return SpatRaster with one layer per hour, longitudes in -180 to 180.
fetch_arco_hourly <- function(arco_url, array, first_hour, last_hour, area) {

  ## Longitude ranges to read, in the store's 0-360 convention
  west <- area[2]
  east <- area[4]
  lon_ranges <- list()
  if (west < 0)  lon_ranges$west <- c(360 + west, 359.99)
  if (east >= 0) lon_ranges$east <- c(max(0, west), east)

  ## Read each strip into its own temporary netCDF and shift western longitudes back.
  ## GDAL/terra may already report the western strip in -180 to 180, so only shift when
  ## the strip actually comes back east of 180
  strips <- purrr::imap(lon_ranges, function(lon, nm) {
    out <- tempfile(fileext = ".nc")
    read_arco_subset(arco_url, array, first_hour, last_hour, lat = c(area[1], area[3]), lon = lon, out_file = out)
    r <- terra::rast(out)
    if (nm == "west" && terra::xmin(r) > 180) r <- terra::shift(r, dx = -360)
    r
  })

  ## Combine strips into one raster covering the whole area
  hourly <- if (length(strips) == 1) strips[[1]] else terra::merge(strips$west, strips$east)
  terra::crs(hourly) <- "EPSG:4326"

  ## Once merged, the strip files are no longer read, so free their disk space now;
  ## mirai workers persist between branches and would otherwise keep them until they exit
  if (length(strips) > 1) unlink(unlist(purrr::map(strips, terra::sources)))

  hourly

}

#' Read One Subset of an ARCO-ERA5 Array with GDAL
#'
#' @param arco_url Character. URL of the ARCO-ERA5 Zarr store.
#' @param array Character. ARCO array name.
#' @param first_hour,last_hour Numeric. Inclusive range in hours since 1900-01-01 UTC.
#' @param lat Numeric length 2. North then south latitude.
#' @param lon Numeric length 2. West then east longitude in 0-360.
#' @param out_file Character. Output netCDF path.
#'
#' @return Character. out_file, invisibly.
read_arco_subset <- function(arco_url, array, first_hour, last_hour, lat, lon, out_file) {

  ## Subsets are by coordinate value and inclusive; GDAL retries dropped HTTP requests itself
  sf::gdal_utils(
    "mdimtranslate"
  , source         = glue::glue('ZARR:"/vsicurl/{arco_url}"')
  , destination    = out_file
  , options        = c(
      "-array",  array
    , "-subset", sprintf("time(%.0f,%.0f)", first_hour, last_hour)
    , "-subset", sprintf("latitude(%s,%s)", lat[1], lat[2])
    , "-subset", sprintf("longitude(%s,%s)", lon[1], lon[2]))
  , config_options = c(
      GDAL_HTTP_MAX_RETRY          = "5"
    , GDAL_HTTP_RETRY_DELAY        = "10"
    , GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR"))

  invisible(out_file)

}
