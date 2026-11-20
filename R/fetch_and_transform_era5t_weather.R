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
  last_day_of_month        <- lubridate::ceiling_date(start_date, "month") - lubridate::days(1)

  if (!is.null(existing_data) && !overwrite) {
    # Determine the target ceiling: the full month end if ERA5T lag covers it,
    # otherwise the ERA5T lag limit (end_date has already been clamped above).
    target_end_date <- if (last_day_of_month <= era5t_cutoff) last_day_of_month else end_date

    max_parquet_date <- existing_data |>
      dplyr::summarise(max_date = max(date, na.rm = TRUE)) |>
      dplyr::collect() |>
      dplyr::pull(max_date) |>
      as.Date()

    if (max_parquet_date >= target_end_date) {
      message(glue::glue("{basename(transformed_file)} is up to date through {target_end_date}, skipping"))
      return(transformed_file)
    }

    message(glue::glue(
      "{basename(transformed_file)} covers through {max_parquet_date} but ERA5T now provides ",
      "data through {target_end_date}; reprocessing to extend coverage."
    ))
    # Fall through to re-download and reprocess
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

  # Remove nc files left from a previous failed run. download_era5t_nc skips existing files,
  # so stale nc files (covering fewer days than now available) would otherwise be silently
  # reused. Clearing them here forces a fresh CDS download for each processing attempt.
  ## Exception: an nc file that already holds every requested day is kept, so a run that was
   ## stopped part-way through a month resumes with only the variables still missing instead of
   ## waiting in the CDS queue again for downloads that already finished
  nc_candidates <- list(
    c("t2m", "VAR_2T", "2m_temperature")
  , c("d2m", "VAR_2D", "2m_dewpoint_temperature")
  , c("tp",  "VAR_TP", "total_precipitation"))
  purrr::walk2(c(nc_t2m_file, nc_d2m_file, nc_precip_file), nc_candidates, function(nc_file, candidates) {
    if (!file.exists(nc_file)) return(invisible(NULL))
    if (!overwrite && era5t_nc_is_complete(nc_file, candidates, n_days = length(days))) {
      message(glue::glue("{basename(nc_file)} from a previous run covers all {length(days)} days, reusing it"))
    } else {
      file.remove(nc_file)
    }
  })

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

  ## Use however many layers are actually present across all three nc files.
  ## CDS sometimes returns fewer days than requested for months near the ERA5T
  ## availability cutoff, and the nc files for a previous failed run may also
  ## be stale. Truncating dates_in_month produces a parquet with available data
  ## rather than a hard failure.
  n_available <- min(terra::nlyr(temp_rast), terra::nlyr(dew_rast), terra::nlyr(precip_rast))
  if (n_available == 0) {
    message(glue::glue("ERA5T nc files for {months_to_process} contain no data layers, skipping"))
    return(NULL)
  }
  if (n_available < length(dates_in_month)) {
    message(glue::glue(
      "ERA5T nc files for {months_to_process} have {n_available} of {length(dates_in_month)} ",
      "expected days; writing parquet with available data only."
    ))
  }
  dates_in_month <- dates_in_month[seq_len(n_available)]

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

  ## Previously wf_request was allowed to wait only 300 s; CDS daily-statistics jobs often
   ## sit in the queue longer, in which case wf_request returned without downloading, the
   ## branch failed, and the job was left orphaned in the CDS queue (where it counts toward
   ## the per-dataset queue limit and causes later requests to be rejected). Now each job is
   ## submitted without waiting (transfer = FALSE) and polled here until it downloads.
   ## Timeout, poll interval and wait-after-rejection can be tuned via .env.
  time_out      <- as.numeric(Sys.getenv("ERA5T_CDS_TIMEOUT", "43200"))
  poll_interval <- as.numeric(Sys.getenv("ERA5T_CDS_POLL_INTERVAL", "30"))
  queue_wait    <- as.numeric(Sys.getenv("ERA5T_CDS_QUEUE_WAIT", "120"))
  deadline      <- Sys.time() + time_out

  ## Holds the current CDS job so it can be cancelled if this function exits early
  cds_job <- NULL
  on.exit({
    ## On error, timeout or user interrupt, remove the job from the CDS queue so it does
     ## not block future requests; a successful job is already deleted below
    if (!is.null(cds_job) && !isTRUE(cds_job$is_success())) try(cds_job$delete(), silent = TRUE)
  }, add = TRUE)

  repeat {

    ## Submit the job; transient gateway errors (e.g. 502) are retried
    cds_job <- retry_transient_cds_error(function() {
      ecmwfr::wf_request(
        request  = request
      , user     = Sys.getenv("ECMWF_USERID")
      , path     = local_folder
      , transfer = FALSE
      , verbose  = TRUE)
    }, label = glue::glue("submitting {basename(filename)}"))

    ## Poll the same job until it downloads, ends in a terminal state, or the timeout passes.
     ## download() refreshes the status and fetches the file once it is ready; a "failed"
     ## job raises an error inside it, but a "rejected" job does not, so check explicitly.
    while (!cds_job$is_success() && !cds_job$get_status() %in% c("rejected", "dismissed", "deleted")) {
      if (Sys.time() > deadline) {
        stop(glue::glue(
          "ERA5T CDS request for {basename(filename)} not complete after {round(time_out / 60)} minutes ",
          "(status: {cds_job$get_status()}); job cancelled. ",
          "Increase ERA5T_CDS_TIMEOUT in .env or rerun later."
        ))
      }
      Sys.sleep(poll_interval)
      retry_transient_cds_error(function() cds_job$download(), label = glue::glue("polling {basename(filename)}"))
    }

    ## Downloaded: done
    if (cds_job$is_success()) break

    ## Ended without data; find out why
    reason <- get_cds_job_reason(cds_job)
    try(cds_job$delete(), silent = TRUE)

    ## Too many queued requests for this dataset is temporary: wait for the queue to drain
     ## and resubmit, as long as there is time left
    if (grepl("Maximum number queued requests", reason, ignore.case = TRUE) &&
        Sys.time() + queue_wait < deadline) {
      message(glue::glue(
        "CDS rejected {basename(filename)} because the dataset queue limit is reached; ",
        "resubmitting in {queue_wait} s"))
      Sys.sleep(queue_wait)
      next
    }

    stop(glue::glue(
      "ERA5T CDS request for {basename(filename)} ended with status '{cds_job$get_status()}': {reason}"))
  }

  ## Remove the finished job from the CDS queue; failure to delete is harmless
  try(cds_job$delete(), silent = TRUE)

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

#' Retry a CDS API Call on Transient Server Errors
#'
#' The CDS API intermittently returns gateway errors (HTTP 502/503/504) or drops
#' connections. These are not problems with the request itself, so the call is retried
#' after a pause instead of failing the whole targets branch. Any other error is
#' re-thrown immediately.
#'
#' @param f Function with no arguments that performs the CDS call.
#' @param label Character. Description of the call, used in messages.
#' @param max_attempts Integer. Maximum number of attempts.
#' @param wait Numeric. Seconds to wait before the first retry; doubles each retry.
#' @return The value returned by f().
retry_transient_cds_error <- function(f, label = "CDS request", max_attempts = 5, wait = 30) {

  # Error message fragments that indicate a temporary server or network problem
  transient_pattern <- paste(
    c("502", "503", "504", "Bad Gateway", "Service Unavailable", "Gateway Time-?out",
      "Timeout was reached", "Connection reset", "Could not resolve host", "Recv failure"),
    collapse = "|")

  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(f(), error = function(e) e)

    # Success: return the value straight away
    if (!inherits(result, "error")) return(result)

    msg <- conditionMessage(result)

    # Non-transient errors, or out of attempts: fail with a short, readable message
    if (!grepl(transient_pattern, msg, ignore.case = TRUE) || attempt == max_attempts) {
      # Gateway error pages arrive as full HTML; strip tags so the targets error field is legible
      stop(glue::glue("{label} failed after {attempt} attempt(s): ",
                      "{substr(trimws(gsub('<[^>]+>|\\\\s+', ' ', msg)), 1, 300)}"), call. = FALSE)
    }

    message(glue::glue("Transient CDS error while {label} (attempt {attempt}/{max_attempts}); ",
                       "retrying in {wait} s"))
    Sys.sleep(wait)
    wait <- wait * 2
  }
}

#' Get the Reason a CDS Job Ended Without Data
#'
#' ecmwfr does not surface why a CDS job was rejected or dismissed. The CDS jobs API
#' returns the reason (e.g. "Maximum number queued requests for this dataset has been
#' reached") in the body of the job's /results endpoint.
#'
#' @param cds_job An ecmwfr ds_service request object.
#' @return Character scalar describing the reason, or a placeholder if none is available.
get_cds_job_reason <- function(cds_job) {

  # The results endpoint for a rejected job returns a JSON error body holding the reason
  response <- tryCatch(
    httr::GET(paste0(cds_job$get_url(), "/results"),
              httr::add_headers(`PRIVATE-TOKEN` = Sys.getenv("ECMWF_TOKEN"))),
    error = function(e) NULL)

  if (is.null(response)) return("no reason available (could not reach CDS)")

  body <- tryCatch(httr::content(response, as = "parsed", encoding = "UTF-8"), error = function(e) NULL)

  # Prefer the most specific field CDS provides
  reason <- body$traceback %||% body$detail %||% body$title
  if (is.null(reason)) "no reason given by CDS" else as.character(reason)
}

#' Check Whether a Leftover ERA5T NetCDF File Covers Every Requested Day
#'
#' Used to decide if an nc file left by an interrupted run can be reused rather than
#' re-requested from CDS. A file is complete when it opens, contains one of the expected
#' variables, and has one layer per requested day. Unreadable or partial files count as
#' incomplete.
#'
#' @param nc_file Character. Path to the NetCDF file.
#' @param candidates Character vector. Variable names passed to detect_era5t_nc_variable().
#' @param n_days Integer. Number of days requested for the month.
#' @return Logical scalar.
era5t_nc_is_complete <- function(nc_file, candidates, n_days) {
  tryCatch({
    # Open just the variable of interest; each layer is one day
    nc_var <- detect_era5t_nc_variable(nc_file, candidates)
    terra::nlyr(terra::rast(nc_file, subds = nc_var)) == n_days
  }, error = function(e) FALSE)
}
