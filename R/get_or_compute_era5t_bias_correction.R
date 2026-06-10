#' Get or Compute ERA5T Bias Correction
#'
#' Wrapper that checks for an existing per-pixel bias correction file and returns its path
#' immediately if found. If the file is absent (or overwrite is TRUE), it determines which
#' months are needed to fill a total calibration window of n_calibration_months, downloads
#' only the months not already present in era5t_weather_dir to a temporary directory, computes
#' the correction using all available files, saves the result, and cleans up the downloads.
#'
#' The correction captures the mean signed difference (ERA5T_scaled - MERRA2_scaled) at each
#' pixel, and is subtracted from ERA5T scaled anomalies before they enter the model. This
#' brings ERA5T onto the MERRA-2 climatological scale that the XGBoost model was trained on.
#'
#' A longer calibration window (n_calibration_months = 42) gives better seasonal coverage and
#' a smaller standard error on the offset estimate (~0.03 SD) compared to 18 months (~0.04 SD).
#' Any months already present in era5t_weather_dir count toward the total, so if the directory
#' already holds 18 months only the remaining months are downloaded.
#'
#' @param era5t_weather_dir Character. Directory of existing era5t_weather_transformed_*.parquet
#'   files produced by fetch_and_transform_era5t_weather.
#' @param weather_anomalies_dir Character. Directory of pre-computed MERRA-2 anomaly parquets.
#' @param weather_historical_means_dir Character. Directory of MERRA-2 historical mean parquets.
#' @param continent_raster_template Wrapped or unwrapped SpatRaster defining valid analysis pixels.
#' @param output_file Character. Full path for the output bias correction parquet.
#' @param n_calibration_months Integer. Total desired calibration window in months, counting
#'   back from the current month. Any months already in era5t_weather_dir are reused; only
#'   the gap is downloaded (default 42, giving ~3.5 years of seasonal coverage).
#' @param skip_download Logical. If TRUE, skip downloading missing calibration months and
#'   compute the correction using only files already present in era5t_weather_dir. Useful
#'   when the CDS API is unavailable or for testing with existing files (default FALSE).
#' @param overwrite Logical. Re-compute even if the output file already exists.
#' @param ... Passed through unused; allows a targets dependency (e.g. era5t_weather_transformed)
#'   to be listed positionally without triggering an "unused argument" error.
#'
#' @return Character. Path to the bias correction parquet.
#' @export
get_or_compute_era5t_bias_correction <- function(
  era5t_weather_dir
, weather_anomalies_dir
, weather_historical_means_dir
, continent_raster_template
, output_file             = "outputs/anomaly_bias_correction/era5t_bias_correction.parquet"
, n_calibration_months   = 42
, skip_download          = FALSE
, overwrite              = FALSE
, ...
) {

  if (file.exists(output_file) && !overwrite) {
    message(glue::glue("{basename(output_file)} already exists and overwrite is FALSE, skipping"))
    return(output_file)
  }

  ## Build the full set of months that the calibration window should cover
  window_end             <- lubridate::floor_date(Sys.Date(), "month") - months(1)
  window_start           <- window_end - months(n_calibration_months - 1)
  all_calibration_months <- format(seq(window_start, window_end, by = "month"), "%Y-%m")

  ## Identify which months are already on disk; only download the gap
  existing_files      <- list.files(era5t_weather_dir, pattern = "\\.parquet$", full.names = TRUE)
  existing_months    <- na.omit(stringr::str_extract(basename(existing_files), "\\d{4}-\\d{2}"))
  months_to_download <- setdiff(all_calibration_months, existing_months)

  message(glue::glue(
    "Calibration window: {all_calibration_months[1]} to {tail(all_calibration_months, 1)} ",
    "({n_calibration_months} months total; {length(existing_months)} already on disk, ",
    "{length(months_to_download)} to download)"
  ))

  if (skip_download || length(months_to_download) == 0) {
    if (skip_download && length(months_to_download) > 0) {
      message(glue::glue("skip_download is TRUE; using {length(existing_months)} existing month(s) only (skipping {length(months_to_download)} missing)"))
    } else {
      message("All calibration months already present; computing bias correction from existing files")
    }
    compute_era5t_bias_correction(
      era5t_weather_dir            = era5t_weather_dir
    , weather_anomalies_dir        = weather_anomalies_dir
    , weather_historical_means_dir = weather_historical_means_dir
    , continent_raster_template    = continent_raster_template
    , output_file                   = output_file
    , overwrite                    = TRUE
    )
    return(output_file)
  }

  ## Download missing months to a temporary directory that is cleaned up on exit
  temp_cal_dir <- file.path(
    tempdir()
  , paste0("era5t_calibration_", format(Sys.time(), "%Y%m%d%H%M%S"))
  )
  dir.create(temp_cal_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_cal_dir, recursive = TRUE), add = TRUE)

  message(glue::glue(
    "Downloading {length(months_to_download)} calibration month(s) ",
    "({months_to_download[1]} to {tail(months_to_download, 1)}) to temporary directory"
  ))

  ecmwfr::wf_set_key(user = Sys.getenv("ECMWF_USERID"), key = Sys.getenv("ECMWF_TOKEN"))

  # Download each missing month; errors are caught so a single failed month does not
  # abort the calibration run (the correction will use fewer months instead)
  purrr::walk(months_to_download, function(m) {
    tryCatch(
      fetch_and_transform_era5t_weather(
        months_to_process         = m
      , continent_raster_template = continent_raster_template
      , local_folder              = temp_cal_dir
      , basename_template         = "era5t_calibration_{months_to_process}.parquet"
      , overwrite                 = FALSE
      )
    , error = function(e) {
        message(glue::glue("  Could not download calibration month {m}: {conditionMessage(e)}"))
      }
    )
  })

  downloaded_files <- list.files(temp_cal_dir, pattern = "\\.parquet$")
  message(glue::glue("  Downloaded {length(downloaded_files)} / {length(months_to_download)} month(s)"))

  # Compute bias correction from both the live ERA5T directory and the downloaded calibration months
  compute_era5t_bias_correction(
    era5t_weather_dir            = c(era5t_weather_dir, temp_cal_dir)
  , weather_anomalies_dir        = weather_anomalies_dir
  , weather_historical_means_dir = weather_historical_means_dir
  , continent_raster_template    = continent_raster_template
  , output_file                  = output_file
  , overwrite                    = TRUE
  )

  # on.exit will remove temp_cal_dir after this function returns
  output_file

}
