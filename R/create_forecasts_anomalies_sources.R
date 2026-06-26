#' Create Forecast Anomalies Sources Lookup
#'
#' Creates a tibble pairing each date with its most recent available forecast file.
#' Uses left join to preserve all dates and maintain consistent branch identity.
#' Dates beyond forecast coverage will have NA for forecast_file.
#'
#' @author Assistant and Nathan Layman
#'
#' @param ecmwf_forecasts_transformed Character vector of forecast file paths (used as
#'   a dependency signal to ensure transforms run before this target; not used as the
#'   file list when ecmwf_forecasts_transformed_directory is provided).
#' @param dates_to_process Vector of dates to process
#' @param ecmwf_forecasts_transformed_directory Character. Directory containing all
#'   transformed ECMWF forecast parquets. When provided, all parquets in the directory
#'   are used rather than only the currently-tracked ecmwf_forecasts_transformed files,
#'   which may cover only the current month in an incremental monthly run.
#'
#' @return A tibble with columns: date, forecast_file
#'
#' @export
create_forecasts_anomalies_sources <- function(ecmwf_forecasts_transformed,
                                               dates_to_process,
                                               ecmwf_forecasts_transformed_directory = NULL) {

  # Scan the full directory for all transformed forecast parquets when the directory is
  # provided. ecmwf_forecasts_transformed may only contain the current run's new file
  # when the pipeline is running in incremental monthly mode; the directory scan ensures
  # all historical forecasts are available for the date-to-file pairing.
  if (!is.null(ecmwf_forecasts_transformed_directory)) {
    forecast_paths <- list.files(
      ecmwf_forecasts_transformed_directory,
      pattern    = "^ecmwf_seasonal_forecast_\\d+_\\d{4}\\.parquet$",
      full.names = TRUE
    )
  } else {
    forecast_paths <- ecmwf_forecasts_transformed[!is.na(ecmwf_forecasts_transformed)]
  }

  # If no forecast parquets exist yet (e.g. first run before any ECMWF transforms
  # complete), return NA forecast_file for all dates rather than erroring on an
  # empty open_dataset call.
  if (length(forecast_paths) == 0) {
    message("No ECMWF forecast parquets found; returning empty sources table")
    return(tibble::tibble(date = dates_to_process, forecast_file = NA_character_))
  }

  # Build basename lookup for converting absolute paths to relative paths
  basename_lookup <- tibble::tibble(
    original_path = forecast_paths,
    basename = basename(forecast_paths)
  )

  # Read actual base_date from files using fast dataset approach
  # Opens all files as one dataset for efficient metadata reading
  forecast_files <- arrow::open_dataset(forecast_paths) |>
    dplyr::select(base_date) |>
    dplyr::mutate(file = arrow:::add_filename()) |>
    dplyr::group_by(file, base_date) |>
    dplyr::summarise(.groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(basename = basename(file)) |>
    dplyr::left_join(basename_lookup, by = "basename") |>
    dplyr::select(-basename, -file) |>
    dplyr::rename(file = original_path) |>
    dplyr::arrange(desc(base_date))

  # For each date, find most recent forecast file available at that date
  forecast_lookup <- tibble::tibble(
    date = dates_to_process,
    forecast_file = purrr::map_chr(dates_to_process, ~{
      idx <- which(forecast_files$base_date <= .x)
      if (length(idx) > 0) forecast_files$file[idx[1]] else NA_character_
    })
  )

  # Return all dates with left join to preserve branch identity
  tibble::tibble(date = dates_to_process) |>
    dplyr::left_join(forecast_lookup, by = "date")
}
