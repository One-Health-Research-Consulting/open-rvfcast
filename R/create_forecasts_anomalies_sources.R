#' Create Forecast Anomalies Sources Lookup
#'
#' Creates a tibble pairing each date with its most recent available forecast file.
#' Uses left join to preserve all dates and maintain consistent branch identity.
#' Dates beyond forecast coverage will have NA for forecast_file.
#'
#' @author Assistant and Nathan Layman
#'
#' @param ecmwf_forecasts_transformed Character vector of forecast file paths
#' @param dates_to_process Vector of dates to process
#'
#' @return A tibble with columns: date, forecast_file
#'
#' @export
create_forecasts_anomalies_sources <- function(ecmwf_forecasts_transformed,
                                               dates_to_process) {
  # Build basename lookup for converting absolute paths to relative paths
  basename_lookup <- tibble::tibble(
    original_path = ecmwf_forecasts_transformed,
    basename = basename(ecmwf_forecasts_transformed)
  )

  # Read actual base_date from files using fast dataset approach
  # Opens all files as one dataset for efficient metadata reading
  forecast_files <- arrow::open_dataset(ecmwf_forecasts_transformed) |>
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
