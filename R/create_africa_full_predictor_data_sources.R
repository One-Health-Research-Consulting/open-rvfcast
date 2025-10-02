#' Create Africa Full Predictor Data Sources Lookup
#'
#' Creates a tibble pairing each date with its corresponding predictor data files.
#' Reads actual data files to determine which dates are present in each file.
#' Uses left joins to preserve all dates and maintain consistent branch identity.
#'
#' @author Assistant and Nathan Layman
#'
#' @param forecasts_anomalies Character vector of forecast anomaly file paths
#' @param weather_anomalies Character vector of weather anomaly file paths
#' @param ndvi_anomalies Character vector of NDVI anomaly file paths
#' @param dates_to_process Vector of dates to process
#'
#' @return A tibble with columns: date, forecasts_anomalies, weather_anomalies, ndvi_anomalies
#'
#' @export
create_africa_full_predictor_data_sources <- function(forecasts_anomalies,
                                                      weather_anomalies,
                                                      ndvi_anomalies,
                                                      dates_to_process) {

  # Reusable function to build lookup from file list
  # Opens all files as a single dataset for faster metadata reading
  # Uses basename matching to convert absolute paths back to relative paths
  build_lookup <- function(file_list, column_name) {
    # Create lookup table: basename → original relative path
    basename_lookup <- tibble::tibble(
      original_path = file_list,
      basename = basename(file_list)
    )

    arrow::open_dataset(file_list) |>
      dplyr::select(date) |>
      dplyr::mutate(!!column_name := arrow:::add_filename()) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::mutate(basename = basename(!!sym(column_name))) |>
      dplyr::left_join(basename_lookup, by = "basename") |>
      dplyr::select(-basename, -!!sym(column_name)) |>
      dplyr::rename(!!column_name := original_path)
  }

  # Build lookups for each predictor type
  forecasts_lookup <- build_lookup(forecasts_anomalies, "forecasts_anomalies")
  weather_lookup <- build_lookup(weather_anomalies, "weather_anomalies")
  ndvi_lookup <- build_lookup(ndvi_anomalies, "ndvi_anomalies")

  # Start with dates_to_process and left join all predictors
  # Left joins preserve all dates to maintain consistent tar_group numbers
  # Missing predictors will be NA, which downstream can handle with error = "null"
  tibble::tibble(date = dates_to_process) |>
    dplyr::left_join(forecasts_lookup, by = "date") |>
    dplyr::left_join(weather_lookup, by = "date") |>
    dplyr::left_join(ndvi_lookup, by = "date")
}
