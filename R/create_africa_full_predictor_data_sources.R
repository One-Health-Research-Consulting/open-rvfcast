#' Create Africa Full Predictor Data Sources Lookup
#'
#' Creates a tibble pairing each date with its corresponding predictor data files.
#' Reads actual data files to determine which dates are present in each file.
#' Uses left joins to preserve all dates and maintain consistent branch identity.
#'
#' weather_anomalies is now computed from ERA5T for all dates, so no separate
#' fallback is needed. NDVI anomalies are carried forward up to ndvi_carryforward_days
#' when an exact-date file is absent (Sentinel-3 composites have a ~14-day effective lag).
#'
#' @author Assistant and Nathan Layman
#'
#' @param forecasts_anomalies Character vector of forecast anomaly file paths
#' @param weather_anomalies Character vector of weather anomaly file paths
#' @param ndvi_anomalies Character vector of NDVI anomaly file paths
#' @param dates_to_process Vector of dates to process
#' @param ndvi_carryforward_days Integer. Maximum number of days to carry the most recent
#'   NDVI anomaly forward when an exact date match is absent (default 21).
#'
#' @return A tibble with columns: date, forecasts_anomalies, weather_anomalies, ndvi_anomalies
#'
#' @export
create_africa_full_predictor_data_sources <- function(forecasts_anomalies,
                                                      weather_anomalies,
                                                      ndvi_anomalies,
                                                      dates_to_process,
                                                      ndvi_carryforward_days = 21,
                                                      ndvi_anomalies_directory = NULL) {

  # Reusable function to build lookup from file list
  # Opens all files as a single dataset for faster metadata reading
  # Uses basename matching to convert absolute paths back to relative paths
  build_lookup <- function(file_list, column_name) {
    ## Guard: return empty lookup when no files are available (e.g. errored upstream branch)
    if (length(file_list) == 0) {
      return(tibble::tibble(date = as.Date(character(0)), !!column_name := character(0)))
    }

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

  # Scan the full ndvi_anomalies directory when provided. ndvi_anomalies may only
  # contain the current run's new files when the pipeline is running in incremental
  # monthly mode; the directory scan ensures all historical anomaly files are
  # available for the NDVI carry-forward look-back (up to ndvi_carryforward_days prior).
  ndvi_anomaly_files <- if (!is.null(ndvi_anomalies_directory)) {
    list.files(ndvi_anomalies_directory, pattern = "\\.parquet$", full.names = TRUE)
  } else {
    ndvi_anomalies[!is.na(ndvi_anomalies)]
  }

  # Build lookups for each predictor type
  forecasts_lookup <- build_lookup(forecasts_anomalies,  "forecasts_anomalies")
  weather_lookup   <- build_lookup(weather_anomalies,    "weather_anomalies")
  ndvi_lookup      <- build_lookup(ndvi_anomaly_files,   "ndvi_anomalies")

  # Start with dates_to_process and left join all predictors.
  # Left joins preserve all dates to maintain consistent tar_group numbers;
  # missing predictors will be NA, which downstream handles with error = "null".
  result <- tibble::tibble(date = dates_to_process) |>
    dplyr::left_join(forecasts_lookup, by = "date") |>
    dplyr::left_join(weather_lookup,   by = "date") |>
    dplyr::left_join(ndvi_lookup,      by = "date")

  # Apply NDVI carry-forward: when no exact-date NDVI file exists, substitute the most
  # recent available NDVI anomaly within ndvi_carryforward_days days prior to the target date
  fill_ndvi_carryforward(result, ndvi_lookup, max_days = ndvi_carryforward_days)

}

#' Fill Missing NDVI Anomaly Dates by Carrying Forward the Most Recent Available File
#'
#' For rows where ndvi_anomalies is NA, looks backwards up to max_days to find the most recent
#' date with an available NDVI anomaly file and substitutes that file path. The substitution
#' is appropriate for the NDVI lag scenario: Sentinel-3 10-day composites have a ~14-day lag,
#' and the most recent composite is a good proxy for the current 10-day window.
#'
#' @param data Tibble containing an ndvi_anomalies column.
#' @param ndvi_lookup Tibble with columns date and ndvi_anomalies from build_lookup.
#' @param max_days Integer. Maximum number of days to look back for a carry-forward value.
#' @return data with ndvi_anomalies NAs replaced where a suitable prior date exists.
fill_ndvi_carryforward <- function(data, ndvi_lookup, max_days = 21) {

  missing_rows <- which(is.na(data$ndvi_anomalies))
  if (length(missing_rows) == 0) return(data)

  available <- ndvi_lookup |>
    dplyr::filter(!is.na(ndvi_anomalies)) |>
    dplyr::arrange(date)

  if (nrow(available) == 0) return(data)

  for (i in missing_rows) {
    target_date <- data$date[i]
    # Only look backwards: carry the most recent past value forward
    candidates  <- available[available$date >= (target_date - max_days) & available$date < target_date, ]
    if (nrow(candidates) == 0) next
    data$ndvi_anomalies[i] <- candidates$ndvi_anomalies[nrow(candidates)]
  }

  data

}
