#' Create Forecast Anomalies Sources Lookup
#'
#' Creates a tibble pairing each date with its most recent available forecast file.
#' Only includes dates up to the latest forecast month to avoid processing dates
#' beyond forecast coverage.
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
  # Extract base_date from forecast filenames and create lookup
  forecast_dates <- tibble(
    file = ecmwf_forecasts_transformed,
    base_date = as.Date(paste0(
      gsub(".*_(\\d+)_(\\d{4})\\.parquet", "\\2-\\1", ecmwf_forecasts_transformed),
      "-01"
    ))
  ) |>
    filter(!is.na(base_date)) |>
    arrange(desc(base_date))

  # Only process dates up to the latest forecast month
  max_forecast_month <- format(max(forecast_dates$base_date), "%Y-%m")
  valid_dates <- dates_to_process[format(dates_to_process, "%Y-%m") <= max_forecast_month]

  # For each date, find most recent forecast file
  tibble(
    date = valid_dates,
    forecast_file = map_chr(valid_dates, ~{
      idx <- which(forecast_dates$base_date <= .x)
      if (length(idx) > 0) forecast_dates$file[idx[1]] else NA_character_
    })
  )
}