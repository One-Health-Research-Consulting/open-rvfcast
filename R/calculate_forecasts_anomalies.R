#' Calculate and Save Anomalies from Forecast Data
#'
#' This function takes transformed ECMWF forecast and historical weather mean data
#' and uses it to forecast weather anomolies for the remaineder of the current month,
#' and any months out present in the forecast. It then saves the forecast anomolies
#' in a specified directory.
#'
#' @author Emma Mendelsohn and Nathan C. Layman
#'
#' @param forecasts_anomalies_sources A single-row tibble with 'date' and 'forecast_file' columns.
#' @param weather_historical_means Filepath to the historical weather means data.
#' @param land_pixel_reference Filepath to any static land-only predictor parquet (e.g. elevation_preprocessed); its x/y rows define the valid analysis pixels.
#' @param forecast_anomalies_directory Directory in which to save the anomalies data.
#' @param forecast_intervals Lead times for forecasts, which will determine the interval over which anomalies are averaged.
#' @param overwrite Boolean flag indicating whether existing file should be overwritten. Default is FALSE.
#' @param ... Additional unused arguments for future extensibility and function compatibility.
#'
#' @return A string containing the filepath to the anomalies data.
#'
#' @note The returned path either points to an existing file (when overwrite is FALSE and the file already exists)
#' or to a newly created file with calculated anomalies (when overwrite is TRUE or the file didn't exist).
#'
#' @examples
#' calculate_forecast_anomalies(
#'   forecasts_anomalies_sources = tibble(
#'     date = as.Date("2000-01-01"),
#'     forecast_file = "./forecasts/ecmwf_seasonal_forecast_2000_01.parquet"
#'   ),
#'   weather_historical_means = "./historical_means.parquet",
#'   forecast_anomalies_directory = "./anomalies",
#'   forecast_intervals = c(0, 30, 60, 90, 120, 150),
#'   overwrite = TRUE
#' )
#'
#' @export
calculate_forecast_anomalies <- function(forecasts_anomalies_sources,
                                         weather_historical_means,
                                         land_pixel_reference,
                                         forecast_anomalies_directory,
                                         basename_template = "forecast_anomaly_{date}.parquet",
                                         forecast_intervals,
                                         overwrite = FALSE,
                                         ...) {
  # Extract date and forecast file from the single-row tibble
  stopifnot(nrow(forecasts_anomalies_sources) == 1)
  date <- forecasts_anomalies_sources$date
  ecmwf_forecast_transformed <- forecasts_anomalies_sources$forecast_file

  # If no forecast file available for this date, return NA
  if (is.na(ecmwf_forecast_transformed)) {
    message(glue::glue("No forecast file available for {date}, skipping"))
    return(NA_character_)
  }

  # Set filename
  save_filename <- file.path(forecast_anomalies_directory, glue::glue(basename_template))

  # Check if file already exists and can be read
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  existing_dataset <- error_safe_read_parquet(save_filename)

  if (!is.null(existing_dataset) && !overwrite) {
    # Check if file has data - if zero rows, overwrite anyway
    row_count <- existing_dataset |> count() |> collect() |> pull(n)
    if (row_count > 0) {
      message(glue::glue("{basename(save_filename)} already exists, has rows, and overwrite is not TRUE, skipping"))
      return(save_filename)
    } else {
      message(glue::glue("{basename(save_filename)} exists but has zero rows, overwriting"))
    }
  } else if (!is.null(existing_dataset) && overwrite) {
    # File exists and overwrite is TRUE
    message(glue::glue("Overwriting existing {basename(save_filename)} for ", date))
  } else {
    # File doesn't exist - we're calculating
    message(paste0("Calculating forecast anomalies for ", date))
  }

  # Notes:

  # 'Anomaly' is the scaled difference between the forecast mean and the historical mean.
  # Values close to 0 mean the forecast temperature is near the historical mean.
  # Values can be negative or positive. Weighting is used because the first day
  # of the forecast will often fall part way through a month and so will the
  # last day. In example to estimate the 30 day forecast anomaly starting on
  # 3/20/2020 and ending on 30 days later on 4/18/2020 would have 12 days
  # (including the first day, the 20th) in March 17 days (not including the
  # last day) in April. The 30 day forecast should account for the relative
  # contributions of March and April.

  # An easier way to do this is to just make a list of every day from start to
  # start + 30 - 1. Figure out the year and month and join to forecast month from
  # ecmwf_forecast_transformed. That way we could do all the things at once.
  # Then group by x, y, and summarize average of data columns. Map over each
  # lead interval and done. A benefit of this approach is that it makes
  # comparing to historical means easy. Just find the historical means for each
  # day then left join that in by month as well.

  # Updated notes after discussion with Noam
  # We want current date, current ndvi, forecast amount in days, forecast weather, forecast outbreak history (check this), and all the static layers. So we don't want wide weather forecast but long.

  # Get the relevant forecast data. Find the most recent base_date that came
  # before the model_date selected.
  forecast_transformed_dataset <- arrow::open_dataset(ecmwf_forecast_transformed) |>
    dplyr::filter(base_date <= !!date)

  relevant_base_date <- forecast_transformed_dataset |>
    select(base_date) |>
    distinct() |>
    arrange(desc(base_date)) |>
    pull(base_date, as_vector = TRUE) |>
    head(n = 1)

  # Read valid analysis pixel coordinates directly from a static land-only predictor.
  # Round x/y to 7 decimal places so that cells processed with different versions
  # of continent_raster_template (floating-point drift of ~1e-15) compare as equal.
  valid_pixels <- arrow::read_parquet(land_pixel_reference, col_select = c("x", "y")) |>
    dplyr::distinct(x, y) |>
    dplyr::mutate(x = round(x, 7), y = round(y, 7))

  forecast_transformed_dataset <- forecast_transformed_dataset |>
    dplyr::filter(base_date == relevant_base_date) |>
    select(-base_date, -month, -year) |>
    mutate(month = lead_month, year = lead_year) |>
    collect() |>
    dplyr::mutate(x = round(x, 7), y = round(y, 7)) |>
    dplyr::semi_join(valid_pixels, by = c("x", "y"))

  forecast_anomalies <- map_dfr(1:(length(forecast_intervals) - 1), function(i) {
    lead_interval_start <- forecast_intervals[i]
    lead_interval_end <- forecast_intervals[i + 1]

    message(glue::glue("Calculating ECMWF anomalies on {date} for {lead_interval_start}-{lead_interval_end} day forecast"))

    # Get a tibble of all the dates in the anomaly forecast range for the given lead interval
    forecast_anomaly <- tibble(date = seq(
      from = date + lead_interval_start,
      to = date + lead_interval_end - 1, by = 1
    )) |>
      mutate(
        doy = as.numeric(lubridate::yday(date)), # Calculate day of year
        month = as.integer(lubridate::month(date)), # Extract month
        year = as.integer(lubridate::year(date)),
        lead_interval_start = lead_interval_start, # Store lead interval duration
        lead_interval_end = lead_interval_end
      ) # Extract year

    # Historical_means is 1.3Gb we need to pre-filter only to relevant doys
    # first before the join.
    # CHECK
    historical_means <- arrow::open_dataset(weather_historical_means) |>
      dplyr::filter(doy %in% forecast_anomaly$doy) |>
      dplyr::collect() |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7)) |>
      dplyr::semi_join(valid_pixels, by = c("x", "y"))

    # Join historical means based on day of year (doy)
    # CHECK
    historical_means <- historical_means |>
      right_join(forecast_anomaly, by = "doy") |>
      relocate(-matches("precipitation|temperature|humidity"))

    # 1. forecast_transformed_dataset

    # Join in forecast data based on x, y, month, and year.
    # The historical data and forecast data _should_ have the same column
    # names so differentiate with a suffix
    historical_means <- historical_means |>
      dplyr::left_join(forecast_transformed_dataset,
        by = c("x", "y", "month", "year"),
        suffix = c("_historical", "_forecast")
      )

    # Summarize by calculating the mean for each variable type (temperature, precipitation, relative_humidity)
    # and across both historical data and forecast data over the days in the model_dates range
    historical_means <- historical_means |>
      group_by(x, y, lead_interval_start, lead_interval_end) |>
      summarize(across(matches("temperature|precipitation|relative_humidity"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

    # Adjust the near-0 sd to a non-zero value to fix the insane scaled values
    historical_means <- historical_means |>
        mutate(
          temperature_sd_historical = ifelse(temperature_sd_historical < 0.1, 0.1, temperature_sd_historical),
          precipitation_sd_historical = ifelse(precipitation_sd_historical < 0.1, 0.1, precipitation_sd_historical),
          relative_humidity_sd_historical = ifelse(relative_humidity_sd_historical < 0.1, 0.1, relative_humidity_sd_historical),
        )

    historical_means <- historical_means |>
      mutate(
        # Calculate temperature anomalies
        anomaly_forecast_temperature = temperature_forecast - temperature_historical,
        anomaly_forecast_scaled_temperature = anomaly_forecast_temperature / temperature_sd_historical,
        # Calculate precipitation anomalies
        anomaly_forecast_precipitation = precipitation_forecast - precipitation_historical,
        anomaly_forecast_scaled_precipitation = anomaly_forecast_precipitation / precipitation_sd_historical,
        # Calculate relative_humidity anomalies
        anomaly_forecast_relative_humidity = relative_humidity_forecast - relative_humidity_historical,
        anomaly_forecast_scaled_relative_humidity = anomaly_forecast_relative_humidity / relative_humidity_sd_historical
      )

    # Clean up intermediate columns
    # Regenerate month and year
    historical_means <- historical_means |>
      mutate(
        date = !!date,
        doy = lubridate::yday(date),
        month = lubridate::month(date),
        year = lubridate::year(date),
        forecast_interval = lead_interval_end
      ) |>
      select(x, y, date, doy, month, year, forecast_interval, starts_with("anomaly")) |>
      collect()
  })

  # Write output to a parquet file
  arrow::write_parquet(forecast_anomalies, save_filename, compression = "gzip", compression_level = 5)

  rm(forecast_anomalies)

  save_filename
}
