#' Calculate weather anomalies based on historical data
#'
#' This function calculates weather anomalies using data from NASA's servers. The anomalies are calculated for each date
#' in the input file using the weather historical means. Calculated anomalies are saved at a specified location and also returned by the function.
#'
#' @author Emma Mendelsohn
#'
#' @param nasa_weather_transformed A single month's transformed NASA weather parquet file.
#' @param weather_historical_means The historical weather averages used to calculate the weather anomalies.
#' @param weather_anomalies_directory The directory where the calculated weather anomalies will be stored.
#' @param basename_template Template for output filenames. Default is 'weather_anomaly_{date}.parquet'.
#' @param dates_to_process Not used anymore, kept for compatibility.
#' @param overwrite A flag indicating whether existing anomaly files should be overwritten. Defaults to FALSE.
#' @param ... Additional arguments not used by this function but included for generic method compatibility.
#'
#' @return A character vector containing filepaths to all files created for dates in this month.
#'
#' @note This function processes all dates within the month contained in nasa_weather_transformed.
#' It creates one output file per date.
#'
#' @examples
#' calculate_weather_anomalies(
#'   nasa_weather_transformed = "data/nasa/nasa_weather_transformed_2020-01.parquet",
#'   weather_historical_means = "./data/historical_means",
#'   weather_anomalies_directory = "./data/anomalies",
#'   overwrite = TRUE
#' )
#'
#' @export
calculate_weather_anomalies <- function(nasa_weather_transformed,
                                        weather_historical_means,
                                        weather_anomalies_directory,
                                        basename_template = "weather_anomaly_{date}.parquet",
                                        overwrite = FALSE,
                                        ...) {
  # Check if input file exists
  if (length(nasa_weather_transformed) == 0 || is.na(nasa_weather_transformed)) {
    stop("nasa_weather_transformed is empty or NA - no weather file available for this month")
  }

  if (!file.exists(nasa_weather_transformed)) {
    stop(glue::glue("NASA weather file does not exist: {nasa_weather_transformed}"))
  }

  # Read the weather data for this month
  weather_data <- arrow::read_parquet(nasa_weather_transformed)

  # Get all unique dates in the parquet file
  dates_in_month <- unique(weather_data$date)

  if (length(dates_in_month) == 0) {
    message("No dates found in month file")
    return(character(0))
  }

  message(paste0("Processing ", length(dates_in_month), " dates in month"))

  # Process each date and collect output filenames
  output_files <- purrr::map_chr(dates_in_month, function(date) {

    # Set filename for this date
    save_filename <- file.path(weather_anomalies_directory, glue::glue(basename_template))

    # Check if file already exists and can be read
    error_safe_read_parquet <- purrr::possibly(arrow::read_parquet, NULL)
    existing_dataset <- error_safe_read_parquet(save_filename)

    if(!is.null(existing_dataset) & !overwrite) {
      # Check if file has data - if zero rows, overwrite anyway
      if(nrow(existing_dataset) > 0) {
        message(glue::glue("{basename(save_filename)} already exists, has rows, and overwrite is not TRUE, skipping"))
        return(save_filename)
      } else {
        message(glue::glue("{basename(save_filename)} exists but has zero rows, overwriting"))
      }
    } else if(!is.null(existing_dataset) & overwrite) {
      # File exists and overwrite is TRUE
      message(glue::glue("Overwriting existing {basename(save_filename)} for ", date))
    } else {
      # File doesn't exist - we're calculating
      message(paste0("Calculating weather anomalies for ", date))
    }

    # Filter to this specific date and round coordinates to guard against
    # floating-point drift (~1e-15) when continent_raster_template is recomputed
    weather_transformed_dataset <- weather_data |>
      dplyr::filter(date == !!date) |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    doy_to_process <- as.numeric(lubridate::yday(date))
    
    ## Open dataset to historical weather data
    historical_means <- arrow::open_dataset(weather_historical_means) |>
      dplyr::filter(doy == doy_to_process) |>
      dplyr::collect() |>
      tidyr::drop_na() |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    # Join the two datasets by day of year (doy)
    weather_transformed_dataset <- dplyr::left_join(weather_transformed_dataset, historical_means,
                                             by = c("x", "y", "doy"),
                                             suffix = c("", "_historical")) |>
      tidyr::drop_na()

    # Calculate temperature anomalies
    weather_transformed_dataset <- weather_transformed_dataset |>
      dplyr::mutate(
        anomaly_temperature = temperature - temperature_historical,
        anomaly_scaled_temperature = anomaly_temperature / temperature_sd
      )

    # Calculate precipitation anomalies
    weather_transformed_dataset <- weather_transformed_dataset |>
      dplyr::mutate(
        anomaly_precipitation = precipitation - precipitation_historical,
        anomaly_scaled_precipitation = dplyr::if_else(precipitation_sd == 0, 0, anomaly_precipitation / precipitation_sd)
      )

    # Calculate relative_humidity anomalies
    weather_transformed_dataset <- weather_transformed_dataset |>
      dplyr::mutate(
        anomaly_relative_humidity = relative_humidity - relative_humidity_historical,
        anomaly_scaled_relative_humidity = anomaly_relative_humidity / relative_humidity_sd
      )

    # Remove intermediate columns
    weather_transformed_dataset <- weather_transformed_dataset |>
      dplyr::mutate(
        doy = as.integer(lubridate::yday(date)), # Calculate day of year
        month = as.integer(lubridate::month(date)), # Extract month
        year = as.integer(lubridate::year(date))
      ) |> # Extract year
      dplyr::select(x, y, date, doy, month, year, dplyr::starts_with("anomaly"))

    # Save as parquet
    arrow::write_parquet(weather_transformed_dataset, save_filename,
                        compression = "gzip", compression_level = 5)

    return(save_filename)
  })

  return(output_files)
}

