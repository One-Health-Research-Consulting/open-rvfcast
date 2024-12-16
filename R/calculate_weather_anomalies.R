#' Calculate weather anomalies based on historical data
#'
#' This function calculates weather anomalies using data from NASA's servers. The anomalies are calculated for each specified date
#' using the weather historical means. Calculated anomalies are saved at a specified location and also returned by the function.
#'
#' @author Emma Mendelsohn
#'
#' @param nasa_weather_transformed_directory The directory where the transformed NASA weather data is located.
#' @param weather_historical_means The historical weather averages used to calculate the weather anomalies.
#' @param weather_anomalies_directory The directory where the calculated weather anomalies will be stored.
#' @param model_dates_selected The dates for which the weather anomalies will be calculated.
#' @param lag_intervals The intervals used to calculate the lags in the weather data.
#' @param overwrite A flag indicating whether existing anomaly files should be overwritten. Defaults to FALSE.
#' @param ... Additional arguments not used by this function but included for generic method compatibility.
#'
#' @return A string containing the filepath to the file containing the calculated weather anomalies.
#'
#' @note This function calculates weather anomalies using NASA data and historical means. If a file containing anomalies for a
#' specified date already exists and the overwrite flag is set to TRUE, the existing file will be overwritten.
#' Otherwise, the existing file will be returned.
#'
#' @examples
#' calculate_weather_anomalies(nasa_weather_transformed_directory = './data/nasa',
#'                             weather_historical_means = './data/historical_means',
#'                             weather_anomalies_directory = './data/anomalies',
#'                             model_dates_selected = as.Date('2020-01-01'),
#'                             lag_intervals = c(1, 3, 7),
#'                             overwrite = TRUE)
#'
#' @export
calculate_weather_anomalies <- function(nasa_weather_transformed,
                                        weather_historical_means,
                                        weather_anomalies_directory,
                                        model_dates_selected,
                                        overwrite = FALSE,
                                        ...) {
  
  # Check that we're only working on one date at a time
  stopifnot(length(model_dates_selected) == 1)
  
  # Set filename
  save_filename <- file.path(weather_anomalies_directory, glue::glue("weather_anomaly_{model_dates_selected}.gz.parquet"))
  message(paste0("Calculating weather anomalies for ", model_dates_selected))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(save_filename)
  }
  
  # Open dataset to transformed data
  weather_transformed_dataset <- arrow::open_dataset(nasa_weather_transformed) |> 
    filter(date == lubridate::as_date(model_dates_selected)) |> collect()
  
  # Open dataset to historical weather data
  historical_means <- arrow::open_dataset(weather_historical_means) |> filter(doy == lubridate::yday(model_dates_selected)) |> collect()
  
  # Join the two datasets by day of year (doy)
  weather_transformed_dataset <- left_join(weather_transformed_dataset, historical_means, by = c("x","y","doy"), suffix = c("", "_historical"))
  
  # Calculate temperature anomalies
  weather_transformed_dataset <- weather_transformed_dataset |>
    mutate(anomaly_temperature = temperature - temperature_historical,
           anomaly_scaled_temperature = anomaly_temperature / temperature_sd)
  
  # Calculate precipitation anomalies
  weather_transformed_dataset <- weather_transformed_dataset |>
    mutate(anomaly_precipitation = precipitation - precipitation_historical,
           anomaly_scaled_precipitation = anomaly_precipitation / precipitation_sd)
  
  # Calculate relative_humidity anomalies
  weather_transformed_dataset <- weather_transformed_dataset |>
    mutate(anomaly_relative_humidity = relative_humidity - relative_humidity_historical,
           anomaly_scaled_relative_humidity = anomaly_relative_humidity / relative_humidity_sd)
  
  # Remove intermediate columns
  weather_transformed_dataset <- weather_transformed_dataset |> 
    mutate(doy = as.integer(lubridate::yday(date)),          # Calculate day of year
           month = as.integer(lubridate::month(date)),       # Extract month
           year = as.integer(lubridate::year(date))) |>      # Extract year
    select(x, y, date, doy, month, year, starts_with("anomaly"))
  
  # Save as parquet 
  arrow::write_parquet(weather_transformed_dataset, save_filename, compression = "gzip", compression_level = 5)
  
  return(save_filename)
}
