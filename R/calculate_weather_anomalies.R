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
calculate_weather_anomalies <- function(nasa_weather_transformed_directory,
                                        weather_historical_means,
                                        weather_anomalies_directory,
                                        model_dates_selected,
                                        lag_intervals,
                                        overwrite = FALSE,
                                        ...) {
  
  # Set filename
  date_selected <- model_dates_selected
  save_filename <- glue::glue("weather_anomaly_{date_selected}.gz.parquet")
  message(paste0("Calculating weather anomalies for ", date_selected))
  
  # Check if file already exists
  existing_files <- list.files(weather_anomalies_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(weather_anomalies_directory, save_filename))
  }
  
  # Open dataset to transformed data
  weather_transformed_dataset <- open_dataset(nasa_weather_transformed_directory)
  
  # Get the lagged anomalies for selected dates, mapping over the lag intervals
  lag_intervals_start <- c(1 , 1+lag_intervals[-length(lag_intervals)]) # 1 to start with previous day
  lag_intervals_end <- lag_intervals # 30 days total including end day
  
  anomalies <- map2(lag_intervals_start, lag_intervals_end, function(start, end){
    
    # get lag dates, removing doy 366
    lag_dates <- seq(date_selected - end, date_selected - start, by = "day")
    lag_doys <- yday(lag_dates)
    if(366 %in% lag_doys){
      lag_doys <- lag_doys[lag_doys!=366]
      lag_doys <- c(head(lag_doys, 1) - 1, lag_doys)
    }
    
    # Get historical means for lag period
    doy_start <- head(lag_doys, 1)
    doy_end <- tail(lag_doys, 1)
    doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
    doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")
    doy_range <- glue::glue("{doy_start_frmt}_to_{doy_end_frmt}")
    
    historical_means <- read_parquet(weather_historical_means[str_detect(weather_historical_means, doy_range)]) 
    assertthat::assert_that(nrow(historical_means) > 0)
    
    # Lag: calculate mean by pixel for the lag days
    lagged_means <- weather_transformed_dataset |> 
      filter(date %in% lag_dates) |> 
      group_by(x, y) |> 
      summarize(lag_relative_humidity_mean = mean(relative_humidity),
                lag_temperature_mean = mean(temperature),
                lag_precipitation_mean = mean(precipitation)) |> 
      ungroup() 
    
    # Join in historical means to calculate anomalies raw and scaled
    full_join(lagged_means, historical_means, by = c("x", "y")) |> 
      mutate(!!paste0("anomaly_relative_humidity_", end) := lag_relative_humidity_mean - historical_relative_humidity_mean,
             !!paste0("anomaly_temperature_", end) := lag_temperature_mean  -  historical_temperature_mean,
             !!paste0("anomaly_precipitation_", end) := lag_precipitation_mean - historical_precipitation_mean,
             !!paste0("anomaly_relative_humidity_scaled_", end) := (lag_relative_humidity_mean - historical_relative_humidity_mean)/historical_relative_humidity_sd,
             !!paste0("anomaly_temperature_scaled_", end) := (lag_temperature_mean  -  historical_temperature_mean)/historical_temperature_sd,
             !!paste0("anomaly_precipitation_scaled_", end) := (lag_precipitation_mean - historical_precipitation_mean)/historical_precipitation_sd) |> 
      select(-starts_with("lag"), -starts_with("historical"))
  }) |> 
    reduce(left_join, by = c("x", "y")) |> 
    mutate(date = date_selected) |> 
    relocate(date)
  
  # Save as parquet 
  write_parquet(anomalies, here::here(weather_anomalies_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(weather_anomalies_directory, save_filename))
  
  
}
