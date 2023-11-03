#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sentinel_ndvi_transformed
#' @param nasa_weather_transformed
#' @return
#' @author Emma Mendelsohn
#' @export
calculate_weather_anomalies <- function(nasa_weather_transformed,
                                        nasa_weather_transformed_directory,
                                        weather_historical_means,
                                        weather_anomalies_directory,
                                        model_dates_selected,
                                        lag_intervals,
                                        overwrite = FALSE) {
  
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
  lag_intervals_start <- c(1 , 1+lag_intervals[-length(lag_intervals)])
  lag_intervals_end <- lag_intervals
  
  anomalies <- map2(lag_intervals_start, lag_intervals_end, function(start, end){
    
    # get lag dates
    lag_dates <- seq(date_selected - end, date_selected - start, by = "day")
    
    # Get historical means for DOY
    doy_start <- yday(lag_dates[1])
    doy_end <- yday(lag_dates[length(lag_dates)])
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
