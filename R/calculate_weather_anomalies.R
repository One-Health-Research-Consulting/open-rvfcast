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
calculate_weather_anomalies <- 
  function( # tranformed files
    nasa_weather_transformed,
    nasa_weather_directory_transformed, # TODO rename this to nasa_weather_transformed_directory
    # historical means
    weather_historical_means,
    # directory for saving anomalies 
    weather_anomalies_directory,
    # dates and lags selected
    model_dates,
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
    weather_transformed_dataset <- open_dataset(nasa_weather_directory_transformed)
    
    # Get historical means for DOY
    doy <- model_dates |> filter(date == date_selected) |> pull(day_of_year)
    doy_frmt <- str_pad(doy,width = 3, side = "left", pad = "0")
    historical_means <- read_parquet(weather_historical_means[str_detect(weather_historical_means, doy_frmt)]) |> 
      select(-day_of_year)
    
    # Get the lagged anomalies for selected dates, mapping over the lag intervals
    row_select <- which(model_dates$date == date_selected)
    
    lag_intervals_start <- c(1 , 1+lag_intervals[-length(lag_intervals)])
    lag_intervals_end <- lag_intervals
    
    anomalies <- map2(lag_intervals_start, lag_intervals_end, function(start, end){
      lag_dates <- model_dates |> slice((row_select - start):(row_select - end))
      
      # Lag: calculate mean by pixel for the preceding x days
      lagged_means <- weather_transformed_dataset |> 
        filter(date %in% !!lag_dates$date) |> 
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
