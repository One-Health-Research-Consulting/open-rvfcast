#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param forecasts_validate_directory
#' @param forecasts_anomalies
#' @param nasa_weather_transformed
#' @param weather_historical_means
#' @param model_dates_selected
#' @param lead_intervals
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
validate_forecasts_anomalies <- function(forecasts_validate_directory,
                                         forecasts_anomalies,
                                         nasa_weather_transformed,
                                         weather_historical_means,
                                         model_dates_selected, lead_intervals,
                                         overwrite = FALSE) {
  
  # Set filename
  date_selected <- model_dates_selected
  save_filename <- glue::glue("forecast_validate_{date_selected}.gz.parquet")
  message(paste0("Validating forecast for ", date_selected))
  
  # Check if file already exists
  existing_files <- list.files(forecasts_validate_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(forecasts_validate_directory, save_filename))
  }
  
  # Open dataset to forecast anomalies and weather data
  forecasts_anomalies <- open_dataset(forecasts_anomalies) |> filter(date == date_selected)
  nasa_weather_transformed <- open_dataset(nasa_weather_transformed)
  
  # Calculate weather anomalies for selected forecast dates, mapping over the lead intervals
  lead_intervals_start <- c(0 , lead_intervals[-length(lead_intervals)]) # 0 to include current day in forecast
  lead_intervals_end <- lead_intervals - 1 # -1 for 30 days total include start day
  
  forecasts_validation <- map(1:length(lead_intervals_start), function(i){
    
    # subset to start and end day of interval
    start <- lead_intervals_start[i]
    end <- lead_intervals_end[i]
    
    lead_start_date <- date_selected + start
    lead_end_date <- date_selected + end 
    
    # get historical means for lead period, removing doy 366
    lead_dates <- seq(lead_start_date, lead_end_date, by = "day")
    lead_doys <- yday(lead_dates)
    
    if(366 %in% lead_doys) {
      if(tail(lead_doys, 1) == 366){
        lead_doys <- lead_doys[lead_doys!=366]
        lead_doys <- c(lead_doys, 1)
      }else{
        lead_doys <- lead_doys[lead_doys!=366]
        lead_doys <- c(lead_doys, tail(lead_doys, 1) + 1)
      }
    }
    
    doy_start <- head(lead_doys, 1)
    doy_end <- tail(lead_doys, 1)
    doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
    doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")
    doy_range <- glue::glue("{doy_start_frmt}_to_{doy_end_frmt}")
    
    historical_means <- read_parquet(weather_historical_means[str_detect(weather_historical_means, doy_range)]) 
    
    # get average for weather data over this period
    weather_means <- nasa_weather_transformed |> 
      filter(date %in% lead_dates) |> 
      group_by(x, y) |> 
      summarize(lead_relative_humidity_mean = mean(relative_humidity),
                lead_temperature_mean = mean(temperature),
                lead_precipitation_mean = mean(precipitation)) |> 
      ungroup() 
    
    # Join in historical means to calculate anomalies raw and scaled
    weather_anomalies <- full_join(weather_means, historical_means, by = c("x", "y")) |> 
      mutate(!!paste0("anomaly_relative_humidity_recorded_", end) := lead_relative_humidity_mean - historical_relative_humidity_mean,
             !!paste0("anomaly_temperature_recorded_", end) := lead_temperature_mean  -  historical_temperature_mean,
             !!paste0("anomaly_precipitation_recorded_", end) := lead_precipitation_mean - historical_precipitation_mean,
             !!paste0("anomaly_relative_humidity_scaled_recorded_", end) := (lead_relative_humidity_mean - historical_relative_humidity_mean)/historical_relative_humidity_sd,
             !!paste0("anomaly_temperature_scaled_recorded_", end) := (lead_temperature_mean  -  historical_temperature_mean)/historical_temperature_sd,
             !!paste0("anomaly_precipitation_scaled_recorded_", end) := (lead_precipitation_mean - historical_precipitation_mean)/historical_precipitation_sd) |> 
      select(-starts_with("lead"), -starts_with("historical")) 
    
    
    # Now calculate difference forecast v recorded 
   forecasts_anomalies |> 
     select(x, y, ends_with(as.character(end))) |> 
     left_join(weather_anomalies, by = c("x", "y")) |> 
     mutate(!!paste0("anomaly_relative_humidity_difference_", end) := !!sym(paste0("anomaly_relative_humidity_forecast_", end)) - !!sym(paste0("anomaly_relative_humidity_recorded_", end)),
            !!paste0("anomaly_temperature_difference_", end) := !!sym(paste0("anomaly_temperature_forecast_", end)) - !!sym(paste0("anomaly_temperature_recorded_", end)),
            !!paste0("anomaly_precipitation_difference_", end) := !!sym(paste0("anomaly_precipitation_forecast_", end)) - !!sym(paste0("anomaly_precipitation_recorded_", end)),
            !!paste0("anomaly_relative_humidity_scaled_difference_", end) := !!sym(paste0("anomaly_relative_humidity_scaled_forecast_", end)) - !!sym(paste0("anomaly_relative_humidity_scaled_recorded_", end)),
            !!paste0("anomaly_temperature_scaled_difference_", end) := !!sym(paste0("anomaly_temperature_scaled_forecast_", end)) - !!sym(paste0("anomaly_temperature_scaled_recorded_", end)),
            !!paste0("anomaly_precipitation_scaled_difference_", end) := !!sym(paste0("anomaly_precipitation_scaled_forecast_", end)) - !!sym(paste0("anomaly_precipitation_scaled_recorded_", end))) 
  }) |> 
    reduce(left_join, by = c("x", "y")) |> 
    mutate(date = date_selected) |> 
    relocate(date)
  
  # Save as parquet 
  write_parquet(forecasts_validation, here::here(forecasts_validate_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(forecasts_validate_directory, save_filename))
  
  
}
