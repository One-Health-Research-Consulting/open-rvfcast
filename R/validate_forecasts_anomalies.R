#' Validate Forecasts Anomalies
#'
#' The function 'validate_forecasts_anomalies' takes in several parameters including aggregated forecasts,
#' processed NASA weather data, historical weather means, model dates, lead intervals and a boolean for overwriting
#' existing files. It validates the anomaly forecasts based on the given parameters and saves the output to 
#' a defined directory.
#'
#' @author Emma Mendelsohn
#'
#' @param forecasts_validate_directory Directory where the validation results will be saved.
#' @param forecasts_anomalies The dataset containing the aggregated weather anomaly forecasts.
#' @param nasa_weather_transformed The processed NASA weather data.
#' @param weather_historical_means The historical means of the weather variables to be used.
#' @param model_dates_selected The dates selected for modelling.
#' @param lead_intervals Time intervals for leading the forecast.
#' @param overwrite A boolean indicating whether to overwrite existing validation files. Default is FALSE.
#' @param ... Additional arguments not used by the function.
#'
#' @return A string containing the filepath to the saved validation results.
#'
#' @note The function validates the anomaly forecast by comparing the forecasted anomalies to the recorded ones based on 
#' the provided lead intervals and updates the database with the results. If the validation file already exists, and overwrite is 
#' set to FALSE, the filepath to the existing file is returned.
#'
#' @examples
#' validate_forecasts_anomalies(forecasts_validate_directory='./forecasts/validate',
#'                              forecasts_anomalies='forecasts_anomalies.parquet',
#'                              nasa_weather_transformed='nasa_weather.parquet',
#'                              weather_historical_means='weather_historical_means.parquet',
#'                              model_dates_selected=as.Date('2007-01-01'),
#'                              lead_intervals=c(0, 7, 14, 21),
#'                              overwrite=FALSE)
#'
#' @expor
validate_forecasts_anomalies <- function(forecasts_validate_directory,
                                         forecasts_anomalies,
                                         nasa_weather_transformed,
                                         weather_historical_means,
                                         model_dates_selected, 
                                         lead_intervals,
                                         overwrite = FALSE,
                                         ...) {
  
  # Set filename
  date_selected <- model_dates_selected
  save_filename <- glue::glue("forecast_validate_{date_selected}.gz.parquet")
  message(paste0("Validating forecast for ", date_selected))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  if(!is.null(error_safe_read_parquet(file.path(forecasts_validate_directory, save_filename))) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(file.path(forecasts_validate_directory, save_filename))
  }
  
  # Open dataset to forecast anomalies and weather data
  forecasts_anomalies <- arrow::open_dataset(forecasts_anomalies) |> filter(date == date_selected)
  nasa_weather_transformed <- arrow::open_dataset(nasa_weather_transformed)
  
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
    
    historical_means <- arrow::read_parquet(weather_historical_means[str_detect(weather_historical_means, doy_range)]) 
    
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
  arrow::write_parquet(forecasts_validation, here::here(forecasts_validate_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(forecasts_validate_directory, save_filename))
  
  
}
