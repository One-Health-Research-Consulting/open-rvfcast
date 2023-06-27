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
process_weather_data <- function(nasa_weather_directory_dataset, nasa_weather_dataset) {
  
  weather_dataset <- open_dataset(nasa_weather_directory_dataset) |> to_duckdb(table_name = "weather")
  
  # calculate monthly averages by pixel
  # calculate anomalies for each day relative to monthly average
  weather_anomalies <- weather_dataset |> 
    group_by(x, y, month) |> 
    mutate(mean_relative_humidity = mean(relative_humidity),
           mean_temperature = mean(temperature),
           mean_precipitation = mean(precipitation)) |> 
    ungroup()  |> 
    mutate(anomaly_relative_humidity = relative_humidity - mean_relative_humidity,
           anomaly_temperature = temperature - mean_temperature,
           anomaly_precipitation = precipitation - mean_precipitation)  
  
  # get rolling avg for each x,y
  # throws memory error - PRAGMA temp_directory='/path/to/tmp.tmp'
  weather_lags <- weather_anomalies |> 
    group_by(x, y) |> 
    window_frame(-30, -1) |> 
    window_order(day_of_year) |> 
    mutate(anomaly_relative_humidity_roll30 = mean(anomaly_relative_humidity))  |> 
    ungroup()
  
    "AVG(anomaly_precipitation) OVER (
     ORDER BY day_of_year
     ROWS BETWEEN 30 PRECEDING AND 1 PRECEDING)"
  
}
