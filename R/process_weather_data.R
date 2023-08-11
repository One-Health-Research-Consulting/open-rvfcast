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
  library(dbplyr)
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
  
  # check this worked
  test = weather_lags |> filter(x == min(x), y == max(y), day_of_year < 50) |> collect()
  
  # ^ get rolling avg for 1 month, 2 month, 3 month ahead for each var
  
  # filter out top 90 days of days (these are used to inform the lags only)
  
  # filter for model_dates_random_select
  
  
  ### testing notes
  # SQL rolling avg
    # "AVG(anomaly_precipitation) OVER (
    #  ORDER BY day_of_year
    #  ROWS BETWEEN 30 PRECEDING AND 1 PRECEDING)"
  
  # sanity checks
  # mf <- memdb_frame(x = 1:100, y = 1:100)
  # mf2 <- mf |> 
  #   window_frame(-30, -1) |> 
  #   window_order(y)  |> 
  #   mutate(x2 = mean(x))
  
}
