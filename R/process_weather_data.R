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
process_weather_data <- function(nasa_weather_directory_dataset, nasa_weather_transformed) {
  
  # connect to transformed data
  weather_conn <- open_dataset(nasa_weather_directory_dataset) 
  # or keep on aws: s3_bucket(nasa_weather_directory_dataset)
  
  # calculate monthly averages by pixel
  weather_means <- weather_conn |> 
    group_by(x, y, mm) |> 
    summarize(across(c("relative_humidity", "temperature", "precipitation"), mean)) |> 
    ungroup() |> 
    rename_with(~ paste0("mean_", .), -c(x, y, mm)) |> 
    collect()
  
  # calculate anomalies for each day relative to monthly average
  weather_anomalies <- weather_conn |> 
    left_join(weather_means, by = c("x", "y", "mm")) |> 
    mutate(anomaly_relative_humidity = relative_humidity - mean_relative_humidity,
           anomaly_temperature = temperature - mean_temperature,
           anomaly_precipitation = precipitation - mean_precipitation)
  
  # randomly select two days per month and get 30, 60, 90 day lags
  
  # collect
  return(NULL)
  
}
