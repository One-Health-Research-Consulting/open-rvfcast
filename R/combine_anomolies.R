combine_anomolies <- function(weather_anomalies,
                              forecasts_anomalies,
                              ndvi_anomalies,
                              combined_anomolies_directory,
                              ...) {
  
  weather <- arrow::open_dataset(weather_anomalies)  
  forecasts <- arrow::open_dataset(forecasts_anomalies)  
  ndvi <- arrow::open_dataset(ndvi_anomalies) 
  
  ds <- arrow::open_dataset(c(weather_anomalies, forecasts_anomalies, ndvi_anomalies))
  
  message("Save as parquets using hive partitioning by date")
  ds |> 
    group_by(date) |> 
    arrow::write_dataset(combined_anomolies_directory, compression = "gzip", compression_level = 5)
  
  return(list.files(combined_anomolies_directory, pattern = ".parquet", recursive = TRUE, full.names = TRUE))
}