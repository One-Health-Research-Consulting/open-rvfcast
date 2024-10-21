#' Combine and Save Weather, Forecasts, and NDVI Anomalies
#'
#' This function takes in file paths of weather, forecast, and NDVI anomalies, combines them and saves as partitioned Parquets using hive partitioning by date
#'
#' @author R Programmer
#'
#' @param weather_anomalies File path of the weather anomalies.
#' @param forecasts_anomalies File path of the forecast anomalies.
#' @param ndvi_anomalies File path of the NDVI anomalies.
#' @param combined_anomolies_directory Directory where the processed files will be saved.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A list containing filepaths of the newly created .parquet files
#'
#' @note This function requires Apache Arrow library for reading datasets and writing them as parquet files.
#'
#' @examples
#' combine_anomolies('<path_to_weather_anomalies>', 
#'                   '<path_to_forecasts_anomalies>', 
#'                   '<path_to_ndvi_anomalies>', 
#'                   '<path_to_output_directory>')
#'
#' @export
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
