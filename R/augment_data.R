
#' @title
#' @param weather_anomalies
#' @param forecasts_anomalies
#' @param ndvi_anomalies
#' @param augmented_data_directory 
#' @return
#' @author Emma Mendelsohn
#' @export
augment_data <- function(weather_anomalies, forecasts_anomalies,
                                   ndvi_anomalies, augmented_data_directory) {


  weather <- arrow::open_dataset(weather_anomalies) 
  forecasts <- arrow::open_dataset(forecasts_anomalies)
  ndvi <- arrow::open_dataset(ndvi_anomalies)

  left_join(weather, forecasts) |> 
    left_join(ndvi) |> 
    group_by(date) |> 
    write_dataset(augmented_data_directory)
  
  return(list.files(augmented_data_directory))
  
}
  