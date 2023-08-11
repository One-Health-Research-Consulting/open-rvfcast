#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sentinel_ndvi_directory_dataset
#' @param sentinel_ndvi_transformed
#' @return
#' @author Emma Mendelsohn
#' @export
process_ndvi_data <- function(sentinel_ndvi_directory_dataset,
                              sentinel_ndvi_transformed) {

  # connect to transformed data
  sentinel_conn <- open_dataset(sentinel_ndvi_directory_dataset) 
  # or keep on aws: s3_bucket(nasa_weather_directory_dataset)
  
  # expand days from start and end dates
  
  # calculate monthly averages by pixel
  
  # calculate anomalies for each day relative to monthly average

  # randomly select two days per month and get 30, 60, 90 day lags
  
  # collect
  return(NULL)
  

}
