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
  

  message("Load datasets into memory")
  weather <- arrow::open_dataset(weather_anomalies) |> dplyr::collect() 
  forecasts <- arrow::open_dataset(forecasts_anomalies) |> dplyr::collect() 
  ndvi <- arrow::open_dataset(ndvi_anomalies) |> dplyr::collect()
  
  message("NA checks")
  ## Weather and forecasts
  ### NAs are in scaled precip data, due to days with 0 precip
  weather_check <- purrr::map_lgl(weather, ~any(is.na(.)))
  assertthat::assert_that(all(str_detect(names(weather_check[weather_check]), "scaled"))) 
  
  forecasts_check <- purrr::map_lgl(forecasts, ~any(is.na(.)))
  assertthat::assert_that(all(str_detect(names(forecasts_check[forecasts_check]), "scaled"))) 
  
  ## NDVI
  ### Prior to 2018: NAs are due to region missing from Eastern Africa in modis data
  ### After 2018: NAs are due to smaller pockets of missing data on a per-cycle basis
  ### okay to remove when developing RSA model (issue #72)
  ndvi_check <- purrr::map_lgl(ndvi, ~any(is.na(.)))
  assertthat::assert_that(!any(ndvi_check[c("date", "x", "y")]))
  ndvi <- drop_na(ndvi)

  message("Join into a single object")
  augmented_data <- left_join(weather, forecasts, by = join_by(date, x, y)) |> 
    left_join(ndvi, by = join_by(date, x, y))
  
  message("Save as parquets using hive partitioning by date")
  augmented_data |> 
    group_by(date) |> 
    write_dataset(augmented_data_directory)
  
  return(list.files(augmented_data_directory))
  
}