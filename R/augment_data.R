#' Augment Weather Data
#'
#' This function collects data from three different sources, checks for missing values,
#' combines them into a single dataset, and saves the augmented data as a partitioned dataset
#' in parquet format to a specified directory.
#'
#' @author Emma Mendelsohn
#'
#' @param weather_anomalies File path to the weather anomalies dataset.
#' @param forecasts_anomalies File path to the forecasts anomalies dataset.
#' @param ndvi_anomalies File path to the NDVI anomalies dataset.
#' @param augmented_data_directory Directory where the augmented data will be saved in parquet format.
#'
#' @return A string containing the file path to the directory where the augmented data is saved.
#'
#' @note This function performs a left join of the three datasets on the date, x, and y variables. 
#' Any NA values in the 'date', 'x', and 'y' columns of the dataset will be dropped. The function 
#' saves the resulting dataset in the specified directory using hive partitioning by date.
#'
#' @examples
#' augment_data(weather_anomalies = 'path/to/weather_data',
#'              forecasts_anomalies = 'path/to/forecast_data',
#'              ndvi_anomalies = 'path/to/ndvi_data',
#'              augmented_data_directory = 'path/to/save/augmented_data')
#'
#' @export
augment_data <- function(weather_anomalies, 
                         forecasts_anomalies,
                         ndvi_anomalies, 
                         augmented_data_directory,
                         overwrite = FALSE,
                         ...) {

  # Figure out how to do all this OUT of memory.
  message("Loading datasets into memory")
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
  
  return(augmented_data_directory)
  
}
