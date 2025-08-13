#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param augmented_data
#' @param rsa_polygon
#' @return
#' @author Emma Mendelsohn
#' @export
aggregate_augmented_data_by_adm <- function(parquet_file_list, 
                                            sf_df, 
                                            predictor_aggregating_functions,
                                            dates_to_process) {
  
  
  arrow_db <- arrow::open_dataset(parquet_file_list)
  
  # Check that the setdiff between variable names and predictor_aggregating_functions is zero
  # if not we need to update the predictor_aggregating_function csv file
  if(length(setdiff(predictor_aggregating_functions$var, arrow_db$schema$names)) != 0) {
    stop("predictor_summary.csv does not match the columns in the provided data. Harmonize before preceeding.")
  }  
  
  
  # #
  # points_with_municipalities <- st_join(africa_points, rsa_polygon)
  # 
  
  # r <- arrow::read_parquet(glue::glue("{augmented_data}/date={dates_to_process}/part-0.parquet")) |> 
  #   rast() 
  # crs(r) <- crs(rast())
  # r <- mask(r, rsa_polygon) 
  # 
  # p <- terra::extract(r, rsa_polygon, mean, na.rm = TRUE, weights = TRUE)
  # 
  # bind_cols(rsa_polygon, p) |> 
  #   as_tibble() |> 
  #   mutate(date = dates_to_process) |> 
  #   select(date, shapeName, ID, names(p))

}
