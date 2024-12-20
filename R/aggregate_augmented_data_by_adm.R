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
aggregate_augmented_data_by_adm <- function(augmented_data, 
                                            rsa_polygon, 
                                            model_dates_selected) {
  
  # #
  # points_with_municipalities <- st_join(africa_points, rsa_polygon)
  # 
  
  # r <- arrow::read_parquet(glue::glue("{augmented_data}/date={model_dates_selected}/part-0.parquet")) |> 
  #   rast() 
  # crs(r) <- crs(rast())
  # r <- mask(r, rsa_polygon) 
  # 
  # p <- terra::extract(r, rsa_polygon, mean, na.rm = TRUE, weights = TRUE)
  # 
  # bind_cols(rsa_polygon, p) |> 
  #   as_tibble() |> 
  #   mutate(date = model_dates_selected) |> 
  #   select(date, shapeName, ID, names(p))

}
