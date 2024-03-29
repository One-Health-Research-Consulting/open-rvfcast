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
  
  # Read augmented data, convert to raster
  r <- arrow::read_parquet(glue::glue("{augmented_data}/date={model_dates_selected}/part-0.parquet")) |> 
    rast() 
  crs(r) <- crs(rast())
  
  # Mask raster to polygon
  r <- mask(r, rsa_polygon) 
  
  # Get the mean value by polygons
  p <- terra::extract(r, rsa_polygon, mean, na.rm = TRUE, weights = FALSE, ID = FALSE) 
  
  
  # spatially weight in fitting - probability per unit area - per size of the district
  # probability of it happening in the district is the spatial weighting
  
  bind_cols(rsa_polygon, p) |> 
    as_tibble() |> 
    mutate(date = model_dates_selected) |> 
    select(date, shapeName, names(p))
  
}
