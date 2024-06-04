#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wahis_rvf_outbreaks_preprocessed
#' @return
#' @author Emma Mendelsohn
#' @export
create_outbreak_layer <- function(wahis_rvf_outbreaks_preprocessed,
                                  rsa_polygon,
                                  model_dates_selected) {
  
  # Get polygons for outbreaks
  rvf_points <- wahis_rvf_outbreaks_preprocessed |> 
    distinct(country_name, outbreak_id, outbreak_start_date, latitude, longitude) |> 
    st_as_sf(coords = c("longitude", "latitude"))
  st_crs(rvf_points) <- st_crs(rsa_polygon)  # Set the CRS (Coordinate Reference System)
  
  rvf_points_polygon <- st_join(rvf_points, rsa_polygon, join = st_within) |> 
    drop_na(shapeGroup) |> 
    mutate(outbreak_start_date = ymd(outbreak_start_date))
  
  # For each model selected date, determine which polygons had an outbreak in the following 30 days
  rvf_points_polygon_dates <- map_dfr(model_dates_selected, function(model_date){
    day_diff <- rvf_points_polygon$outbreak_start_date - model_date
    rvf_points_polygon[which(day_diff >= 1 & day_diff <= 30),] |> 
      mutate(date = model_date)
  })
  
  # Return the unique combination of model select dates and districts with outbreaks
  rvf_points_polygon_dates |> 
    distinct(date, shapeName) |> 
    mutate(outbreak_30 = TRUE)
  
}
