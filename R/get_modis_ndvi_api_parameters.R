#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_modis_ndvi_api_parameters <- function(bounding_boxes, start_year, end_year) {
  
  bbox <- bounding_boxes |> filter(region == "southern") 
  bbox_coords <- as.numeric(bbox[1,-1])
  
  planetary_query <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  
  collection <- "modis-13Q1-061" # this is 250m (modis-13A1-061 is 500m)
  
  out <- map_dfr(start_year:end_year, function(year){
    ndvi_query <- planetary_query |> 
      stac_search(collections = collection,
                  limit = 1000,
                  datetime = paste0(year, "-01-01T00:00:00Z/", year, "-12-31T23:59:59Z"),
                  bbox = bbox_coords) |>
      get_request() |> 
      items_sign(sign_fn = sign_planetary_computer())
    
    tibble(url = map_vec(ndvi_query$features, ~.$assets$`250m_16_days_NDVI`$href),
           id = (map_vec(ndvi_query$features, ~.$id)))
  }) 
  
  return(out)
  
}
