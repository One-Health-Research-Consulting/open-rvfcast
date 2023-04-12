#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_modis_ndvi_api_parameters <- function(modis_country_bounding_boxes_years) {
  
  bbox_coords <- unlist(modis_country_bounding_boxes_years$bounding_box)
  country_name <- modis_country_bounding_boxes_years$country
  year <- modis_country_bounding_boxes_years$year
  
  planetary_query <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  
  collection <- "modis-13Q1-061" # this is 250m (modis-13A1-061 is 500m)
  
    ndvi_query <- planetary_query |> 
      stac_search(collections = collection,
                  limit = 1000,
                  datetime = paste0(year, "-01-01T00:00:00Z/", year, "-12-31T23:59:59Z"),
                  bbox = bbox_coords) |>
      get_request() |> 
      items_sign(sign_fn = sign_planetary_computer())
    
    out <- tibble(country_name = country_name,
           year = year,
           url = map_vec(ndvi_query$features, ~.$assets$`250m_16_days_NDVI`$href),
           id = (map_vec(ndvi_query$features, ~.$id)))
  
  

}
