#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_modis_ndvi_api_parameters <- function(bounding_boxes) {
  
  bounding_boxes <- bounding_boxes |> filter(region == "africa")

  planetary_query <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  
  collection <- "modis-13Q1-061" # this is 250m (modis-13A1-061 is 500m)
  
  # do this by year
  
  ndvi_query <- planetary_query |> 
    stac_search(collections = collection,
                limit = 1000,
                datetime = "2005-01-01T00:00:00Z/..",
                bbox = as.numeric(bounding_boxes[1,-1])) |>
    get_request() |> 
    items_sign(sign_fn = sign_planetary_computer())
  # max 250 returned
  
  map_vec(ndvi_query$features, ~.$properties$start_datetime) |> unique()
  map_vec(ndvi_query$features, ~.$properties$end_datetime) |> unique()
  # returning 52 objects per date - what is the diff?
  
  map_vec(ndvi_query$features, ~.$properties$end_datetime) |> table()
  
  
  map(ndvi_query$features, ~.$bbox) 
  ndvi_query$features[[2]]$bbox
  ndvi_query$features[[54]]$bbox
  
  
}
