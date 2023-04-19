#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_modis_ndvi_api_parameters <- function(continent_bounding_box,
                                          modis_ndvi_years) {

    bbox_coords <- continent_bounding_box
    year <- modis_ndvi_years
    
    # Connect to API 
    planetary_query <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
    
    # This is the NDVI collection
    collection <- "modis-13A1-061" # this is 500m (modis-13Q1-061 is 250m)
    
    # Run query for year/country
    date_ranges <- c(paste0(year, "-01-01T00:00:00Z/", year, "-03-31T23:59:59Z"),
                     paste0(year, "-04-01T00:00:00Z/", year, "-06-30T23:59:59Z"),
                     paste0(year, "-07-01T00:00:00Z/", year, "-09-30T23:59:59Z"),
                     paste0(year, "-10-01T00:00:00Z/", year, "-12-31T23:59:59Z"))
    
    ndvi_query <-  map(date_ranges, function(date_range){
      q <- planetary_query |> 
        stac_search(collections = collection,
                    limit = 1000,
                    datetime = date_range,
                    bbox = bbox_coords) |>
        get_request() |> 
        items_sign(sign_fn = sign_planetary_computer())
      assertthat::assert_that(length(q$features) < 1000)
      return(q$features)
    }) |> reduce(c)
    
    ndvi_params <- tibble(
      url = map_vec(ndvi_query, ~.$assets$`500m_16_days_NDVI`$href),
      id = map_vec(ndvi_query, ~.$id),
      start_date = map_vec(ndvi_query, ~.$properties$start_datetime), 
      end_date = map_vec(ndvi_query, ~.$properties$end_datetime),
      platform = map_vec(ndvi_query, ~.$properties$platform),
      bbox = list(map(ndvi_query, ~.$bbox))
    ) |> 
      filter(platform == "terra")
    
    Sys.sleep(30)
    
    return(ndvi_params)
}
