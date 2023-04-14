#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_api_parameters
#' @param download_directory
#' @return
#' @author Emma Mendelsohn
#' @export
download_modis_ndvi <- function(modis_country_bounding_boxes_years, download_directory =
                                  "data/modis_ndvi_rasters") {
  
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  
  bbox_coords <- unlist(modis_country_bounding_boxes_years$bounding_box)
  country_name <- modis_country_bounding_boxes_years$country
  year <- modis_country_bounding_boxes_years$year
  
  # Connect to API 
  planetary_query <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  
  # This is the NDVI collection
  collection <- "modis-13A1-061" # this is 500m (modis-13Q1-061 is 250m)
  
  # Run query for year/country
  ndvi_query <- planetary_query |> 
    stac_search(collections = collection,
                limit = 1000,
                datetime = paste0(year, "-01-01T00:00:00Z/", year, "-12-31T23:59:59Z"),
                bbox = bbox_coords) |>
    get_request() |> 
    items_sign(sign_fn = sign_planetary_computer())
  
  # Save the parameters from the query
  ndvi_params <- tibble(
    url = map_vec(ndvi_query$features, ~.$assets$`250m_16_days_NDVI`$href),
    id = map_vec(ndvi_query$features, ~.$id),
    start_date = map_vec(ndvi_query$features, ~.$properties$start_datetime), 
    end_date = map_vec(ndvi_query$features, ~.$properties$end_datetime),
    bbox = list(map(ndvi_query$features, ~.$bbox))
  )
  
  # Plan to download by start date - each file will be a mosaic of the tiles for each NDVI day
  ndvi_params_split <- ndvi_params |> 
    mutate(id_lab = str_extract(id, "[^\\.]*\\.[^\\.]*")) |> 
    mutate(start_lab = str_remove_all(start_date, "\\:|\\-")) |> 
    mutate(end_lab = str_remove_all(end_date, "\\:|\\-")) |> 
    mutate(filename = paste0(paste(id_lab, tolower(country_name), start_lab, end_lab, sep = "_"), ".tif")) |> 
    group_split(start_date) 
  
  assertthat::assert_that(unique(map_vec(ndvi_params_split, ~n_distinct(.$filename)))==1)
  
  # read in and mosaic the tiles 
  filenames <- map_vec(ndvi_params_split, function(params){
    message(paste("downloading",  unique(params$filename)))
    urls <- paste0("/vsicurl/", params$url)
    tiles <- map(urls, rast) 
    if(length(tiles)>1){
      rast_downloaded <- do.call(terra::merge, tiles)
    }else{
      rast_downloaded <- tiles[[1]]
    }
    terra::writeRaster(rast_downloaded, here::here(download_directory, unique(params$filename)), overwrite = T)
    return(unique(params$filename))
  })
  
  return(file.path(download_directory, filenames))
  
}
