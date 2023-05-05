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
download_modis_ndvi <- function(continent_bounding_box,
                                modis_ndvi_years,
                                download_directory = "data/modis_ndvi_rasters") {
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
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
  
  ndvi_parameters <- tibble(
    url = map_vec(ndvi_query, ~.$assets$`500m_16_days_NDVI`$href),
    id = map_vec(ndvi_query, ~.$id),
    start_date = map_vec(ndvi_query, ~.$properties$start_datetime), 
    end_date = map_vec(ndvi_query, ~.$properties$end_datetime),
    platform = map_vec(ndvi_query, ~.$properties$platform),
    bbox = list(map(ndvi_query, ~.$bbox))
  ) |> 
    filter(platform == "terra")
  
  # Plan to download by start date - each file will be a mosaic of the tiles for each NDVI day
  ndvi_params_split <- ndvi_parameters |> 
    mutate(id_lab = str_extract(id, "[^\\.]*\\.[^\\.]*")) |> 
    mutate(start_lab = str_remove_all(start_date, "\\:|\\-")) |> 
    mutate(end_lab = str_remove_all(end_date, "\\:|\\-")) |> 
    mutate(filename = paste0(paste(id_lab, "africa", start_lab, end_lab, sep = "_"), ".tif")) |> 
    group_split(start_date) 
  
  assertthat::assert_that(unique(map_vec(ndvi_params_split, ~n_distinct(.$filename)))==1)
  
  # Debugging check
  ### Some of these parameter tibble are double the number of rows
  ### seems to be the same three dates each year (manually checked 2005, 2006, 2018)
  # nrows <- map_vec(ndvi_params_split, nrow)
  # assertthat::are_equal(n_distinct(nrows), 2)
  # which_dupes <- which(nrows > median(nrows))
  # ndvi_params_split_with_dupes <- ndvi_params_split[which_dupes]
  ### They have dupe IDs
  # map_vec(ndvi_params_split_with_dupes, ~n_distinct(.$id))
  ### But they do have different URL endpoints
  # map_vec(ndvi_params_split_with_dupes, ~n_distinct(.$url))
  ### Other than URLs, everything we've extracted is the same
  # map_vec(ndvi_params_split_with_dupes, ~nrow(distinct(select(., -url))))
  ### Let's look at these dupes
  # message("spot checking that dupes are identical")
  # dupe_tiles_to_test <- map(ndvi_params_split_with_dupes, function(x){
  #   x |> 
  #     mutate(url = paste0("/vsicurl/", url)) |> 
  #     group_by(id) |> 
  #     group_split() |> 
  #     sample(size = 3) 
  # }) |> reduce(c)
  # assertthat::assert_that(all(imap_lgl(dupe_tiles_to_test, function(dup, i){
  #   print(i)
  #   assertthat::are_equal(nrow(dup), 2)
  #   tile1 <- as.data.frame(rast(dup$url[1]))
  #   tile2 <- as.data.frame(rast(dup$url[2]))
  #   identical(tile1, tile2)
  # })))
  # ### Since they are identical, let's just select the first
  # ndvi_params_split[which_dupes] <- map(ndvi_params_split[which_dupes], function(x){
  #   x |> group_by(id) |> slice(1) |> ungroup()
  # })
  # nrows_new <- map_vec(ndvi_params_split, nrow)
  # assertthat::are_equal(n_distinct(nrows_new), 1)
  
  # read in and mosaic the tiles 
  filenames <- map_vec(ndvi_params_split, function(params){
    filename <-  unique(params$filename)
    message(paste("downloading", filename))
    if(filename %in% existing_files){
      message("file already exists, skipping download")
      return(filename)
    }
    urls <- paste0("/vsicurl/", params$url)
    tiles <- map(urls, rast) 
    rast_downloaded <- do.call(terra::merge, tiles)
    terra::writeRaster(rast_downloaded, here::here(download_directory, unique(params$filename)), overwrite = T)
    return(filename)
  })
  
  return(file.path(download_directory, filenames))
  
}
