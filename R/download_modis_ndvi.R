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
download_modis_ndvi <- function(modis_ndvi_parameters,
                                download_directory = "data/modis_ndvi_rasters") {
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
  # Plan to download by start date - each file will be a mosaic of the tiles for each NDVI day
  ndvi_params_split <- modis_ndvi_parameters |> 
    mutate(id_lab = str_extract(id, "[^\\.]*\\.[^\\.]*")) |> 
    mutate(start_lab = str_remove_all(start_date, "\\:|\\-")) |> 
    mutate(end_lab = str_remove_all(end_date, "\\:|\\-")) |> 
    mutate(filename = paste0(paste(id_lab, "africa", start_lab, end_lab, sep = "_"), ".tif")) |> 
    group_split(start_date) 
  
  assertthat::assert_that(unique(map_vec(ndvi_params_split, ~n_distinct(.$filename)))==1)
  
  # Debugging check
  ### Some of these parameter tibble are double the number of rows
  ### seems to be the same three dates each year (manually checked 2005, 2006, 2018)
  nrows <- map_vec(ndvi_params_split, nrow)
  assertthat::are_equal(n_distinct(nrows), 2)
  which_dupes <- which(nrows > median(nrows))
  ndvi_params_split_with_dupes <- ndvi_params_split[which_dupes]
  ### They have dupe IDs
  # map_vec(ndvi_params_split_with_dupes, ~n_distinct(.$id))
  ### But they do have different URL endpoints
  # map_vec(ndvi_params_split_with_dupes, ~n_distinct(.$url))
  ### Other than URLs, everything we've extracted is the same
  # map_vec(ndvi_params_split_with_dupes, ~nrow(distinct(select(., -url))))
  ### Let's look at these dupes
  message("spot checking that dupes are identical")
  dupe_tiles_to_test <- map(ndvi_params_split_with_dupes, function(x){
    x |> 
      mutate(url = paste0("/vsicurl/", url)) |> 
      group_by(id) |> 
      group_split() |> 
      sample(size = 3) 
  }) |> reduce(c)
  assertthat::assert_that(all(imap_lgl(dupe_tiles_to_test, function(dup, i){
    print(i)
    assertthat::are_equal(nrow(dup), 2)
    tile1 <- as.data.frame(rast(dup$url[1]))
    tile2 <- as.data.frame(rast(dup$url[2]))
    identical(tile1, tile2)
  })))
  ### Since they are identical, let's just select the first
  ndvi_params_split[which_dupes] <- map(ndvi_params_split[which_dupes], function(x){
    x |> group_by(id) |> slice(1) |> ungroup()
  })
  nrows_new <- map_vec(ndvi_params_split, nrow)
  assertthat::are_equal(n_distinct(nrows_new), 2)
  
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
