#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param raster_file
#' @param template
#' @param sentinel_ndvi_transformed_directory
#' @return
#' @author Emma Mendelsohn
#' @export
transform_sentinel_ndvi <- function(sentinel_ndvi_downloaded,
                                    continent_raster_template,
                                    sentinel_ndvi_transformed_directory,
                                    overwrite = FALSE) {
  
  # Extract start and end dates from the raw downloaded file name
  filename <- basename(sentinel_ndvi_downloaded)
  assertthat::are_equal(nchar(filename), 97)
  start_date <- as.Date(str_sub(filename, 17, 24), format = "%Y%m%d")
  end_date <- as.Date(str_sub(filename, 33, 40), format = "%Y%m%d")
  
  # Set filename for saving
  save_filename <- glue::glue("transformed_sentinel_NDVI_{start_date}_to_{end_date}.gz.parquet")
  message(paste0("Transforming ", save_filename))
  
  # Check if file already exists
  existing_files <- list.files(sentinel_ndvi_transformed_directory)
  if(save_filename %in% existing_files & !overwrite){
    message("file already exists, skipping transform")
    return(file.path(sentinel_ndvi_transformed_directory, save_filename))
  }
  
  # Transform with template raster
  transformed_raster <- transform_raster(raw_raster = rast(sentinel_ndvi_downloaded),
                                         template = rast(continent_raster_template))
  
  # Convert to dataframe
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = NDVI) |> 
    mutate(start_date = start_date,
           end_date = end_date)
  
  # Save as parquet 
  write_parquet(dat_out, here::here(sentinel_ndvi_transformed_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(sentinel_ndvi_transformed_directory, save_filename))
  
}
