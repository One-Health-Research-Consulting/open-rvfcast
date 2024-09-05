#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_downloaded_subset
#' @param continent_raster_template
#' @param modis_ndvi_transformed_directory
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
transform_modis_ndvi <- function(modis_ndvi_downloaded_subset,
                                 continent_raster_template,
                                 modis_ndvi_transformed_directory, 
                                 overwrite = FALSE) {
  
  # Extract start and end dates from the raw downloaded file name
  filename <- basename(modis_ndvi_downloaded_subset)
  year_doy <- sub(".*doy(\\d+).*", "\\1", filename) 
  start_date <- as.Date(year_doy, format = "%Y%j") # confirmed this is start date through manual download tests 

  # Set filename for saving
  save_filename <- glue::glue("transformed_modis_NDVI_{start_date}.gz.parquet")
  message(paste0("Transforming ", save_filename))
  
  # MOD13A2.061__1_km_16_days_NDVI_doy2004353_aid0001.tif' not recognized as a supported file format
  
  # Check if file already exists
  existing_files <- list.files(modis_ndvi_transformed_directory)
  if(save_filename %in% existing_files & !overwrite){
    message("file already exists, skipping transform")
    return(file.path(modis_ndvi_transformed_directory, save_filename))
  }
  
  # Transform with template raster
  transformed_raster <- transform_raster(raw_raster = rast(modis_ndvi_downloaded_subset),
                                         template = rast(continent_raster_template))
  
  # Convert to dataframe
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = 3) |> 
    mutate(start_date = start_date)
  
  # Save as parquet 
  write_parquet(dat_out, here::here(modis_ndvi_transformed_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(modis_ndvi_transformed_directory, save_filename))
  
  
}
