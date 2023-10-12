#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_downloaded
#' @param continent_raster_template
#' @param modis_ndvi_directory_transformed
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
transform_modis_ndvi <- function(modis_ndvi_downloaded_subset,
                                      continent_raster_template,
                                      modis_ndvi_directory_transformed, overwrite =
                                        FALSE) {
  
  # Extract start and end dates from the raw downloaded file name
  filename <- basename(modis_ndvi_downloaded_subset)
  year_doy <- sub(".*doy(\\d+).*", "\\1", filename) 
  start_date <- as.Date(year_doy, format = "%Y%j") # confirmed this is start date through manual download tests 
  end_date <- start_date + 16 
  
  # Set filename for saving
  save_filename <- glue::glue("transformed_modis_NDVI_{start_date}_to_{end_date}.gz.parquet")
  message(paste0("Transforming ", save_filename))
  
  # Check if file already exists
  existing_files <- list.files(modis_ndvi_directory_transformed)
  if(save_filename %in% existing_files & !overwrite){
    message("file already exists, skipping transform")
    return(file.path(modis_ndvi_directory_transformed, save_filename))
  }
  
  # Transform with template raster
  transformed_raster <- transform_raster(raw_raster = rast(modis_ndvi_downloaded),
                                         template = rast(continent_raster_template))
  
  # Convert to dataframe
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = 3) |> 
    mutate(start_date = start_date,
           end_date = end_date)
  
  # Save as parquet 
  write_parquet(dat_out, here::here(modis_ndvi_directory_transformed, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(modis_ndvi_directory_transformed, save_filename))
  
  
}
