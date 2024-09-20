#' Transform and download MODIS NDVI data from NASA AppEEARS
#'
#' This function downloads, transforms, and saves MODIS NDVI data from NASA's AppEEARS API.
#' The function handles the conversion of the raw raster data into a standardized format
#' (parquet file), aligning it with a continental raster template. If the transformed file
#' already exists and can be read, it will return the existing file.
#' 
#' @author Nathan Layman, Emma Mendelsohn
#' 
#' @param modis_ndvi_token Character. The authentication token required for the AppEEARS API.
#' @param modis_ndvi_bundle_request List. Contains the `file_name`, `task_id`, and `file_id` from the AppEEARS bundle request for MODIS NDVI data.
#' @param continent_raster_template Character. The file path to the template raster used for resampling the MODIS NDVI data.
#' @param local_folder Character. The path to the local directory where both raw and transformed files are saved.
#' 
#' @return A list of successfully transformed files
#' 
#' @export
transform_modis_ndvi <- function(modis_ndvi_token,
                                 modis_ndvi_bundle_request,
                                 continent_raster_template,
                                 local_folder) {
  
  # Figure out raw file name and path
  raw_file <- file.path(local_folder, basename(modis_ndvi_bundle_request$file_name))
  
  # Extract start and end dates from the raw downloaded file name
  year_doy <- sub(".*doy(\\d+).*", "\\1", basename(raw_file))
  start_date <- as.Date(year_doy, format = "%Y%j") # confirmed this is start date through manual download tests 
  
  # Set transformed file name and path for saving
  transformed_file <- file.path(local_folder, glue::glue("transformed_modis_NDVI_{start_date}.gz.parquet"))
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  # Check if transformed file already exists and can be loaded. If so return file name and path
  if(!is.null(error_safe_read_parquet(transformed_file))) return(transformed_file)
  
  # If not download temporary file
  message(paste0("Downloading ", raw_file))
  
  task_id <- unique(modis_ndvi_bundle_request$task_id)
  file_id <- modis_ndvi_bundle_request$file_id
  
  # Write the file to disk
  response <- httr::GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, '/', file_id, sep = ""),
                        httr::write_disk(raw_file, overwrite = TRUE), httr::progress(), httr::add_headers(Authorization = modis_ndvi_token))
  
  # Verify rast can open the saved raster file. If not return NULL
  error_safe_read_rast <- possibly(terra::rast, NULL)
  raw_raster = error_safe_read_rast(raw_file)
  
  if(is.null(raw_raster)) {
    file.remove(raw_file)
    return(NULL)
  }
  
  # If it can transform the rast then delete the raw file
  message(paste0("Transforming ", transformed_file))
  
  transformed_raster <- transform_raster(raw_raster = raw_raster,
                                         template = rast(continent_raster_template)) |>
    as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = 3) |> 
    mutate(start_date = start_date)
  
  # Save transformed rast as parquet
  arrow::write_parquet(transformed_raster, here::here(modis_ndvi_transformed_directory, save_filename), compression = "gzip", compression_level = 5)
  
  # Clean up raw file
  file.remove(raw_file)
  
  # Test if parquet file can be loaded. If not clean up and return NULL
  if(is.null(error_safe_read_parquet(transformed_file))) {
    file.remove(transformed_file)
    return(NULL)
  }
  
  # If it can be loaded remove return filename and path of transformed raster
  return(transformed_file)
}