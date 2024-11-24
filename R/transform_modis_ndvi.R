#' Transform MODIS NDVI Datasets
#'
#' This function is used to download and transform a MODIS NDVI Dataset. It returns the path of the processed NDVI file.
#'
#' @author Nathan Layman, Emma Mendelsohn
#'
#' @param modis_ndvi_token token to authenticate the MODIS service, string.
#' @param modis_ndvi_bundle_request bundle request for the MODIS service, list.
#' @param continent_raster_template a raster template for the continent, of class terra.
#' @param modis_ndvi_transformed_directory directory where the processed files will be saved, character.
#' @param overwrite Boolean flag, if TRUE, overwrite existing transformed file.
#' @param ... Additional arguments not used by this function but potentially passed in for compatibility with generic methods.
#'
#' @return String for the path of the transformed MODIS NDVI file.
#'
#' @note If the transformed file already exists in the directory and overwrite = FALSE, the function will return the existing file path without performing the transformation again.
#'
#' @examples
#' transform_modis_ndvi(modis_ndvi_token = 'YOUR_TOKEN',
#'                      modis_ndvi_bundle_request = list(task_id = 'YOUR_TASK_ID', file_id = 'FILE_ID'),
#'                      continent_raster_template = terra::rast(CONTINENT_DATA),
#'                      modis_ndvi_transformed_directory = 'YOUR_DIRECTORY',
#'                      overwrite = FALSE)
#'
#' @export
transform_modis_ndvi <- function(modis_ndvi_token,
                                 modis_ndvi_bundle_request,
                                 continent_raster_template,
                                 modis_ndvi_transformed_directory,
                                 overwrite = FALSE,
                                 ...) {
  
  # Figure out raw file name and path
  raw_file <- file.path(modis_ndvi_transformed_directory, basename(modis_ndvi_bundle_request$file_name[[1]]))
  
  start_date <- modis_ndvi_bundle_request$start_date 
  end_date <- modis_ndvi_bundle_request$end_date
  
  continent_raster_template <- terra::unwrap(continent_raster_template)
  
  # Set transformed file name and path for saving
  transformed_file <- file.path(modis_ndvi_transformed_directory, glue::glue("transformed_modis_NDVI_{start_date}.gz.parquet"))
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  # Check if transformed file already exists and can be loaded. If so return file name and path
  # Check if glw files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(transformed_file)) & !overwrite) {
    message("preprocessed modis ndvi parquet file already exists and can be loaded, skipping download and processing")
    return(transformed_file)
  }
  
  # If not download temporary file
  message(paste0("Downloading ", transformed_file))
  
  task_id <- unique(modis_ndvi_bundle_request$task_id)
  file_id <- modis_ndvi_bundle_request$file_id
  
  # Write the file to disk
  response <- httr::GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, '/', file_id, sep = ""),
                        httr::write_disk(raw_file, overwrite = TRUE), httr::progress(), httr::add_headers(Authorization = modis_ndvi_token))
  
  httr::stop_for_status(response)
  
  # Verify rast can open the saved raster file. If not return NULL
  error_safe_read_rast <- possibly(terra::rast, NULL)
  raw_raster = error_safe_read_rast(raw_file)
  
  if(is.null(raw_raster)) {
    file.remove(raw_file)
    stop(glue::glue("Raw raster could not be read. GET response code: {response$status_code}"))
  }
  
  # If it can transform the rast then delete the raw file
  message(paste0("Transforming ", transformed_file))
  
  # This implements the step function interpolation across the 16 day interval
  transformed_raster <- transform_raster(raw_raster = raw_raster,
                                         template = continent_raster_template) |>
    as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = 3) |> 
    mutate(days_count = as.integer(end_date - start_date) + 1) |>
    uncount(days_count, .id = "step") |> # This is a pretty cool trick. Very fast.
    mutate(date = start_date + step - 1,
           doy = as.integer(lubridate::yday(date)),
           month = as.integer(lubridate::month(date)),
           year = as.integer(lubridate::year(date)),
           source = "modis")
  
  # Save transformed rast as parquet
  arrow::write_parquet(transformed_raster, transformed_file, compression = "gzip", compression_level = 5)
  
  # Test if transformed data parquet file can be loaded. If not clean up and return NULL
  if(is.null(error_safe_read_parquet(transformed_file))) {
    file.remove(transformed_file)
    stop("Transformed Modis NDVI parquet could not be read after transformation. Cleaning up.")
  }
  
  # Clean up raw file
  file.remove(raw_file)
  
  # If it can be loaded return path to transformed file
  return(transformed_file)
}
