#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_token
#' @param modis_ndvi_bundle_request
#' @param continent_raster_template
#' @param local_folder
#' @return
#' @author Nathan Layman
#' @export
transform_nasa_weather2 <- function(modis_ndvi_token,
                                   modis_ndvi_bundle_request,
                                   continent_raster_template,
                                   local_folder) {
  
  # Extract start and end dates to make filename
  start <- paste0(nasa_weather_years, "-01-01")
  end <- paste0(nasa_weather_years, "-12-31")
  current_time <- Sys.Date()
  if(nasa_weather_years == year(current_time)) end <- as.character(current_time-1)
  dates <- c(start, end)
  
  # Figure out raw file name and path
  raw_file <- file.path(local_folder,
                        paste("nasa", 
                              "recorded_weather", 
                              nasa_weather_coordinates$country_iso3c, 
                              nasa_weather_years, sep = "_"))
  
  raw_file <- paste0(raw_file, ".gz.parquet")
  
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