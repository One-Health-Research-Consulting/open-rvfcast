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
transform_sentinel_ndvi <- function(sentinel_ndvi_api_parameters,
                                    continent_raster_template,
                                    sentinel_ndvi_transformed_directory,
                                    overwrite = FALSE,
                                    ...) {
  
  # Create directory if it does not yet exist
  dir.create(sentinel_ndvi_transformed_directory, recursive = TRUE, showWarnings = FALSE)
  
  template <- terra::unwrap(continent_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  raw_filename <- file.path(sentinel_ndvi_transformed_directory, sentinel_ndvi_api_parameters$properties$title)
  
  # Extract start and end dates from the raw downloaded file name
  # naming conventions
  # https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-3-synergy/naming-conventions
  filename_dates <- regmatches(basename(raw_filename), gregexpr("\\d{8}", basename(raw_filename))) |> 
    unlist() |>
    map_vec(~as.Date(.x, format = "%Y%m%d"))
  start_date <- min(filename_dates)
  end_date <- max(filename_dates)
  
  sentinel_ndvi_filename <- file.path(sentinel_ndvi_transformed_directory, glue::glue("transformed_sentinel_NDVI_{start_date}_to_{end_date}.gz.parquet"))
  
  # Check if glw files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(sentinel_ndvi_filename)) & !overwrite) {
    message("transformed sentinel ndvi parquet file already exists and can be loaded, skipping download and processing")
    return(sentinel_ndvi_filename)
  }
  
  # Download raw data
  sentinel_ndvi_downloaded <- download_sentinel_ndvi(sentinel_ndvi_api_parameters, raw_filename)
  
  message(paste0("Transforming ", raw_filename))
  
  # Re-project to raster to template
  transformed_raster <- transform_raster(raw_raster = rast(sentinel_ndvi_downloaded),
                                         template = rast(continent_raster_template))
  
  # Convert raster to dataframe
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = NDVI) |> 
    mutate(start_date = start_date,
           end_date = end_date)
  
  # Save as parquet 
  arrow::write_parquet(dat_out, sentinel_ndvi_filename, compression = "gzip", compression_level = 5)
  
  # Clean up download file
  unlink(sentinel_ndvi_downloaded)
  
  return(sentinel_ndvi_filename)
}
