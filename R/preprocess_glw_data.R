#' Preprocess GLW Data
#'
#' This function performs preprocessing tasks on glw datasets.
#'
#' @author Nathan C. Layman
#'
#' @param glw_directory_dataset Directory containing GLW datasets. This directory is created if it doesn't exist.
#' @param glw_urls URLs of the GLW datasets to download.
#' @param continent_raster_template Template to be used for terra raster operations.
#' @param overwrite Boolean flag indicating whether existing preprocessed files should be overwritten. Default is FALSE.
#' @param ... Additional parameters not used by this function but included for generic function compatibility.
#' 
#' @return A character vector of filepaths to the preprocessed GLW dataset files.
#'
#' @note This function creates a new directory, downloads datasets, processes data and saves results
#'       as parquet files in the specified directory.
#'
#' @example 
#' preprocess_glw_data(glw_directory_dataset = "./data",
#'                     glw_urls = c("http://example.com/dataset1", "http://example.com/dataset2"),
#'                     continent_raster_template = raster_template,
#'                     overwrite = TRUE)
#'
#' @export
preprocess_glw_data <- function(glw_directory_dataset, 
                                glw_urls, 
                                continent_raster_template,
                                overwrite = FALSE,
                                ...) {
  
  # Create directory if it does not yet exist
  dir.create(glw_directory_dataset, recursive = TRUE, showWarnings = FALSE)
  
  # Unwrap terra raster
  continent_raster_template <- terra::unwrap(continent_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  # GLW filenames
  glw_filenames <- file.path(glw_directory_dataset, paste0(names(glw_urls), ".parquet"))
  
  # Check if glw files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(glw_filenames)) & !overwrite) {
    message("preprocessed glw parquet file already exists and can be loaded, skipping download and processing")
    return(glw_filenames)
  }
  
  # Raw filenames
  glw_filenames_raw <- file.path(glw_directory_dataset, paste0(names(glw_urls), ".tif"))
  
  # Download raw rasters
  map2(glw_urls, glw_filenames_raw, ~download.file(url=.x, destfile=.y))
  
  # Transform rasters
  transformed_rasters <- map(glw_filenames_raw, ~transform_raster(raw_raster = terra::rast(.x),
                                                                  template = continent_raster_template) |>
                               as.data.frame(xy = TRUE) |> 
                               as_tibble())

  map2(transformed_rasters, glw_filenames, ~arrow::write_parquet(.x, .y, compression = "gzip", compression_level = 5))
  
  # Clean up tif files
  file.remove(glw_filenames_raw)
  
  return(glw_filenames)
}
