#' Retrieve and preprocess global elevation data
#'
#' This function downloads global elevation data, transforms it, and saves it as an optimized Parquet file
#' and a TIF file in the specified directory. If a file already exists at the target filepath and overwrite
#' is FALSE, the existing file is returned.
#'
#' @author Nathan C. Layman
#'
#' @param output_dir Directory where the processed files will be saved. This directory is created if it doesn't exist.
#' @param output_filename Desired filename for the processed file.
#' @param continent_raster_template Template to be used for terra raster operations.
#' @param overwrite Boolean flag indicating whether existing processed files should be overwritten. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A string containing the filepath to the processed file.
#'
#' @note This function creates a new directory, downloads elevation data, processes and saves results
#' as parquet and tif files in the specified directory. If a file already exists at the target filepath and
#' overwrite is FALSE, the existing file is returned.
#'
#' @examples
#' get_elevation_data(output_dir = './data',
#'                    output_filename = 'elevation.parquet',
#'                    continent_raster_template = raster_template,
#'                    overwrite = TRUE)
#'
#' @export
get_elevation_data <- function(output_dir, 
                               output_filename, 
                               continent_raster_template,
                               overwrite = FALSE,
                               ...) {
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Unwrap the raster template
  continent_raster_template <- terra::unwrap(continent_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  # GLW filenames
  elevation_filename <- file.path(output_dir, output_filename)
  
  if(!is.null(error_safe_read_parquet(elevation_filename)) & !overwrite) {
    message("preprocessed elevation parquet file already exists and can be loaded, skipping download and processing")
    return(elevation_filename)
  }
  
  # Create a bounding bounding box template
  elevation_data <- geodata::elevation_global(res = 0.5, path = output_dir)
  elevation_data <- transform_raster(elevation_data, template = continent_raster_template)
    
  if(grepl("\\.parquet", elevation_filename)) {
    
    # Convert to dataframe
    dat <- as.data.frame(elevation_data, xy = TRUE) |> as_tibble()
    
    # Save as parquet 
    arrow::write_parquet(dat, elevation_filename, compression = "gzip", compression_level = 5)
    terra::writeRaster(elevation_data, filename=gsub("parquet", "tif", elevation_filename), overwrite=T, gdal=c("COMPRESS=LZW"))
    
  } else {
    terra::writeRaster(elevation_data, filename=elevation_filename, overwrite=T, gdal=c("COMPRESS=LZW"))
    # Check if glw files exist and can be read and that we don't want to overwrite them.
  }

  # Clean up raw data  
  unlink(paste(output_dir, "elevation", sep = "/"), recursive=TRUE)

  # Return path to compressed raster
  return(elevation_filename)
}
