#' Retrieve and preprocess global land cover data
#'
#' The function 'get_landcover_data' downloads global land cover data, preprocesses it based on the provided types,
#' and saves it as a Parquet file and a TIF file in the specified directory. If a file already exists at the target
#' filepath and overwrite is set to FALSE, the existing file is returned without downloading and preprocessing the data.
#'
#' @author Nathan C. Layman
#'
#' @param output_dir Directory where the downloaded and preprocessed files will be saved. The directory is created if it doesn't exist.
#' @param output_filename Filename for the output files.
#' @param landcover_types Types of land cover data to retrieve. The data is downloaded according to these types.
#' @param continent_raster_template Raster template to be used for land cover data transformation.
#' @param overwrite Boolean option to indicate whether to overwrite existing files. Default is FALSE.
#' @param ... Additional parameters not required by this function but included for compatibility with generic functions.
#'
#' @return A string containing the filepath to the preprocessed and stored Parquet file.
#'
#' @note If a file already exists at the target filepath and overwrite is FALSE, the existing file is returned without 
#' downloading and preprocessing the data again.
#'
#' @examples
#' get_landcover_data(output_dir = './data', output_filename = 'landcover.parquet',
#'                    landcover_types = c('forest', 'urban', 'water'), 
#'                    continent_raster_template = raster_template, overwrite = TRUE)
#'
#' @export
get_landcover_data <- function(output_dir, 
                               output_filename, 
                               landcover_types,
                               continent_raster_template,
                               overwrite = FALSE,
                               ...) {
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  continent_raster_template <- terra::unwrap(continent_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  # GLW filenames
  landcover_filename <- file.path(output_dir, output_filename)
  
  # Check if glw files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(landcover_filename)) & !overwrite) {
    message("preprocessed landcover parquet file already exists and can be loaded, skipping download and processing")
    return(landcover_filename)
  }
  
  # Fetch each layer, process them and stack them into a single SpatRaster
  # Cleaning up files as we go to save space.
  landcover_data <- map(landcover_types, function(l) {
    landcover <- geodata::landcover(var = l, path = output_dir)
    file <- terra::sources(landcover)
    landcover <- transform_raster(landcover, continent_raster_template)
    unlink(file) # Clean up as we go along these files are huge.
    landcover
  })
  
  # Bind into one raster
  landcover_data <- do.call(c, landcover_data)
  
  if(grepl("\\.parquet", landcover_filename)) {
    # Convert to dataframe
    dat <- as.data.frame(landcover_data, xy = TRUE) |> as_tibble()
    
    # Save as parquet 
    arrow::write_parquet(dat, landcover_filename, compression = "gzip", compression_level = 5)
    
    terra::writeRaster(landcover_data, filename=gsub("parquet", "tif", landcover_filename), overwrite=T, gdal=c("COMPRESS=LZW"))
    
  } else {
    terra::writeRaster(landcover_data, filename=landcover_filename, overwrite=T, gdal=c("COMPRESS=LZW"))
  }
  
  # Clean up raw files which are very large and no longer needed.
  unlink(paste(output_dir, "landuse", sep = "/"), recursive=TRUE)
  
  return(landcover_filename)
}
