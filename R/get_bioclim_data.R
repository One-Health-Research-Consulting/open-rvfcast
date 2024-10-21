#' Retrieve and preprocess global bioclimatic data
#'
#' This function downloads global bioclimatic data, transforms it, and saves it as a Parquet file and a TIF
#' file in the specified directory. If a file already exists at the target filepath and overwrite is FALSE, 
#' the existing file is returned.
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
#' @note This function creates a new directory, downloads bioclimatic data, processes and saves results
#' as parquet and tif files in the specified directory. If a file already exists at the target filepath and 
#' overwrite is FALSE, the existing file is returned. The downloaded data includes various bioclimatic variables.
#'
#' @examples
#' get_bioclim_data(output_dir = './data',
#'                  output_filename = 'bioclim.parquet',
#'                  continent_raster_template = raster_template,
#'                  overwrite = TRUE)
#'
#' @export
get_bioclim_data <- function(output_dir, 
                             output_filename, 
                             continent_raster_template,
                             overwrite = FALSE,
                             ...) {
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  template <- terra::unwrap(continent_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  # GLW filenames
  bioclim_filename <- file.path(output_dir, output_filename)
  
  # Check if glw files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(bioclim_filename)) & !overwrite) {
    message("preprocessed bioclim parquet file already exists and can be loaded, skipping download and processing")
    return(bioclim_filename)
  }
  
  bioclim_data <- geodata::worldclim_global(var = "bio", res = 2.5, path = output_dir)
  
  bioclim_data <- transform_raster(bioclim_data, template)
  
  bioclim_names <- c(
    "Annual_Mean_Temperature", "Mean_Diurnal_Range", "Isothermality",
    "Temperature_Seasonality", "Max_Temperature_of_Warmest_Month",
    "Min_Temperature_of_Coldest_Month", "Temperature_Annual_Range",
    "Mean_Temperature_of_Wettest_Quarter", "Mean_Temperature_of_Driest_Quarter",
    "Mean_Temperature_of_Warmest_Quarter", "Mean_Temperature_of_Coldest_Quarter",
    "Annual_Precipitation", "Precipitation_of_Wettest_Month",
    "Precipitation_of_Driest_Month", "Precipitation_Seasonality",
    "Precipitation_of_Wettest_Quarter", "Precipitation_of_Driest_Quarter",
    "Precipitation_of_Warmest_Quarter", "Precipitation_of_Coldest_Quarter")
  
  # Assign the new names to the layers
  names(bioclim_data) <- bioclim_names
  
  if(grepl("\\.parquet", bioclim_filename)) {
    # Convert to dataframe
    dat <- as.data.frame(bioclim_data, xy = TRUE) |> as_tibble()
    
    # Save as parquet 
    arrow::write_parquet(dat, bioclim_filename, compression = "gzip", compression_level = 5)
    
    terra::writeRaster(bioclim_data, filename=gsub("parquet", "tif", bioclim_filename), overwrite=T, gdal=c("COMPRESS=LZW"))
    
  } else {
    terra::writeRaster(bioclim_data, filename=bioclim_filename, overwrite=T, gdal=c("COMPRESS=LZW"))
  }
  
  unlink(paste(output_dir, "climate", sep = "/"), recursive=TRUE)
  
  return(bioclim_filename)
}
