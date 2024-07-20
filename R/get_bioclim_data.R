#' Title
#'
#' @param output_dir 
#' @param output_filename 
#' @param raster_template 
#'
#' @return
#' @export
#'
#' @examples
get_bioclim_data <- function(output_dir, 
                             output_filename, 
                             raster_template) {
  
  template <- terra::unwrap(raster_template)
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
  
  filename = paste(output_dir, output_filename, sep = "/")
  
  if(grepl("\\.parquet", filename)) {
    # Convert to dataframe
    dat <- as.data.frame(bioclim_data, xy = TRUE) |> as_tibble()
    
    # Save as parquet 
    arrow::write_parquet(dat, filename, compression = "gzip", compression_level = 5)
    
    terra::writeRaster(bioclim_data, filename=gsub("parquet", "tif", filename), overwrite=T, gdal=c("COMPRESS=LZW"))
    
  } else {
    terra::writeRaster(bioclim_data, filename=filename, overwrite=T, gdal=c("COMPRESS=LZW"))
  }
  
  unlink(paste(output_dir, "climate", sep = "/"), recursive=TRUE)
  
  return(filename)
}