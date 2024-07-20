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
get_elevation_data <- function(output_dir, 
                               output_filename, 
                               raster_template) {
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  template <- terra::unwrap(raster_template)
  
  # Create a bounding bounding box template
  elevation_data <- geodata::elevation_global(res = 0.5 , 
                                              path = output_dir)
  
  elevation_data <- transform_raster(elevation_data, 
                                template = template)
  
  filename = paste(output_dir, output_filename, sep = "/")
  
  if(grepl("\\.parquet", filename)) {
    # Convert to dataframe
    dat <- as.data.frame(elevation_data, xy = TRUE) |> as_tibble()
    
    # Save as parquet 
    arrow::write_parquet(dat, filename, compression = "gzip", compression_level = 5)
    
    terra::writeRaster(elevation_data, filename=gsub("parquet", "tif", filename), overwrite=T, gdal=c("COMPRESS=LZW"))
    
  } else {
    terra::writeRaster(elevation_data, filename=filename, overwrite=T, gdal=c("COMPRESS=LZW"))
  }
  
  unlink(paste(output_dir, "elevation", sep = "/"), recursive=TRUE)
  
  # Return path to compressed raster
  return(filename)
}