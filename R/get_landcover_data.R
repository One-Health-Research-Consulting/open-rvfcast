get_landcover_data <- function(output_dir, 
                               output_filename, 
                               raster_template) {
  
  template <- terra::unwrap(raster_template)
  
  landcover_types <- c("trees", "grassland", "shrubs", "cropland", "built", "bare", "snow", "water", "wetland", "mangroves", "moss")
  
  # Fetch each layer, process them and stack them into a single SpatRaster
  # Cleaning up files as we go to save space.
  landcover_data <- map(landcover_types, function(l) {
    landcover <- geodata::landcover(var = l, path = output_dir)
    file <- sources(landcover)
    landcover <- transform_raster(landcover, template)
    unlink(file)
    landcover
  })
  
  landcover_data <- do.call(c, landcover_data)
  
  filename = paste(output_dir, output_filename, sep = "/")
  
  if(grepl("\\.parquet", filename)) {
    # Convert to dataframe
    dat <- as.data.frame(landcover_data, xy = TRUE) |> as_tibble()
    
    # Save as parquet 
    arrow::write_parquet(dat, filename, compression = "gzip", compression_level = 5)
    
    terra::writeRaster(landcover_data, filename=gsub("parquet", "tif", filename), overwrite=T, gdal=c("COMPRESS=LZW"))
    
  } else {
    terra::writeRaster(landcover_data, filename=filename, overwrite=T, gdal=c("COMPRESS=LZW"))
  }
  
  unlink(paste(output_dir, "landuse", sep = "/"), recursive=TRUE)
  
  return(filename)
}