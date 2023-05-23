save_transform_raster <- function(raster_file, template, transform_directory, verbose = FALSE) {
  if(verbose) cat(raster_file, "\n")
  template <- rast(template)
  raw_raster <- terra::rast(raster_file)
  filename <- paste0("transformed_", basename(raster_file))
  
  suppressWarnings(dir.create(transform_directory, recursive = TRUE))
  existing_files <- list.files(transform_directory)
  
  if(filename %in% existing_files){
    message("file already exists, skipping transform")
    return(file.path(transform_directory, filename))
  }
  
  if(!identical(crs(raw_raster), crs(template))) {
    raw_raster <- terra::project(raw_raster, template)
  }
  if(!identical(origin(raw_raster), origin(template)) ||
     !identical(res(raw_raster), res(template))) {
    raw_raster <- terra::resample(raw_raster, template, method = "cubicspline")
  } 
  terra::writeCDF(raw_raster, here::here(transform_directory, filename), overwrite = T)
  return(file.path(transform_directory, filename))
  
}

