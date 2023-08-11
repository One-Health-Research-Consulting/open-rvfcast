transform_raster <- function(raw_raster, template) {
  
  template <- rast(template)

  if(!identical(crs(raw_raster), crs(template))) {
    raw_raster <- terra::project(raw_raster, template)
  }
  if(!identical(origin(raw_raster), origin(template)) ||
     !identical(res(raw_raster), res(template))) {
    raw_raster <- terra::resample(raw_raster, template, method = "cubicspline")
  } 
  
  return(raw_raster)
  
}

