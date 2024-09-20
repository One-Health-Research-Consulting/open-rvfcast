#' Reproject, resample, crop, and normalize a raster to template
#'
#' @param raw_raster 
#' @param template 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
transform_raster <- function(raw_raster, 
                             template, 
                             method = "cubicspline") {
  
  norm_rast <- raw_raster
  if(!identical(terra::crs(norm_rast), terra::crs(template))) {
    norm_rast <- terra::project(raw_raster, template)
  }
  if(!identical(terra::origin(norm_rast), terra::origin(template)) ||
     !identical(terra::res(norm_rast), terra::res(template))) {
    norm_rast <- terra::resample(norm_rast, template, method = method) 
  } 
  
  norm_rast <- terra::crop(norm_rast, template)
  
  return(norm_rast)
  
}

