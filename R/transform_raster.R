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
  if(!identical(crs(norm_rast), crs(template))) {
    norm_rast <- terra::project(raw_raster, template)
  }
  if(!identical(origin(norm_rast), origin(template)) ||
     !identical(res(norm_rast), res(template))) {
    norm_rast <- terra::resample(norm_rast, template, method = method) 
  } 
  
  norm_rast <- crop(norm_rast, template)
  
  return(norm_rast)
  
}

