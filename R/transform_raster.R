#' Transforms a Raster image to a specified template
#'
#' This function takes a raw_raster as input and transforms it into a norm_raster using a specified template and method.
#' It handles differences in CRS, origin, and resolution between the raw_raster and the template raster.
#'
#' @author Nathan C. Layman
#'
#' @param raw_raster The raw Raster object to be transformed.
#' @param template The template Raster object to be used as reference. If NULL, raw_raster will be returned as is.
#' @param method The method to be used for resampling. Default is "cubicspline".
#'
#' @return A Raster object that has been transformed according to the given template and method.
#'
#' @note The non-matching CRS, origin and resolution between raw_raster and template Raster are handled in this function.
#'
#' @examples
#' transform_raster(raw_raster = raw_raster_object,
#'                  template = template_raster_object,
#'                  method = 'cubicspline')
#'
#' @export
transform_raster <- function(raw_raster, 
                             template = NULL, 
                             method = "cubicspline") {
  
  template <- terra::unwrap(template)
  
  if(is.null(template)) {
    message("returning raw raster")
    return(raw_raster)
  }
  
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

