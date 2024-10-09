#' Transform a raster to match a template
#'
#' This function takes a raw raster, and transform it to have the same
#' crs, resolution, and origin as the provided template.
#' The transforming process is done using a provided method, defaults to "cubicspline"
#'
#' @author Nathan C. Layman
#'
#' @param raw_raster A raster to be transformed.
#' @param template A template raster that the raw raster will match to.
#' @param method A method used in the resampling process when the raw raster is transformed to match the template, 
#' default is "cubicspline".
#'
#' @return A raster that has been transformed to match the template raster.
#'
#' @note This function uses the `terra` R package for handling rasters.
#'
#' @examples
#' # assuming `r` is a raw raster, and `t` is template raster
#' transformed_raster = transform_raster(r, t, method = "bilinear")
#'
#' @export
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

