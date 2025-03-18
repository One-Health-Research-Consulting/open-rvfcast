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
#' @param fill_na Logical; if TRUE, NA values will be filled before transformation using focal operation. Default is FALSE.
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
                             method = "cubicspline",
                             fill_na = FALSE) {
  
  if (is.null(template)) {
    message("Returning raw raster (no transformation applied).")
    return(raw_raster)
  }
  
  template <- terra::unwrap(template)
  norm_rast <- raw_raster
  
  # Fill NA values using focal before transformation (optional)
  if (fill_na) {
    norm_rast <- terra::focal(norm_rast, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)
  }
  
  # Ensure CRS is set before transformation
  if (!is.na(terra::crs(template)) && !is.na(terra::crs(norm_rast))) {
    if (!identical(terra::crs(norm_rast), terra::crs(template))) {
      norm_rast <- terra::project(norm_rast, template)
    }
  } else {
    warning("CRS is not defined for one of the rasters. Transformation may not be accurate.")
  }
  
  # Resample if resolution or origin differ
  if (!identical(terra::origin(norm_rast), terra::origin(template)) ||
      !identical(terra::res(norm_rast), terra::res(template))) {
    norm_rast <- terra::resample(norm_rast, template, method = method) 
  }
  
  # Crop as the final step to ensure all data is used for interpolation
  norm_rast <- terra::crop(norm_rast, template)
  
  return(norm_rast)
}