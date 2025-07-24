#' Transforms a SpatRaster to a specified template
#'
#' This function takes a raw_raster as input and transforms it into a standardized raster using a specified template and method.
#' It handles differences in CRS, origin, resolution, and extent between the raw_raster and the template raster.
#'
#' @author Nathan C. Layman
#'
#' @param raw_raster A SpatRaster object to be transformed.
#' @param template A SpatRaster object to be used as reference. If NULL, raw_raster will be returned as is.
#' @param method The method to be used for resampling. Default is "cubicspline".
#' @param fill_na Logical; if TRUE, NA values will be filled before transformation using focal operation. Default is FALSE.
#' @param verbose Logical; if TRUE, diagnostic messages will be printed. Default is FALSE.
#'
#' @return A SpatRaster object that has been transformed according to the given template and method.
#'
#' @note The function handles differences in CRS, origin, resolution, and extent between raw_raster and template.
#'
#' @importFrom terra rast focal project resample crop crs origin res
#'
#' @examples
#' transform_raster(raw_raster = raw_raster_object,
#'                  template = template_raster_object,
#'                  method = 'cubicspline')
#'                  
#' # With verbose output
#' transform_raster(raw_raster = raw_raster_object,
#'                  template = template_raster_object,
#'                  method = 'cubicspline',
#'                  verbose = TRUE)
#'
#' @export
transform_raster <- function(raw_raster,
                            template = NULL,
                            method = "cubicspline",
                            fill_na = FALSE,
                            verbose = FALSE) {
  
  # Return early if no template provided
  if (is.null(template)) {
    if (verbose) message("Returning raw raster (no transformation applied).")
    return(raw_raster)
  }
  
  # Ensure rasters are properly loaded (handles wrapped rasters automatically)
  raw_raster <- terra::rast(raw_raster)
  template <- terra::rast(template)
  
  norm_rast <- raw_raster
  
  # Fill NA values using focal before transformation (optional)
  # This fills gaps in sparse rasterized data before interpolation
  if (fill_na) {
    norm_rast <- terra::focal(norm_rast, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)
  }
  
  # Handle CRS differences
  if (!is.na(terra::crs(template)) && !is.na(terra::crs(norm_rast))) {
    if (!identical(terra::crs(norm_rast), terra::crs(template))) {
      if (verbose) message("Projecting raster to template CRS...")
      norm_rast <- terra::project(norm_rast, template)
    }
  } else {
    warning("CRS is not defined for one of the rasters. Transformation may not be accurate.")
  }
  
  # Resample if resolution or origin differ (needed for NASA coarse -> template fine grid)
  if (!identical(terra::origin(norm_rast), terra::origin(template)) ||
      !identical(terra::res(norm_rast), terra::res(template))) {
    if (verbose) message("Resampling raster to match template grid...")
    norm_rast <- terra::resample(norm_rast, template, method = method)
  }
  
  # Crop as the final step to ensure exact template extent
  norm_rast <- terra::crop(norm_rast, template)
  
  return(norm_rast)
}
