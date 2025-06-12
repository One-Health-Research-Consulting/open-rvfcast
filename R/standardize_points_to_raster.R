#' Standardize Points to Raster Grid Using Interpolation
#'
#' This function directly converts point data to a standardized raster that matches a template raster grid.
#' It uses a two-step process: initial rasterization followed by cubicspline interpolation to ensure smooth results
#' that conform exactly to the template grid specifications.
#'
#' @param points_df A dataframe containing point data with coordinates and values
#' @param template_raster A SpatRaster object defining the desired output grid properties (resolution, extent, CRS)
#' @param value_col Character string specifying the column in points_df containing the values to be rasterized (default: "temperature")
#' @param x_col Character string specifying the column in points_df containing x-coordinates (default: "x")
#' @param y_col Character string specifying the column in points_df containing y-coordinates (default: "y")
#' @param method The method to be used for resampling in transform_raster. Default is "cubicspline".
#' @param fill_na Logical; if TRUE, NA values will be filled before transformation using focal operation. Default is TRUE.
#' @param verbose Logical; if TRUE, diagnostic messages will be printed. Default is FALSE.
#'
#' @return A SpatRaster object with the same properties as template_raster, filled with interpolated values from points_df
#'
#' @importFrom terra vect rasterize crs
#'
#' @examples
#' # Create precipitation raster with NA handling
#' precip_raster <- standardize_points_to_raster(
#'   points_df = weather_data,
#'   template_raster = my_template,
#'   value_col = "precipitation",
#'   fill_na = TRUE
#' )
#' 
#' # Create precipitation raster with diagnostic messages
#' precip_raster <- standardize_points_to_raster(
#'   points_df = weather_data,
#'   template_raster = my_template,
#'   value_col = "precipitation",
#'   fill_na = TRUE,
#'   verbose = TRUE
#' )
#'
#' @export
standardize_points_to_raster <- function(points_df,
                                         template_raster,
                                         value_col = "temperature",
                                         x_col = "x",
                                         y_col = "y",
                                         method = "cubicspline",
                                         fill_na = TRUE,
                                         verbose = FALSE) {
  
  # Validate that required columns exist in the dataframe
  required_cols <- c(x_col, y_col, value_col)
  missing_cols <- setdiff(required_cols, names(points_df))
  if (length(missing_cols) > 0) {
    stop(glue::glue("Missing required columns in points_df: {paste(missing_cols, collapse = ', ')}"))
  }
  
  # Check for NA values in coordinate columns
  if (any(is.na(points_df[[x_col]])) || any(is.na(points_df[[y_col]]))) {
    warning("NA values found in coordinate columns - these points will be excluded")
    points_df <- points_df[!is.na(points_df[[x_col]]) & !is.na(points_df[[y_col]]), ]
  }
  
  # Create SpatVector from points
  points <- terra::vect(points_df, geom = c(x_col, y_col), crs = terra::crs(template_raster))
  
  # Diagnostic checks (only if verbose)
  if (verbose) {
    message(glue::glue("Points CRS: {terra::crs(points)}"))
    message(glue::glue("Template CRS: {terra::crs(template_raster)}"))
    message(glue::glue("Points extent: {paste(as.vector(terra::ext(points)), collapse=', ')}"))
    message(glue::glue("Template extent: {paste(as.vector(terra::ext(template_raster)), collapse=', ')}"))
    message(glue::glue("Number of points: {nrow(points_df)}"))
    message(glue::glue("Non-NA values in {value_col}: {sum(!is.na(points_df[[value_col]]))}"))
    
    # Check if points overlap with template
    points_in_template <- terra::is.related(points, template_raster, "intersects")
    message(glue::glue("Points intersecting template: {sum(points_in_template)}"))
  }
  
  # Convert points to standardized raster grid
  # This is a two-step process to ensure exact template conformity:
  # 1. Create an initial rasterization from points (creates sparse raster)
  initial_rast <- terra::rasterize(points, template_raster, field = value_col)
  
  # Check if rasterization worked (only if verbose)
  if (verbose) {
    has_values <- !all(is.na(terra::values(initial_rast)))
    message(glue::glue("Rasterization successful (has values): {has_values}"))
  }
  
  # 2. Apply transform_raster to handle NA gaps and ensure perfect grid alignment
  # The fill_na parameter controls whether focal smoothing fills gaps before final interpolation
  result <- transform_raster(initial_rast,
                            template = template_raster,
                            method = method,
                            fill_na = fill_na,
                            verbose = verbose)
  
  # Store the original value column name as an attribute
  names(result) <- value_col
  
  return(result)
}
