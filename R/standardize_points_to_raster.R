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
#' @param fill_na Logical; if TRUE, NA values will be filled before transformation using focal operation. Default is FALSE.
#'
#' @return A SpatRaster object with the same properties as template_raster, filled with interpolated values from points_df
#'
#' @importFrom terra vect rasterize interpolate
#' @importFrom dplyr filter
#' @importFrom rlang sym
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
#' @export
standardize_points_to_raster <- function(points_df, 
                                         template_raster,
                                         value_col = "temperature",
                                         x_col = "x", 
                                         y_col = "y",
                                         method = "cubicspline",
                                         fill_na = FALSE) {
  
  # Create SpatVector from points
  points <- vect(points_df, geom = c(x_col, y_col), crs = crs(template_raster))
  
  # Fill in values through direct interpolation
  # This is a two-step process:
  # 1. Create an initial rasterization
  initial_rast <- rasterize(points, template_raster, field = value_col)
  
  # 2. Apply transform_raster with parameters
  result <- transform_raster(initial_rast, 
                             template = template_raster, 
                             method = method,
                             fill_na = fill_na)
  
  # Store the original value column name as an attribute
  names(result) <- value_col
  
  return(result)
}