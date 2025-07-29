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
  
  inital_rast <- points_df |> 
    select(any_of(c(x_col, y_col, value_col))) |> rast(type = "xyz", crs = "EPSG:4326")
  
  # 2. Apply transform_raster to handle NA gaps and ensure perfect grid alignment
  # The fill_na parameter controls whether focal smoothing fills gaps before final interpolation
  result <- transform_raster(inital_rast,
                            template = template_raster,
                            method = method,
                            fill_na = fill_na,
                            verbose = verbose)
  
  # Store the original value column name as an attribute
  names(result) <- value_col
  
  return(result)
}
