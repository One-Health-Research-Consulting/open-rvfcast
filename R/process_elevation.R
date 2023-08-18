#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Whitney Bagge
#' @export
process_elevation <- function(elevation_layer_raw, bounding_boxes) {
  
  extent_object <- extent(bounding_boxes)
  elevation_layer_raw_out <- crop(elevation_layer_raw, extent_object)
  return(elevation_layer_raw_out)
  
}
