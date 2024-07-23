#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Whitney Bagge
#' @export
preprocess_glw <- function(glw_layer, bounding_boxes) {
  
  extent_object <- extent(bounding_boxes)
  glw_layer_out <- crop(glw_layer, extent_object)
  return(glw_layer_out)
  
}
