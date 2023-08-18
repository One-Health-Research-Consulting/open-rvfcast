#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Whitney Bagge
#' @export
preprocess_glw <- function(bounding_boxes) {
  
  taxa_tif <- c("url_cattle.tif","url_sheep.tif","url_goats.tif")
  
  for(i in 1:length(taxa_tif)) { 
  extent_object <- raster::extent(bounding_boxes)
  glw_layer_out <- raster::crop(taxa_tif, extent_object)
  return(glw_layer_out)
  
  }
}
