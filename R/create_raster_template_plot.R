#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @param continent_polygon
#' @return
#' @author Emma Mendelsohn
#' @export
create_raster_template_plot <- function(continent_raster_template =
                                        rast(continent_raster_template),
                                        continent_polygon) {

  values(continent_raster_template) <- 1:ncell(continent_raster_template)
  
  # Extract continent_polygon, first crop by bounding box, then mask the raster
  # Since terra doesn't play nice with `sf` yet we need to convert the objects
  # to spatial data frames, which we do in-operation using `as()`
  continent_raster_template <- crop(continent_raster_template, as(continent_polygon, "Spatial"))
  continent_raster_template <- mask(continent_raster_template, continent_polygon)
  plot(continent_raster_template, main = "Raster Template", legend=FALSE)
  plot(continent_polygon, col = NA, add = TRUE)
  
}
