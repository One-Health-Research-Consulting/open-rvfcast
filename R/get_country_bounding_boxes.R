#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param country_polygons
#' @return
#' @author Emma Mendelsohn
#' @export
get_country_bounding_boxes <- function(country_polygons) {
  
  country_polygons |> 
    mutate(bounding_box = map(geometry, sf::st_bbox)) |>
    sf::st_drop_geometry() |>  # Remove geometry column to get regular data frame
    select(country, country_iso3c, bounding_box)

}
