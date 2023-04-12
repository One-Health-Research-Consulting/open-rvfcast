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
    rowwise() |> 
    group_split() |> 
    map_dfr(function(x){
      tibble(country = x$country, 
             country_iso3c = x$country_iso3c,
             bounding_box = list(sf::st_bbox(x)))
      
    })


}
