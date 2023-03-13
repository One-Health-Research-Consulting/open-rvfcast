#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
define_bounding_boxes <- function(country_regions) {
  
  data(countries_bbox, package = "cartographer")
  
  bounding_boxes <- left_join(country_regions, countries_bbox, by = c("iso2c" = "iso")) |> 
    group_by(region) |> 
    summarize(across(c(x_min, y_min), ~min(., na.rm = TRUE)),
              across(c(x_max, y_max), ~max(., na.rm = TRUE)))
  

  return(bounding_boxes)
  
}