#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
create_africa_polygon <- function() {
  
  # Get all countries as sf
  africa_countries <- ne_countries(scale = "medium", returnclass = "sf") |>
    dplyr::filter(continent == "Africa" | 
             name %in% c("Seychelles", "Mauritius", "Comoros", "Cape Verde", 
                         "São Tomé and Príncipe")) |>
    select(country = name, country_iso3c = iso_a3, geometry)
  
  africa_countries

}
