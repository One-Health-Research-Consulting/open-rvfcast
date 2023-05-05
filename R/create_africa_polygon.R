#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
create_africa_polygon <- function() {

 ne_countries(continent = "Africa", returnclass = "sf") |> 
    select(featurecla, country = name, country_iso3c = sov_a3)

}
