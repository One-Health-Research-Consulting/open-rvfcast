#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param countries
#' @return
#' @author Emma Mendelsohn
#' @export
create_country_polygons <- function(countries, states) {

  country_polygons <- rnaturalearth::ne_countries(country = countries, returnclass = "sf")
  assertthat::assert_that(nrow(country_polygons) == length(countries))
  country_polygons <- country_polygons |> 
    select(featurecla, country = name, country_iso3c = sov_a3)
  
  state_polygons <- states |>
    rowwise() |> 
    group_split() |> 
    map_dfr(function(x){
      rnaturalearth::ne_states(country = x$country, returnclass = "sf") |> 
        filter(name == x$state)
    })
  assertthat::assert_that(nrow(states) == nrow(state_polygons))
  state_polygons <- state_polygons |> 
    select(featurecla, country = name, country_iso3c = sov_a3)
  
  polygons <- bind_rows(country_polygons, state_polygons)
  
  return(polygons)
  
}
