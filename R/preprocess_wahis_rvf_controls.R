#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wahis_rvf_controls_raw
#' @return
#' @author Emma Mendelsohn
#' @export
preprocess_wahis_rvf_controls <- function(wahis_rvf_controls_raw) {
  
  
  wahis_rvf_controls <- wahis_rvf_controls_raw |> 
    mutate(country = recode(country, "central african (rep.)" = "central african republic",
                            default = country)) |> 
    mutate(iso_code = countrycode::countrycode(country, origin = "country.name", destination = "iso3c")) |> 
    mutate(continent = countrycode::countrycode(country, origin = "country.name", destination = "continent")) |> 
    filter(continent == "Africa")  
  
  return(wahis_rvf_controls)
  
}
