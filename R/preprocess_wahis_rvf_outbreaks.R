#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wahis_rvf_outbreaks_raw
#' @return
#' @author Emma Mendelsohn
preprocess_wahis_rvf_outbreaks <- function(wahis_rvf_outbreaks_raw) {

  wahis_rvf_outbreaks_raw$continent <- countrycode::countrycode(wahis_rvf_outbreaks_raw$iso_code, origin = "iso3c", destination = "continent")
  wahis_rvf_outbreaks <- wahis_rvf_outbreaks_raw |> 
    filter(continent == "Africa")  |> 
    mutate(iso_code = toupper(iso_code))
  
  return(wahis_rvf_outbreaks)

}
