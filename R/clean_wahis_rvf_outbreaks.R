#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wahis_rvf_outbreaks_raw
#' @return
#' @author Emma Mendelsohn
#' @export
clean_wahis_rvf_outbreaks <- function(wahis_rvf_outbreaks_raw) {

  wahis_rvf_outbreaks_raw$continent <- countrycode::countrycode(wahis_rvf_outbreaks_raw$country_iso3c, origin = "iso3c", destination = "continent")
  wahis_rvf_outbreaks <- wahis_rvf_outbreaks_raw |> filter(continent == "Africa") |> filter(cases_per_interval > 0)
  
  regions <- tribble( ~"country", ~"region",
                      "Libya", "Northern",
                      "Kenya", "Eastern",
                      "South Africa", "Southern", 
                      "Mauritania", "Western",
                      "Niger", "Western",
                      "Namibia", "Southern",
                      "Madagascar", "Eastern",
                      "Eswatini", "Southern",
                      "Botswana" , "Southern",
                      "Mayotte" , "Eastern",
                      "Mali", "Western",
                      "Tanzania", "Eastern",
                      "Chad", "Central",
                      "Sudan", "Northern",
                      "Senegal","Western" )
  
  wahis_rvf_outbreaks <- left_join(wahis_rvf_outbreaks, regions)
  
  return(wahis_rvf_outbreaks)

}
