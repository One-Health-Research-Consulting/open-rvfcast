#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wahis_rvf_outbreaks_raw
#' @return
#' @author Emma Mendelsohn
preprocess_wahis_rvf_outbreaks <- function(wahis_rvf_outbreaks_raw, country_regions) {

  wahis_rvf_outbreaks_raw$continent <- countrycode::countrycode(wahis_rvf_outbreaks_raw$country_iso3c, origin = "iso3c", destination = "continent")
  wahis_rvf_outbreaks <- wahis_rvf_outbreaks_raw |> filter(continent == "Africa") |> filter(cases_per_interval > 0) # wahis will record zero outbreaks in locations that previously had cases within a given thread
  
  wahis_rvf_outbreaks <- left_join(wahis_rvf_outbreaks, country_regions, by = "country") |> 
    select(unique_id, source, outbreak_thread_id, outbreak_location_id, names(country_regions), everything())

  return(wahis_rvf_outbreaks)

}
