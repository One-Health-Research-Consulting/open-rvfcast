#' Preprocess and Filter WAHIS RVF Outbreak Data
#'
#' This function preprocesses the raw WAHIS RVF outbreak data and filters it to include only African data.
#' The data is then selected and returned with the proper ISO codes for the countries.
#'
#' @author Emma Mendelsohn
#'
#' @param wahis_rvf_outbreaks_raw Raw WAHIS data including RVF outbreaks. 
#'
#' @return A dataframe of filtered and preprocessed WAHIS RVF outbreak data, including only African data with proper country ISO codes.
#'
#' @note This function will only process African data and will update country codes to proper ISO codes.
#'
#' @examples
#' preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw = raw_data)
#'
#' @export
preprocess_wahis_rvf_outbreaks <- function(wahis_rvf_outbreaks_raw) {

  wahis_rvf_outbreaks_raw$continent <- countrycode::countrycode(wahis_rvf_outbreaks_raw$country_unique_code, origin = "iso3c", destination = "continent")
  wahis_rvf_outbreaks <- wahis_rvf_outbreaks_raw |> 
    filter(continent == "Africa")  |> 
    mutate(iso_code = toupper(country_unique_code)) |> 
    select(-country_unique_code)
  
  return(wahis_rvf_outbreaks)

}
