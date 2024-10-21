#' Retrieve and preprocess WAHIS RVF control measures data
#'
#' This function downloads the WAHIS (World Animal Health Information System) controls data for Rift Valley Fever, 
#' then filters it, and returns it as a dataframe.
#'
#' @author Emma Mendelsohn
#'
#' @return A dataframe containing the WAHIS controls data for Rift Valley Fever
#'
#' @note This function downloads data from "https://www.dolthub.com/csv/ecohealthalliance/wahisdb/main/wahis_six_month_controls", 
#' filters it for "rift valley fever" and returns the result. 
#'
#' @examples
#' get_wahis_rvf_controls_raw()
#'
#' @export
get_wahis_rvf_controls_raw <- function() {

  # Read full dataset into memory and filter for RVF
  wahis_controls <- read_csv("https://www.dolthub.com/csv/ecohealthalliance/wahisdb/main/wahis_six_month_controls") |>
    filter(standardized_disease_name == "rift valley fever")
  
  return(wahis_controls)
  

}
