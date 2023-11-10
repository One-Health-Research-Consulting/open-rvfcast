#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_wahis_rvf_controls_raw <- function() {

  # Read full dataset into memory and filter for RVF
  wahis_controls <- read_csv("https://www.dolthub.com/csv/ecohealthalliance/wahisdb/main/wahis_six_month_controls") |>
    filter(standardized_disease_name == "rift valley fever")
  
  return(wahis_controls)
  

}
