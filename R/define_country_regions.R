#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
define_country_regions <- function() {
  
  tribble( ~"country", ~"region",
           "libya", "northern",
           "kenya", "eastern",
           "south africa", "southern", 
           "mauritania", "western",
           "niger", "western",
           "namibia", "southern",
           "madagascar", "eastern",
           "eswatini", "southern",
           "botswana" , "southern",
           "mayotte" , "eastern",
           "mali", "western",
           "tanzania", "eastern",
           "chad", "central",
           "sudan", "northern",
           "senegal","western" ) |> 
    mutate(iso2c = countrycode::countrycode(country,  origin = "country.name", destination = "iso2c")) |> 
    mutate(iso3c = countrycode::countrycode(country,  origin = "country.name", destination = "iso3c"))
  
  
}
