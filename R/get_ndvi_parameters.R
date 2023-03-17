#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_ndvi_parameters <- function() {

  # total results was 229 on 2023-03-17, so setting max records to 500 should be more than sufficient
  
  url <- "https://catalogue.dataspace.copernicus.eu/resto/api/collections/Sentinel3/search.json?maxRecords=500&productType=SY_2_V10___&platform=S3A" 
  resp <- GET(url)
  out <- fromJSON(rawToChar(resp$content))
  return(out$features$properties)

}
