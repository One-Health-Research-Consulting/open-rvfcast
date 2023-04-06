
#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_sentinel_ndvi_api_parameters <- function() {

  # Query using our bounding box for Central Africa - returns list of full Africa files
  # 229 results as of 2023-03-20, so max records of 500 is safe
  url <- "https://catalogue.dataspace.copernicus.eu/resto/api/collections/Sentinel3/search.json?maxRecords=500&productType=SY_2_V10___&platform=S3A&box=13.4,7.46,24.0,23.4" 
  resp <- GET(url)
  out <- fromJSON(rawToChar(resp$content))
  return(out$features)

}
