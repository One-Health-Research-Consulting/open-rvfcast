#' Retrieve Sentinel-3 data from Copernicus Open Access Hub 
#'
#' This function retrieves Sentinel-3 data from the Copernicus Open Access Hub. The function
#' constructs a URL string based on the given parameters and sends a GET request to the server 
#' to download the data. The data is received in JSON format and then converted to a list in R.
#' The function returns a list of features from the Sentinel-3 dataset.
#'
#' @author Emma Mendelsohn
#'
#' @param 
#'
#' @return A list. Each element of the list corresponds to a feature from the Sentinel-3 dataset.
#'
#' @note This function retrieves satellite data using the HTTP protocol 
#' from the Copernicus Open Access Hub. 
#'
#' @examples
#' #This function doesn't require any arguments.
#' get_sentinel_ndvi_api_parameters()
#'
#' @export
get_sentinel_ndvi_api_parameters <- function() {

  # Query using an arbitrary bounding box from Central Africa - returns list of full Africa files
  # 229 results as of 2023-03-20, so max records of 500 is safe
  url <- "https://catalogue.dataspace.copernicus.eu/resto/api/collections/Sentinel3/search.json?maxRecords=500&productType=SY_2_V10___&platform=S3A&box=13.4,7.46,24.0,23.4&timeliness=NT" 
  resp <- httr::GET(url)
  out <- jsonlite::fromJSON(rawToChar(resp$content)) |> 
    pluck("features") |>
    arrange(properties$startDate) |>
    mutate(start_date = lubridate::floor_date(lubridate::as_date(properties$startDate), unit = "day"), 
           end_date = lead(start_date - 1))
  
  out$end_date[nrow(out)] <- lubridate::floor_date(lubridate::as_date(out$properties$completionDate[nrow(out)]), unit = "day")
  
  return(out)

}
