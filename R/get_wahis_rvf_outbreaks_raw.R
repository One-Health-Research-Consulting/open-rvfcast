#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_wahis_rvf_outbreaks_raw <- function() {

  # intialize API download parameters
  offset <- 0 
  limit <- 200
  outbreaks <- tibble()
  
  # Repeat the query until all data is downloaded
  while(TRUE) {
    
    # Set the url call
    url <- wahis_rvf_query(offset)
    
    headers <- add_headers("authorization" = glue::glue("token {Sys.getenv('DOLTHUB_API_KEY')}"))
    
    # Make the API request
    res <- RETRY("POST", url = url, headers = headers, encode = "json", times = 3)
    
    # Check if the request was successful
    if (res$status_code != 200) {
      stop("API request failed with status code: ", res$status_code)
    }
    
    # Parse the JSON response
    dat <- fromJSON(content(res, as = "text"))
    
    # Add to the dataframe
    outbreaks <- bind_rows(outbreaks, dat$rows)
    
    # Increment the offset
    offset <- offset + limit
    
    Sys.sleep(1)
    
    # Check if all data has been downloaded
    if (nrow(dat$rows) < limit) {
      break
    }
  }
  
  return(outbreaks)

}

# function to run query with variable offset
wahis_rvf_query <- function(offset){
  
  endpoint <- "https://www.dolthub.com/api/v1alpha1/ecohealthalliance/wahisdb/main"
  query <- glue::glue(
    "SELECT we.*, wo.*
FROM wahis_epi_events we
JOIN wahis_outbreaks wo 
ON wo.epi_event_id_unique = we.epi_event_id_unique 
WHERE we.standardized_disease_name = 'rift valley fever'
ORDER BY we.epi_event_id_unique
LIMIT 200
OFFSET {offset}")
  
  url <- param_set(endpoint, key = "q", value = url_encode(query)) 
  return(url)
}
