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
    
    # Make the API request
    res <- RETRY("POST", url, encode = "json", times = 3)
    
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
    "SELECT ts.*, ob.outbreak_thread_id, ob.country, ob.country_iso3c, ob.disease, ob.duration_in_days, ob.total_cases_per_outbreak 
FROM outbreak_summary ob 
JOIN outbreak_time_series ts 
ON ob.outbreak_thread_id=ts.outbreak_thread_id 
WHERE disease = 'rift valley fever'
LIMIT 200
OFFSET {offset}")
  
  url <- param_set(endpoint, key = "q", value = url_encode(query)) 
  return(url)
}
