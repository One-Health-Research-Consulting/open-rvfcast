#' Retrieve WAHIS Rift Valley Fever Outbreaks Data
#'
#' This function downloads and filters the WAHIS Rift Valley Fever outbreaks data from 
#' the provided source and returns a filtered dataframe. 
#'
#' @author Emma Mendelsohn
#'
#' @return A dataframe containing the filtered WAHIS Rift Valley Fever outbreaks data.
#'
#' @note This function performs no input arguments. It directly downloads and 
#' filter the dataset for "rift valley fever" outbreaks.
#'
#' @examples
#' get_wahis_rvf_outbreaks_raw()
#'
#' @export
get_wahis_rvf_outbreaks <- function() {
  
  # Read full dataset into memory and filter for RVF
  wahis_outbreaks <- read_csv("https://www.dolthub.com/csv/ecohealthalliance/wahisdb/main/wahis_outbreaks") |>
    filter(standardized_disease_name == "rift valley fever")
  
  wahis_outbreaks <- preprocess_wahis_rvf_outbreaks(wahis_outbreaks)

  return(wahis_outbreaks)
  
  # Below is archived code for retrieving data via SQL query through dolthub API
  
  # # intialize API download parameters
  # offset <- 0 
  # limit <- 200
  # outbreaks <- tibble()
  # 
  # # Repeat the query until all data is downloaded
  # while(TRUE) {
  #   
  #   # Set the url call
  #   url <- wahis_rvf_query(offset)
  # 
  #   headers <- add_headers("authorization" = glue::glue("token {Sys.getenv('DOLTHUB_API_KEY')}"))
  #   
  #   # Make the API request
  #   res <- RETRY("GET", url = url,  encode = "json", times = 3)
  #   
  #   # Check if the request was successful
  #   if (res$status_code != 200) {
  #     stop("API request failed with status code: ", res$status_code)
  #   }
  #   
  #   # Parse the JSON response
  #   dat <- fromJSON(content(res, as = "text"))
  #   
  #   # Add to the dataframe
  #   outbreaks <- bind_rows(outbreaks, dat$rows)
  #   
  #   # Increment the offset
  #   offset <- offset + limit
  #   
  #   Sys.sleep(1)
  #   
  #   # Check if all data has been downloaded
  #   if (nrow(dat$rows) < limit) {
  #     break
  #   }
  # }
  
}

# function to run query with variable offset
# wahis_rvf_query <- function(offset){
#   
#   endpoint <- "https://www.dolthub.com/api/v1alpha1/ecohealthalliance/wahisdb/main"
#   query <- glue::glue("SELECT *
#                       FROM `wahis_outbreaks`
#                       WHERE standardized_disease_name = 'rift valley fever'
#                       ORDER BY epi_event_id_unique
#                       LIMIT 200
#                       OFFSET {offset}")
#   
#   url <- param_set(endpoint, key = "q", value = url_encode(query)) 
#   return(url)
# }
