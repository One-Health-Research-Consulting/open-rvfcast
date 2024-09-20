#' Get a tibble of tasks from ECMWF API
#'
#' This function retrieves tasks from the ECMWF (European Centre for Medium-Range Weather Forecasts) API.
#' It sends a GET request using the `httr` package and returns the content of the response as a data frame.
#'
#' @author Nathan Layman
#'
#' @param url A character string specifying the base URL for the ECMWF CDS tasks API. 
#'            Defaults to "https://cds.climate.copernicus.eu/api/v2/tasks/".
#'
#' @return A data frame with the tasks retrieved from ECMWF.
#' 
#' @export
get_ecwmf_tasks <- function(url = "https://cds.climate.copernicus.eu/api/v2/tasks/") {
  response <- httr::GET(
    url,
    httr::authenticate(Sys.getenv("ECMWF_USERID"), Sys.getenv("ECMWF_TOKEN"))
  )
  
  httr::content(response) |> bind_rows()
}


#' Clear ECMWF Queued Tasks
#'
#' This function retrieves the currently queued tasks from the ECMWF (European Centre for Medium-Range Weather Forecasts) CDS (Climate Data Store)
#' API and deletes them using their request IDs.
#'
#' @author Nathan Layman
#'
#' @param url A character string specifying the base URL for the ECMWF CDS tasks API. 
#'            Defaults to "https://cds.climate.copernicus.eu/api/v2/tasks/".
#'
#' @return A data frame with the tasks retrieved from ECMWF.
#' 
#' @export
clear_ecwmf_tasks <- function(url = "https://cds.climate.copernicus.eu/api/v2/tasks/") {
  
  tasks_to_clear <- get_ecwmf_tasks() |> 
    filter(state == "queued") |> 
    mutate(request_id = paste0(url, request_id)) |>
    pull(request_id)
  
  request <- walk(tasks_to_clear, ~httr::DELETE(.x, httr::authenticate(Sys.getenv("ECMWF_USERID"), Sys.getenv("ECMWF_TOKEN"))))

}

submit_ecwmf_request <- function(request, 
                                 url = "https://cds.climate.copernicus.eu/api/v2/tasks/") {
  
 response <- httr::PUT(jsonlite::toJSON(request), httr::authenticate(Sys.getenv("ECMWF_USERID"), Sys.getenv("ECMWF_TOKEN")))
  
}
response <- POST(
  url = paste0(ecmwf_url, "/datasets/data/era5"),  # Update endpoint as needed
  add_headers(
    Authorization = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  ),
  body = toJSON(query),  # Convert the query to JSON format
  encode = "json"  # Encode the body as JSON
)

# Check if the request was successful
if (status_code(response) == 200) {
  # Save the response content to the desired file
  writeBin(content(response, "raw"), "output.nc")
  cat("File saved successfully.\n")
} else {
  cat("Failed to fetch data. Status code:", status_code(response), "\n")
}