#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_token
#' @param modis_ndvi_task_request
#' @return
#' @author Emma Mendelsohn
#' @export
submit_modis_ndvi_bundle_request <- function(modis_ndvi_token, modis_ndvi_task_request, timeout = 1000) {
  
  # Get the task ID
  modis_ndvi_request_task_id <- fromJSON(modis_ndvi_task_request)$task_id
  
  # Get sys time for the loop timeout
  sys_start_time <- Sys.time()
  
  # Function to check task status
  check_task_status <- function() {
    task_response <- GET("https://appeears.earthdatacloud.nasa.gov/api/task", add_headers(Authorization = modis_ndvi_token))
    task_response <- fromJSON(toJSON(content(task_response))) |>  filter(task_id == modis_ndvi_request_task_id)
    task_status <- task_response |> pull(status) |> unlist()
    assertthat::assert_that(task_status %in% c("pending", "processing", "done"))
    return(task_status)
  }
  
  # Check task status in a loop
  task_status <- ""
  while (task_status != "done") {
    task_status <- check_task_status()

    # Print current task status
    message(paste("task status:", task_status))
    
    # Check timeout
    elapsed_time <- difftime(Sys.time(), sys_start_time, units = "secs")
    if (task_status != "done" & elapsed_time >= timeout) {
      message("timeout reached")
      break
    }

    # Sleep for a few seconds before checking again
    if(task_status != "done"){
      message("pausing 60 seconds before checking again")
      Sys.sleep(60)
    }
  }
  
  bundle_response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", modis_ndvi_request_task_id, sep = ""), add_headers(Authorization = modis_ndvi_token))
  bundle_response <- fromJSON(toJSON(content(bundle_response))) 
  
  return(bundle_response)
  
}
