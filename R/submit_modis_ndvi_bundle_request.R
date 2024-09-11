#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_token
#' @param modis_ndvi_task_id_list
#' @return
#' @author Emma Mendelsohn
#' @export
submit_modis_ndvi_bundle_request <- function(modis_ndvi_token, 
                                             modis_ndvi_task_id_continent, 
                                             modis_ndvi_bundle_request_file) {
  
  # Extract current task id
  task_id <- modis_ndvi_task_id_continent$task_id
  
  # Check the previous bundle if it exits
  previous_bundle <- tryCatch(read_rds(modis_ndvi_bundle_request_file), error = function(e) NULL, warning = function(e) NULL)

  if(!is.null(previous_bundle) && previous_bundle$task_id == task_id) {
    bundle_response <- previous_bundle 
  } else {
    task_response <- httr::GET("https://appeears.earthdatacloud.nasa.gov/api/task", httr::add_headers(Authorization = modis_ndvi_token))
    task_response <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(task_response))) |> filter(task_id == !!task_id)
    task_status <- task_response |> pull(status) |> unlist()
    assertthat::assert_that(task_status %in% c("queued", "pending", "processing", "done"))
    
    if(task_status == "done") {
      # Fetch bundle response
      bundle_response <- httr::GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, sep = ""), httr::add_headers(Authorization = modis_ndvi_token))
      bundle_response <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(bundle_response)))
    } else {
      if(is.null(previous_bundle)) {
        stop(glue::glue("modis_ndvi bundle is {task_status} and no previous bundle has been recorded. \nRe-run pipeline later."))
      } else {
        message(glue::glue("modis_ndvi bundle is {task_status}. Proceeding with {previous_bundle$task_id} bundle."))
        bundle_response <- previous_bundle
      }
    }
  }
  
  # Record the bundle response to save time next time
  write_rds(bundle_response, modis_ndvi_bundle_request_file)
  
  # Extract files from bundle response
  bundle_response_files <- bundle_response$files |>
    mutate(file_type = unlist(file_type)) |>
    mutate(file_name = unlist(file_name)) |>
    mutate(file_id = unlist(file_id)) |>
    filter(file_type == "tif") |>
    mutate(task_id = task_id)
  
  # Return bundle response files
  return(bundle_response_files)
}
