#' Request for MODIS NDVI Data Bundle
#'
#' This function handles the request for a MODIS NDVI data bundle by making an API call to the APPEEARS. If the call is 
#' successful and the task status is 'done', a data bundle is returned. If the task is not 'done', the function can also 
#' use a previously saved bundle, if available and requested. 
#'
#' @author Emma Mendelsohn
#'
#' @param modis_ndvi_token The token used to authenticate the API request.
#' @param modis_ndvi_task_id_continent The task id for the continent for which the data is requested.
#' @param modis_ndvi_bundle_request_file The file where the data bundle request information is stored.
#' @param ... Additional function arguments.
#'
#' @return A data bundle in response to the API data request.
#'
#' @note The function first checks if there is a previous bundle that can be used if 'overwrite' is set to FALSE. If not
#' or if 'overwrite' is set to TRUE, an API request is made. If the task status from the API response is 'done', a new
#' data bundle is returned. If the task status is not 'done' and a previous data bundle exists, the old bundle is 
#' returned. If no old bundle exists, the function stops. ALSO appears returns files for more than just ndvi such
#' as vegatation index quality. Those are discarded retaining only NDVI
#'
#' @examples
#' response <- submit_modis_ndvi_bundle_request(modis_ndvi_token="token", 
#'                      modis_ndvi_task_id_continent="task_id", 
#'                      modis_ndvi_bundle_request_file="file_path", 
#'                      overwrite=FALSE)
#'
#' @export
submit_modis_ndvi_bundle_request <- function(modis_ndvi_token, 
                                             modis_ndvi_task_id_continent, 
                                             retry_time = 5, # in seconds
                                             ...) {
  
  # Extract current task id
  task_id <- modis_ndvi_task_id_continent$task_id
  
  assertthat::are_equal(nrow(modis_ndvi_task_id_continent), 1)
  
  task_status = "pending"
  start_time <- Sys.time()
  i = 0
  while(task_status != "done") {
    
    task_response <- httr::GET("https://appeears.earthdatacloud.nasa.gov/api/task", httr::add_headers(Authorization = modis_ndvi_token))
    task_response <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(task_response))) |> dplyr::filter(task_id == !!task_id)
    task_status <- task_response |> pull(status) |> unlist()
    assertthat::assert_that(task_status %in% c("queued", "pending", "processing", "done"))
    
    elapsed_minutes <- difftime(Sys.time(), start_time, units = "mins")[[1]] |> round(2)
    cat(glue::glue("  Fetching MODIS NDVI task details for year {modis_ndvi_task_id_continent$year}. Task {task_id} is '{task_status}' with {elapsed_minutes} minutes elapsed.        \r"))
    
    if(i > 1000) stop(glue::glue("MODIS NDVI task {task_id} timed out."))
    
    Sys.sleep(retry_time)
    i <- i + 1
  }
  message(" ")
  
  bundle_response <- httr::GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, sep = ""), 
                               httr::add_headers(Authorization = modis_ndvi_token))
  
  bundle_response <- jsonlite::fromJSON(jsonlite::toJSON(httr::content(bundle_response))) |> 
    bind_cols() |> 
    dplyr::filter(file_type == "tif", grepl("NDVI", file_name)) |>
    mutate(year_doy = sub(".*doy(\\d+).*", "\\1", file_name),
           start_date = as.Date(year_doy, format = "%Y%j"),
           year = year(start_date)) |> # confirmed this is start date through manual download test
    arrange(start_date) |> 
    dplyr::filter(year == modis_ndvi_task_id_continent$year) # Ensure we're not getting stuff outside the requested year which might lead to dupes
  
  # Return bundle response files
  return(bundle_response)
}

get_current_task_status <- function(modis_ndvi_token) {
  task_status <- httr::GET("https://appeears.earthdatacloud.nasa.gov/api/status", httr::add_headers(Authorization = modis_ndvi_token)) |>
    httr::stop_for_status() |> 
    httr::content() 
  
  task_status |> 
    pluck(1) |> 
    pluck("progress") |> 
    pluck("details") |> 
    bind_rows()
}

get_task_status_overview <- function(modis_ndvi_token) {
  task_overview <- GET("https://appeears.earthdatacloud.nasa.gov/api/task", add_headers(Authorization = modis_ndvi_token)) |>
    httr::stop_for_status() |>
    httr::content() |>
    map_dfr(~bind_cols(.x)) |>
    suppressMessages()
  
  task_overview
}

delete_appears_task <- function(modis_ndvi_token, task_id) {
  response <- httr::DELETE(paste("https://appeears.earthdatacloud.nasa.gov/api/task/", task_id, sep = ""), httr::add_headers(Authorization = modis_ndvi_token))
  response$status_code
}

delete_crashed_tasks <- function(modis_ndvi_token) {
  crashed_tasks <- get_task_status_overview(modis_ndvi_token) |> dplyr::filter(crashed == TRUE) |> pull(task_id)
  
  map(crashed_tasks, ~delete_appears_task(modis_ndvi_token, .x))
}

delete_all_current_tasks <- function(modis_ndvi_token) {
  current_tasks <- get_task_status_overview(modis_ndvi_token) |> dplyr::filter(status %in% c("queued", "pending", "processing")) |> pull(task_id)
  
  map(current_tasks, ~delete_appears_task(modis_ndvi_token, .x))
}


