#' Submit Request for MODIS NDVI Task by Continent
#'
#' This function submits a request for a MODIS NDVI task for a given continent.
#' It avoids duplications by verifying previous requests and takes into account
#' the current date for the request's end date. If overwrite is TRUE, it proceeds 
#' to submit a new request even if an existing valid task exists.
#'
#' @author Nathan Layman and Emma Mendelsohn
#'
#' @param end_date The end date for the MODIS data request.
#' @param modis_ndvi_token The token used to authenticate the MODIS NDVI request.
#' @param bbox_coords The bounding box coordinates for the continent.
#' @param modis_ndvi_transformed_directory The directory of the transformed MODIS NDVI data.
#' @param overwrite A flag indicating whether to overwrite/resubmit even if an existing valid task exists. Default is FALSE.
#'
#' @return A tibble containing the response of the task request, with added columns for country iso3c code and year.
#'
#' @note The function makes use of the httr::POST function to make the request. It also uses functions
#' from the arrow, dplyr, and lubridate packages to manipulate data.
#'
#' @examples
#' submit_modis_ndvi_task_request_continent(end_date = as.Date("2021-12-31"),
#' modis_ndvi_token = "your_token",
#' bbox_coords = c(-180, -90, 180, 90),
#' modis_ndvi_transformed_directory = "./modis_ndvi",
#' overwrite = TRUE)
#'
#' @export
submit_modis_ndvi_task_request_continent <- function(end_date,
                                                   modis_ndvi_token,
                                                   bbox_coords,
                                                   modis_ndvi_transformed_directory,
                                                   overwrite = FALSE) {
  task_name <- "africa"
  start_date <- floor_date(end_date, unit = "year")
  
  # create the task list
  task <- list(task_type = "area",
               task_name = task_name,
               startDate = start_date |> format("%m-%d-%Y"),
               endDate = end_date |> format("%m-%d-%Y"),
               layer = "MOD13A2.061,_1_km_16_days_NDVI",
               file_type = "geotiff",
               projection_name = "native",
               bbox = paste(bbox_coords, collapse = ","))
  
  # Check if a previous request already exists for that task in the task history
  current_tasks <- get_task_status_overview(modis_ndvi_token) |>
    dplyr::filter(startDate == task$startDate, endDate == task$endDate, crashed == FALSE) |>
    arrange(status) |>
    slice(1)
  
  # If existing valid task exists and overwrite is FALSE, reuse existing task
  # Otherwise, submit new request
  if(length(current_tasks$status) > 0 && current_tasks$status != "expired" && !overwrite) {
    task_response <- tibble(task_id = current_tasks$task_id, status = current_tasks$status)
  } else {
    # post the task request (either no existing task, expired task, or overwrite = TRUE)
    task_response <- httr::POST("https://appeears.earthdatacloud.nasa.gov/api/task", 
                               query = task, 
                               httr::add_headers(Authorization = modis_ndvi_token))
    task_response <- httr::content(task_response)
  }
  
  task_response$country_iso3c <- task_name
  task_response$year <- lubridate::year(start_date)
  task_response$end_date <- end_date
  
  return(as_tibble(task_response))
}
