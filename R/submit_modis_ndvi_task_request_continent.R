#' Submit Request for MODIS NDVI Task by Continent
#'
#' This function submits a request for a MODIS NDVI task for a given continent. 
#' It avoids duplications by verifying previous requests and takes into account 
#' the current date for the request's end date. If overwrite is TRUE or the 
#' modis_year is the current year, it proceeds to submit or overwrite the request.
#'
#' @author Nathan Layman and Emma Mendelsohn
#'
#' @param modis_year The year for the MODIS data.
#' @param modis_ndvi_token The token used to authenticate the MODIS NDVI request.
#' @param bbox_coords The bounding box coordinates for the continent.
#' @param modis_ndvi_transformed_directory The directory of the transformed MODIS NDVI data.
#' @param overwrite A flag indicating whether to overwrite the existing file.
#'
#' @return A tibble containing the response of the task request, with added columns for country iso3c code and year.
#'
#' @note The function makes use of the httr::POST function to make the request. It also uses functions 
#'       from the arrow, dplyr, and lubridate packages to manipulate data.
#'
#' @examples
#' submit_modis_ndvi_task_request_continent(modis_year = 2021, 
#'            modis_ndvi_token = "your_token", 
#'            bbox_coords = c(-180, -90, 180, 90), 
#'            modis_ndvi_transformed_directory = "./modis_ndvi",
#'            overwrite = TRUE)
#'
#' @export
submit_modis_ndvi_task_request_continent <- function(end_date,
                                                     modis_ndvi_token,
                                                     bbox_coords,
                                                     modis_ndvi_transformed_directory) {
    
  task_name <- "africa"

  start_date <- floor_date(end_date, unit = "year")
  
  # create the task list
  task <- list(task_type = "area", 
               task_name = task_name, 
               startDate = start_date |> format("%m-%d-%Y"), 
               endDate = end_date |> format("%m-%d-%Y"),
               layer =  "MOD13A2.061,_1_km_16_days_NDVI", 
               file_type = "geotiff", 
               projection_name = "native", 
               bbox = paste(bbox_coords, collapse = ","))

  # Check if a previous request already exists for that task in the task history
  current_tasks <- get_task_status_overview(modis_ndvi_token) |> 
    filter(startDate == task$startDate, endDate == task$endDate, crashed == FALSE) |>
    arrange(status) |>
    slice(1)
    
  if(nrow(current_tasks)) {
    task_response <- tibble(task_id = current_tasks$task_id, status = current_tasks$status)
    } else {
      # post the task request
      task_response <- httr::POST("https://appeears.earthdatacloud.nasa.gov/api/task", query = task, httr::add_headers(Authorization = modis_ndvi_token))
      task_response <- httr::content(task_response)
  }
  
  task_response$country_iso3c <- task_name
  task_response$year <- lubridate::year(start_date)
  task_response$end_date <- end_date
  
  return(as_tibble(task_response))
}
