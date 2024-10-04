#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_start_year
#' @param modis_ndvi_end_year
#' @param modis_ndvi_token
#' @param bbox_coords
#' @return
#' @author Emma Mendelsohn
#' @export
submit_modis_ndvi_task_request_continent <- function(modis_ndvi_start_year,
                                                     modis_ndvi_token,
                                                     bbox_coords) {

  task_name <- "africa"
  
  # 1 month lag from current.
  endDate <- Sys.Date() %m-% months(1) |> format("%m-%d-%Y")
  
  # create the task list
  task <- list(task_type = "area", 
               task_name = task_name, 
               startDate = paste0("01-01-", modis_ndvi_start_year), 
               endDate = endDate,
               layer =  "MOD13A2.061,_1_km_16_days_NDVI", 
               file_type = "geotiff", 
               projection_name = "native", 
               bbox = paste(bbox_coords, collapse = ","))
  
  # post the task request
  task_response <- httr::POST("https://appeears.earthdatacloud.nasa.gov/api/task", query = task, httr::add_headers(Authorization = modis_ndvi_token))
  task_response <- httr::content(task_response)
  task_response$country_iso3c <- task_name
  
  return(task_response)
}
