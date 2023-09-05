#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
submit_modis_ndvi_task_request <- function(modis_ndvi_start_year, 
                                           modis_ndvi_end_year, 
                                           modis_ndvi_token,
                                           country_bounding_boxes) {
  
  task_name <- country_bounding_boxes$country_iso3c
  bbox_coords <- unlist(country_bounding_boxes$bounding_box)

  # create the task list
  task <- list(task_type = "area", 
               task_name = task_name, 
               startDate = paste0("01-01-", modis_ndvi_start_year), 
               endDate = paste0("12-31-", modis_ndvi_end_year),
               layer =  "MOD13A2.061,_1_km_16_days_NDVI", 
               file_type = "geotiff", 
               projection_name = "native", 
               bbox = paste(bbox_coords, collapse = ","))
  
  # post the task request
  task_response <- POST("https://appeears.earthdatacloud.nasa.gov/api/task", query = task, add_headers(Authorization = modis_ndvi_token))
  task_response <- content(task_response)
  task_response$country_iso3c <- task_name
  
  return(list(task_response))

}
