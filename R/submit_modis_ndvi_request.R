#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
submit_modis_ndvi_request <- function(modis_ndvi_start_year, modis_ndvi_end_year, continent_bounding_box, modis_ndvi_token) {

  token <- modis_ndvi_token
  
  # create the task list
  task <- list(task_type = "area", 
               task_name = "modis_ndvi_africa", 
               startDate = paste0("01-01-", modis_ndvi_start_year), 
               endDate = paste0("12-31-", modis_ndvi_end_year),
               layer =  "MOD13A2.061,_1_km_16_days_NDVI", 
               file_type = "geotiff", 
               projection_name = "native", 
               bbox = paste(continent_bounding_box, collapse = ","))
  
  # post the task request
  task_response <- POST("https://appeears.earthdatacloud.nasa.gov/api/task", query = task, add_headers(Authorization = token))
  task_response <- prettify(toJSON(content(task_response), auto_unbox = TRUE))
  
  # get the associated bundle
  task_id <- fromJSON(task_response)$task_id
  bundle_response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, sep = ""), add_headers(Authorization = token))
 
  #TODO  in progress
  bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
  bundle_response <- fromJSON(bundle_response)
  bundle_response$files$file_id

}
