#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_api_parameters
#' @param download_directory
#' @return
#' @author Emma Mendelsohn
#' @export
download_modis_ndvi <- function(modis_ndvi_api_parameters, download_directory =
                                "data/modis_ndvi_rasters") {
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
  download_filename <- modis_ndvi_api_parameters$id

  message(paste0("Downloading ", download_filename))
  
  if(save_filename %in% existing_files) {
    message("file already exists, skipping download")
    return(file.path(download_directory, save_filename)) # skip if file exists
  }  
  
  
  url <- paste0("/vsicurl/", modis_ndvi_api_parameters$url)
  
  data <- rast(url)
  
  # save here - best approach?
  # aggregate tiles by region and date
  
  return(file.path(download_directory, save_filename))
  

}
