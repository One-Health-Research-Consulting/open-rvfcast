#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_ndvi_bundle_request
#' @param download_directory
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
download_modis_ndvi <- function(modis_ndvi_token,
                                modis_ndvi_bundle_request,
                                download_directory,
                                overwrite = FALSE) {
  

  existing_files <- list.files(download_directory)

  task_id <- unique(modis_ndvi_bundle_request$task_id)
  file_id <- modis_ndvi_bundle_request$file_id
  filename <- basename(modis_ndvi_bundle_request$file_name)
  
  message(paste0("Downloading ", filename))

  if(filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(download_directory, filename))
  }

  # Write the file to disk
  response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, '/', file_id, sep = ""),
                  write_disk(file.path(download_directory, filename), overwrite = TRUE), progress(), add_headers(Authorization = modis_ndvi_token))


  return(file.path(download_directory, filename))
  
}
