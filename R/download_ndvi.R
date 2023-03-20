#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndvi_api_parameters
#' @return
#' @author Emma Mendelsohn
#' @export
download_ndvi <- function(ndvi_api_parameters, download_directory) {
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
  id <- ndvi_api_parameters$id
  filename <- ndvi_api_parameters$properties$title
  
  message(paste0("Downloading ", filename))
  
  if(filename %in% existing_files) {
    message("file already exists, skipping download")
    return(file.path(download_directory, filename)) # skip if file exists
  }  
  
  # TODO set to refresh
  # auth_header <- paste("Bearer", Sys.getenv("KEYCLOAK_TOKEN"), sep = " ")
  
  url <- glue::glue("http://catalogue.dataspace.copernicus.eu/odata/v1/Products({id})/$value")
  file <- here::here(download_directory, glue::glue("{filename}.zip"))
  response <- GET(url, add_headers(Authorization = auth_header), write_disk(filename, overwrite = TRUE))
  
  # unzip, and keep only the ndvi.nc?
  
}
