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
  download_filename <- tools::file_path_sans_ext(ndvi_api_parameters$properties$title)
  start_date <- str_extract(filename, "(\\d{8}T\\d{6})")
  end_date <- str_extract(filename, "(?<=_)(\\d{8}T\\d{6})(?=_\\w{6}_)")
  save_filename <- paste0("NDVI_Africa_",start_date, "_to_", end_date, ".nc")
  
  message(paste0("Downloading ", download_filename))
  
  if(save_filename %in% existing_files) {
    message("file already exists, skipping download")
    return(file.path(download_directory, save_filename)) # skip if file exists
  }  
  
  # auth
  auth <- POST("https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token", 
                   body = list(
                     grant_type = "password",
                     username = Sys.getenv("COPERNICUS_USERNAME"),
                     password =  Sys.getenv("COPERNICUS_PASSWORD"),
                     client_id = "cdse-public"
                   ), 
                   encode = "form")
  
  url <- glue::glue("http://catalogue.dataspace.copernicus.eu/odata/v1/Products({id})/$value")
  file <- here::here(download_directory, glue::glue("{download_filename}.zip"))
  response <- GET(url, add_headers(Authorization =  paste("Bearer", content(auth)$access_token)),
                  write_disk(file, overwrite = TRUE))
  unzip(file, files = paste0(download_filename, c(".SEN3/NDVI.nc")), exdir= download_directory)
  file.remove(file)
  file.rename(here::here(download_directory, paste0(download_filename, ".SEN3/NDVI.nc")), 
              here::here(download_directory, save_filename))
  file.remove(here::here(download_directory, paste0(download_filename, ".SEN3")))
  return(file.path(download_directory, save_filename))
}
