#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sentinel_ndvi_api_parameters
#' @return
#' @author Emma Mendelsohn
#' @export
download_sentinel_ndvi <- function(sentinel_ndvi_api_parameters, download_directory,  overwrite = FALSE) {
  
  existing_files <- list.files(download_directory)
  
  id <- sentinel_ndvi_api_parameters$id
  download_filename <- tools::file_path_sans_ext(sentinel_ndvi_api_parameters$properties$title)
  
  # extract info based on naming conventions 
  # https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-3-synergy/naming-conventions
  start_date <- str_extract(download_filename, "(\\d{8}T\\d{6})")
  end_date <- str_extract(download_filename, "(?<=_)(\\d{8}T\\d{6})(?=_\\w{6}_)")
 
  save_filename <- paste0(download_filename, ".nc")
  
  message(paste0("Downloading ", download_filename))
  
  if(save_filename %in% existing_files & !overwrite) {
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
  
  url <- glue::glue("https://download.dataspace.copernicus.eu/odata/v1/Products({id})/$value")
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

