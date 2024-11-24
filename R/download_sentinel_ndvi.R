#' Download and Preprocess Sentinel NDVI Data
#'
#' The function downloads Sentinel NDVI data from the Copernicus Data Space 
#' using credentials from environment variables. The data is then extracted,
#' renamed, saved to disk, and the initial download is removed.
#'
#' @author Emma Mendelsohn
#'
#' @param sentinel_ndvi_api_parameters A list containing the id of the product 
#' from the Copernicus Data Space that needs to be downloaded 
#' @param raw_filename The name of the raw file to be downloaded
#'
#' @return The function returns the filepath to '.nc' file containing the 
#' Sentinel NDVI data
#'
#' @note This function requires the "COPERNICUS_USERNAME" and "COPERNICUS_PASSWORD" 
#' to be defined in your system's environment variables
#'
#' @examples
#' download_sentinel_ndvi(sentinel_ndvi_api_parameters = list(id = "example_product_id"), 
#'                        raw_filename = "tempfile.zip")
#'
#' @export
download_sentinel_ndvi <- function(sentinel_ndvi_api_parameters, 
                                   raw_filename,
                                   sentinel_ndvi_token_file = "sentinel.token") {
  
  product_id <- sentinel_ndvi_api_parameters$id
  message(paste0("Downloading ", raw_filename))
  
  # Read in sentinel token
  sentinel_ndvi_token <- readLines(sentinel_ndvi_token_file)
  
  url <- glue::glue('https://zipper.dataspace.copernicus.eu/odata/v1/Products({product_id})/$value')
  
  i <- 0
  response <- list()
  response$status_code <- 401
  
  while(response$status_code != 200 && i < 6) {
   response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", sentinel_ndvi_token)),
               httr::write_disk(raw_filename, overwrite = TRUE))
   httr::message_for_status(response)
   if(response$status_code == 401) {
     get_sentinel_ndvi_token(filename = sentinel_ndvi_token_file)
     sentinel_ndvi_token <- readLines(sentinel_ndvi_token_file)
   }
   Sys.sleep(ifelse(2^i>60, 60, 2^i))
  }
  
  httr::stop_for_status(response)
  
  # Remove old nc file if it exists
  # Construct the file path of the .nc file
  nc_file <- paste0(tools::file_path_sans_ext(raw_filename), ".nc")
  if (file.exists(nc_file)) {
    file.remove(nc_file)
  }
  
  # Unzip the new download and rename
  unzip(raw_filename, 
        files = paste0(basename(raw_filename),"/NDVI.nc"), 
        junkpaths = TRUE, # Ditch archive folder structure
        overwrite = TRUE,
        exdir = dirname(raw_filename)) |>
    file.rename(paste0(tools::file_path_sans_ext(raw_filename), ".nc"))
  
  # Clean up archive
  file.remove(raw_filename)
  
  # Return path to .nc file
  return(paste0(tools::file_path_sans_ext(raw_filename), ".nc"))
}

