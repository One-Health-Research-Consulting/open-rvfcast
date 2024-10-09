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
                                   raw_filename) {
  
  product_id <- sentinel_ndvi_api_parameters$id
  message(paste0("Downloading ", raw_filename))
  
  auth <- httr::POST("https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token", 
                   body = list(
                     grant_type = "password",
                     username = Sys.getenv("COPERNICUS_USERNAME"),
                     password = Sys.getenv("COPERNICUS_PASSWORD"),
                     client_id = "cdse-public"), 
                   encode = "form")
  
  url <- glue::glue('https://zipper.dataspace.copernicus.eu/odata/v1/Products({product_id})/$value')
  
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", httr::content(auth)$access_token)),
                  httr::write_disk(raw_filename, overwrite = TRUE))
  
  # Remove old nc file if it exists
  file.remove(paste0(tools::file_path_sans_ext(raw_filename), ".nc"))
  
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

