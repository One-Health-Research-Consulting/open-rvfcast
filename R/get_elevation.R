#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param 
#' @return
#' @author Whitney Bagge
#' @export
library(paws)
get_elevation<- function(elevation_directory_raw, overwrite=FALSE) {
  
  existing_files <- list.files(elevation_directory_raw)
  
  download_filename <- tools::file_path_sans_ext(existing_files)
  
  save_filename <- paste0(download_filename, ".tif")
  
  message(paste0("Downloading ", download_filename))
  
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(elevation_directory_raw, save_filename)) # skip if file exists
  }  
  
  s3_svc <- s3(config = list(region = "af-south-1",  credentials = list(anonymous = TRUE)))
  # Download the file directly to disk in the current working directory
  s3_svc$download_file(
    Bucket = "deafrica-input-datasets",
    Key = "srtm_dem/srtm_africa.tif",
    Filename = "data/elevation/srtm_africa.tif"
  )

  
  return(elevation_directory_raw)
  
}