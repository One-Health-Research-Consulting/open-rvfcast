#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param parameters
#' @param variable
#' @param timestep
#' @param download_directory
#' @return
#' @author Emma Mendelsohn
#' @export
download_nasa_recorded_weather <- function(parameters = nasa_api_parameters, 
                                           variable = c("RH2M", "T2M", "PRECTOTCORR"),
                                           timestep = "daily", 
                                           download_directory = "data/nasa_parquets") {
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
  year <- parameters$year
  region <- parameters$region
  i <- parameters$i
  dates <- unlist(parameters$dates)
  x <- unlist(parameters$x) 
  y <- unlist(parameters$y) 
  
  filename <- paste("nasa", "recorded_weather", region, year, i, sep = "_")
  message(paste0("Downloading ", filename))
  filename <- paste0(filename, ".gz.parquet")
  
  if(filename %in% existing_files) {
    message("file already exists, skipping download")
    return(file.path(download_directory, filename)) # skip if file exists
  }  
  
  out <- nasapower::get_power(community = "ag",
                                   lonlat = c(x[1], y[1], x[2], y[2]), # xmin (W), ymin (S), xmax (E), ymax (N)
                                   pars = variable,
                                   dates = dates,
                                   temporal_api = "daily"
  )  
  
  write_parquet(out, here::here(download_directory, filename), compression = "gzip", compression_level = 5)
  
  return(file.path(download_directory, filename))
  
}
