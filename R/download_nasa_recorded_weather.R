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
                                           download_directory = "data/nasa_parquets") {
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
  year <- unique(parameters$year)
  region <- unique(parameters$region)
  variables <-  unlist(unique(parameters$variables))
  #i <- parameters$i
  
  filename <- paste("nasa", "recorded_weather", region, year, sep = "_")
  message(paste0("Downloading ", filename))
  filename <- paste0(filename, ".gz.parquet")
  
  if(filename %in% existing_files) {
    message("file already exists, skipping download")
    return(file.path(download_directory, filename)) # skip if file exists
  }  
  
  
  out <- parameters |> 
    rowwise() |> 
    group_split() |> 
    map_dfr(function(grp){
      message(paste("downloading", grp$i, "of", max(parameters$i)))
      dates <- unlist(grp$dates)
      x <- unlist(grp$x) 
      y <- unlist(grp$y) 
      
      nasapower::get_power(community = "ag",
                                  lonlat = c(x[1], y[1], x[2], y[2]), # xmin (W), ymin (S), xmax (E), ymax (N)
                                  pars = variables,
                                  dates = dates,
                                  temporal_api = "daily")
    })
  
  write_parquet(out, here::here(download_directory, filename), compression = "gzip", compression_level = 5)
  
  return(file.path(download_directory, filename))
  
}
