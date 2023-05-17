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
download_nasa_weather <- function(nasa_weather_coordinates,
                                  nasa_weather_years,
                                  nasa_weather_variables, 
                                  download_directory) {
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
  start <- paste0(nasa_weather_years, "-01-01")
  end <- paste0(nasa_weather_years, "-12-31")
  current_time <- Sys.Date()
  if(nasa_weather_years == year(current_time)) end <- as.character(current_time)
  dates <- c(start, end)
  
  filename <- paste("nasa", "recorded_weather", nasa_weather_coordinates$country_iso3c, nasa_weather_years, sep = "_")
  message(paste0("Downloading ", filename))
  filename <- paste0(filename, ".gz.parquet") 
  
  if(filename %in% existing_files) {
    message("file already exists, skipping download")
    return(file.path(download_directory, filename)) # skip if file exists
  }  
  
  coords <-  nasa_weather_coordinates$coords |> 
    pluck(1) |> 
    rowwise() |> 
    group_split()
  
  out <- imap_dfr(coords, function(grp, y){
    message(paste("downloading", y, "of", length(coords)))
    
    x <- unlist(grp$x) 
    y <- unlist(grp$y) 
    
    nasapower::get_power(community = "ag",
                         lonlat = c(x[1], y[1], x[2], y[2]), # xmin (W), ymin (S), xmax (E), ymax (N)
                         pars = nasa_weather_variables,
                         dates = dates,
                         temporal_api = "daily")
  }) |> 
    distinct() # in case there is overlap due to need to have 2 deg coverage
  
  write_parquet(out, here::here(download_directory, filename), compression = "gzip", compression_level = 5)
  
  return(file.path(download_directory, filename))
}
