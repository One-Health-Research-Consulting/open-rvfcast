#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
download_ecmwf_forecasts <- function(parameters, 
                                     variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                     product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                     leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                     download_directory){
  
  suppressWarnings(dir.create(download_directory, recursive = TRUE))
  existing_files <- list.files(download_directory)
  
  system <- parameters$system
  year <- parameters$year
  month <- unlist(parameters$month)
  spatial_bounds <- unlist(parameters$spatial_bounds) |> round(1)

  filename <- paste("ecmwf", "seasonal_forecast", paste0("sys", system), year, sep = "_")
  filename <- paste0(filename, ".grib")

  message(paste0("Downloading ", filename))
  
  if(filename %in% existing_files) {
    message("file already exists, skipping download")
    return(file.path(download_directory, filename)) # skip if file exists
  }  
  request <- list(
    originating_centre = "ecmwf",
    system = system,
    variable = variable,
    product_type = product_type,
    year = year,
    month = month,
    leadtime_month = leadtime_month,
    area = spatial_bounds,
    format = "grib",
    dataset_short_name = "seasonal-monthly-single-levels",
    target = filename
  )
  
  wf_set_key(user = Sys.getenv("ECMWF_USERID"), key = Sys.getenv("ECMWF_TOKEN"), service = "cds")
  
  safely(wf_request(user = Sys.getenv("ECMWF_USERID"), request = request, transfer = TRUE, path = download_directory))
  
  
  return(file.path(download_directory, filename))
}
