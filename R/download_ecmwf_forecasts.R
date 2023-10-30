#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
download_ecmwf_forecasts <- function(ecmwf_api_parameters,
                                     download_directory,
                                     overwrite = FALSE){
  
  existing_files <- list.files(download_directory)
  
  system <- ecmwf_api_parameters$system
  year <- ecmwf_api_parameters$year
  month <- unlist(ecmwf_api_parameters$month)

  filename <- paste("ecmwf", "seasonal_forecast", paste0("sys", system), year, sep = "_")
  filename <- paste0(filename, ".grib")

  message(paste0("Downloading ", filename))
  
  if(filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(download_directory, filename)) # skip if file exists
  }  
  request <- list(
    originating_centre = "ecmwf",
    system = system,
    variable = unlist(ecmwf_api_parameters$variables),
    product_type = unlist(ecmwf_api_parameters$product_types),
    year = year,
    month = month,
    leadtime_month = unlist(ecmwf_api_parameters$leadtime_months),
    area = round(unlist(ecmwf_api_parameters$spatial_bounds), 1),
    format = "grib",
    dataset_short_name = "seasonal-monthly-single-levels",
    target = filename
  )
  
  wf_set_key(user = Sys.getenv("ECMWF_USERID"), key = Sys.getenv("ECMWF_TOKEN"), service = "cds")
  
  safely(wf_request(user = Sys.getenv("ECMWF_USERID"), request = request, transfer = TRUE, path = download_directory))
  
  return(file.path(download_directory, filename))
}
