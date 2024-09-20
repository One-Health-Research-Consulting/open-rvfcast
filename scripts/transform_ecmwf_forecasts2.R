#' Transform ECMWF Seasonal Forecast Data
#'
#' This function downloads ECMWF seasonal forecast data, transforms it into parquet format, and performs basic checks 
#' on the downloaded GRIB files. It leverages the ECMWF API to fetch forecast data for a specific system, year, and set of variables.
#' 
#' @author Nathan Layman, Emma Mendelsohn
#'
#' @param ecmwf_forecasts_api_parameters A list containing the parameters for the ECMWF API request such as system, year, month, variables, etc.
#' @param local_folder Character. The path to the local folder where transformed files will be saved. Defaults to `ecmwf_forecasts_transformed_directory`.
#' @param continent_raster_template The path to the raster file used as a template for continent-level spatial alignment.
#' @param n_workers Integer. The number of workers to use for parallel processing, defaults to 2.
#' 
#' @return Returns the path to the transformed parquet file if successful, or stops the function if there is an error.
#' @details The function checks if the transformed file already exists for the given year and system. If it exists and is valid, it returns the file path. 
#' If not, it downloads the raw GRIB file using the ECMWF API, attempts to load and transform it, and saves the output as a parquet file. The function
#' checks file validity at multiple stages.
#' 
#' @export
transform_ecmwf_forecasts <- function(ecmwf_forecasts_api_parameters,
                                      local_folder = ecmwf_forecasts_transformed_directory,
                                      continent_raster_template,
                                      n_workers = 2) {
  
  # Check that ecmwf_forecasts_api_parameters is only one row
  stopifnot(nrow(ecmwf_forecasts_api_parameters) == 1)
  
  # Extract necessary details from the ecmwf paramters
  system <- ecmwf_forecasts_api_parameters$system
  year <- ecmwf_forecasts_api_parameters$year
  month <- unlist(ecmwf_forecasts_api_parameters$month)
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  raw_file <- file.path(local_folder, glue::glue("ecmwf_seasonal_forecast_sys{system}_{year}.grib"))
  transformed_file <- gsub("\\.grib", "\\.gz\\.parquet", raw_file)
  
  # Check if transformed file already exists and can be loaded. 
  # If so return file name and path unless it's the current year
  if(!is.null(error_safe_read_parquet(transformed_file)) && year < year(Sys.time())) return(transformed_file)
  
  message(paste0("Downloading ", raw_file))
  
  request <- list(
    originating_centre = "ecmwf",
    system = system,
    variable = unlist(ecmwf_forecasts_api_parameters$variables),
    product_type = unlist(ecmwf_forecasts_api_parameters$product_types),
    year = year,
    month = month,
    leadtime_month = unlist(ecmwf_forecasts_api_parameters$leadtime_months),
    area = round(unlist(ecmwf_forecasts_api_parameters$spatial_bounds), 1),
    format = "grib",
    dataset_short_name = "seasonal-monthly-single-levels",
    target = basename(raw_file)
  )
  
  ecmwfr::wf_set_key(user = Sys.getenv("ECMWF_USERID"), key = Sys.getenv("ECMWF_TOKEN"), service = "cds")
  
  ecmwfr::wf_request(user = Sys.getenv("ECMWF_USERID"), 
                     request = request[[1]], 
                     transfer = TRUE, 
                     path = local_folder)
  
  # Test if transformed_file is in directory and can be successfully loaded
  # Verify that terra can open the saved grib file. If not return NULL
  error_safe_read_rast <- possibly(terra::rast, NULL)
  raw_grib = error_safe_read_rast(raw_file)
  
  # If not remove the file and stop
  if(is.null(raw_grib)) {
    file.remove(raw_file)
    stop("Grib could not be loaded.")
  }
  
  # Read in continent template raster
  continent_raster_template <- terra::rast(continent_raster_template)
  
  # # Get associated metadata and remove non-df rows
  # # But we aren't doing anything with the metadata?
  # grib_meta <- system(paste("grib_ls", raw_file), intern = TRUE)
  # remove <- c(1, (length(grib_meta)-2):length(grib_meta)) 
  # grib_meta <- grib_meta[-remove]
  
  # Here's a method to get metadata that doesn't depend on grib_ls.
  # grib_meta <- get_grib_metadata(raw_file)
  
  
  
  
  return(file.path(local_folder, filename))
}
