#' Transform ECMWF Seasonal Forecast Data
#'
#' This function downloads ECMWF seasonal forecast data, transforms it into parquet format, and performs basic checks 
#' on the downloaded GRIB files. It leverages the ECMWF API to fetch forecast data for a specific year, and set of variables.
#' 
#' @author Nathan Layman, Emma Mendelsohn
#'
#' @param ecmwf_forecasts_api_parameters A list containing the parameters for the ECMWF API request such as year, month, variables, etc.
#' @param ecmwf_forecasts_transformed_directory Character. The path to the local folder where transformed files will be saved. Defaults to `ecmwf_forecasts_transformed_directory`.
#' @param continent_raster_template The path to the raster file used as a template for continent-level spatial alignment.
#' @param overwrite A boolean flag indicating whether to overwrite existing processed files. Default is FALSE.
#' @param ... Further arguments not used by the function but included for compatibility.
#' 
#' @return Returns the path to the transformed parquet file if successful, or stops the function if there is an error.
#' @details The function checks if the transformed file already exists for the given year and system. If it exists and is valid, it returns the file path. 
#' If not, it downloads the raw GRIB file using the ECMWF API, attempts to load and transform it, and saves the output as a parquet file. The function
#' checks file validity at multiple stages. Notes: Must accept licenses by manually downloading a dataset from here: https://cds-beta.climate.copernicus.eu/datasets/seasonal-postprocessed-single-levels?tab=overview
#' 
#' @notes ECNWF is an collection of 51 forecasts for each combination of base_date, lead_day, and variable which get summarized here
#' @export
#' 
#' #' @author Nathan Layman, Emma Mendelsohn
transform_ecmwf_forecasts <- function(ecmwf_forecasts_api_parameters,
                                      ecmwf_forecasts_transformed_directory,
                                      continent_raster_template,
                                      overwrite = FALSE,
                                      ...) {
  
  # Check that ecmwf_forecasts_api_parameters is only one row
  stopifnot(nrow(ecmwf_forecasts_api_parameters) == 1)
  
  # Extract necessary details from the ecmwf paramters
  year <- ecmwf_forecasts_api_parameters$year
  month <- unlist(ecmwf_forecasts_api_parameters$month)
  
  transformed_file <- file.path(ecmwf_forecasts_transformed_directory, glue::glue("ecmwf_seasonal_forecast_{month}_{year}.gz.parquet"))
  
  # Check if transformed file already exists and can be loaded. 
  # If so return file name and path **unless it's the current year**
  # If it's the current year there might be more data than last time so
  # re-run it.
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  # Re-run current forecasts as they may change
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  current_month <- as.numeric(format(Sys.Date(), "%m"))
  
  if (!is.null(error_safe_read_parquet(transformed_file)) &&
      (year < current_year || (year == current_year && month < current_month)) &&
      !overwrite) {
    message(glue::glue("{transformed_file} is already present and can be read."))
    return(transformed_file)
  }
  
  # If the transformed file doesn't exist download what we need from ECMWF
  raw_files <- expand.grid(product_type = unlist(ecmwf_forecasts_api_parameters$product_types), 
                           variable = unlist(ecmwf_forecasts_api_parameters$variables)) |>
    rowwise() |>
    mutate(raw_file = file.path(ecmwf_forecasts_transformed_directory, glue::glue("ecmwf_seasonal_forecast_{month}_{year}_{product_type}_{variable}.grib")))
  
  # Restrict to one product type
  raw_files <- raw_files |> filter(str_detect(product_type, "mean"))
  
  # Check if raw files can be opened
  # If not re-download them all.
  error_safe_read_rast <- possibly(terra::rast, NULL)
  raw_gribs = map(raw_files$raw_file, ~error_safe_read_rast(.x)) |> suppressWarnings()
  
  if(any(map_vec(raw_gribs, is.null))) {
    
    request_list <- purrr::pmap(raw_files, function(product_type, variable, raw_file) {
  
      list(
        originating_centre = "ecmwf",
        system = ifelse(year > 2021, 51, 5),
        product_type = product_type, 
        variable = variable, 
        year = year,
        month = month, # The current month
        leadtime_month = unlist(ecmwf_forecasts_api_parameters$leadtime_months), # What will the weather be X months from the current month?
        area = round(unlist(ecmwf_forecasts_api_parameters$spatial_bounds), 1),  
        format = "grib",
        dataset_short_name = "seasonal-monthly-single-levels",
        target = basename(raw_file)
      )
    })
    
    ecmwfr::wf_set_key(user = Sys.getenv("ECMWF_USERID"), key = Sys.getenv("ECMWF_TOKEN"))
    
    # https://cds-beta.climate.copernicus.eu/datasets/seasonal-postprocessed-single-levels?tab=overview
    # https://cds.climate.copernicus.eu/requests?tab=all
    purrr::walk(request_list, 
                .progress = TRUE,
                ~ecmwfr::wf_request(request = .x,
                                    user = Sys.getenv("ECMWF_USERID"), 
                                    path = ecmwf_forecasts_transformed_directory,
                                    verbose = T))
  
    # Verify that terra can open all the saved grib files. If not error out to try again next time
    raw_gribs = map(raw_files$raw_file, ~error_safe_read_rast(.x))
    
    # If not remove the files and stop
    if(any(map_vec(raw_gribs, is.null))) {
      file.remove(raw_files$raw_file)
      stop("At least one of the grib files could not be loaded after download.")
    }
    
    message(glue::glue("ecmwf_seasonal_forecast_{month}_{year} raw files successfully downloaded."))
  }
  
  # Product type 'mean' is how data was aggregated spatially.
  # Here mean refers to taking the average of the values within a grid cell.
  # The mean operation performed within extract_grib_data averages across the forecast ensemble
  grib_data <- pmap_dfr(raw_files, function(product_type, variable, raw_file) {
    extract_grib_data(raw_file, template = continent_raster_template) |> mutate(variable = variable)
  })
  
  message(glue::glue("ecmwf_seasonal_forecast_{month}_{year} metadata successfully read."))
  
  # The base date for an ECMWF forecast is the time when the forecast was initialized 
  # or made. For example, a forecast with a base date of Wed 1 Nov 2024 18 UTC (T+6) s
  # means that the forecast was made at 18 UTC on Wednesday, November 1, 2024
  # And lead month is the month average for that many months out with a lead month
  # of 1 being the month that includes the base_date which _should_ always be on the
  # first of the month. So base date 01-01-2005 1 lead month is the monthly average
  # forecast for January 2005. 
  
  # 1. Convert kelvin to celsius (note these columns are mislabeled as C as provided by ecmwf)
  # 2. Fix total_precipitation metadata and convert units from m/second to mm/day. 
  # Note the variable name is total_precipitation but it is really *mean total precipitation rate*
  # 3. Correct precip sd
  grib_data <- grib_data |> mutate(mean = ifelse(units == "C", mean - 273.15, ((mean > 0) * 8.64e+7 * mean)),
                                   sd = ifelse(units == "", ((sd > 0) * 8.64e+7 * sd), sd),
                                   var_id = ifelse(units == "", "tprate", var_id),
                                   units = ifelse(units == "", "mm/day", units),
                                   month = as.integer(lubridate::month(base_date)), # Base month
                                   year = as.integer(lubridate::year(base_date)), # Base year
                                   lead_month = as.integer(lubridate::month(forecast_end_date - 1)),
                                   lead_year = as.integer(lubridate::year(forecast_end_date - 1)),
                                   variable = fct_recode(variable,
                                                         dewpoint = "2m_dewpoint_temperature",
                                                         temperature = "2m_temperature",
                                                         precipitation = "total_precipitation"))
  
  # Calculate relative humidity from temperature and dewpoint temperature
  grib_data <- grib_data |> 
    select(x, y, base_date, month, year, lead_month, lead_year, mean, sd, variable) |>
    pivot_wider(names_from = variable, values_from = c(mean, sd), names_glue = "{variable}_{.value}") |> # Reshape to make it easier to calculate composite values like relative humidity
    mutate(
  
      # Calculate saturation and actual vapor pressures
      saturation_vapor_pressure = exp((17.625 * temperature_mean) / (243.04 + temperature_mean)),
      actual_vapor_pressure = exp((17.625 * dewpoint_mean) / (243.04 + dewpoint_mean)),
      
      # Calculate mean relative humidity
      relative_humidity_mean = 100 * actual_vapor_pressure / saturation_vapor_pressure,
      
      # Calculate partial derivatives of RH with respect to temperature and dewpoint temperature
      # Used for propagation of error to get sd of rh from sd of temperature and dewpoint
      dRH_dT = -relative_humidity_mean * (17.625 * 243.04) / ((243.04 + temperature_mean)^2),
      dRH_dTd = relative_humidity_mean * (17.625 * 243.04) / ((243.04 + dewpoint_mean)^2),
      
      # Apply error propagation formula to get standard deviation of relative humidity from 
      # standard deviations of temperature and dewpoint temperature
      relative_humidity_sd = sqrt((dRH_dT * temperature_sd)^2 + (dRH_dTd * dewpoint_sd)^2)
    ) |> 
    select(-saturation_vapor_pressure,  # Remove intermediate columns
           -actual_vapor_pressure, 
           -dRH_dT, 
           -dRH_dTd, -contains("dewpoint")) %>%
    setNames(gsub("_mean", "", names(.))) |>
    select(-contains("sd"), contains("sd"))
    
  # Save as parquet 
  arrow::write_parquet(grib_data, transformed_file, compression = "gzip", compression_level = 5)
  
  # Test if transformed parquet file can be loaded.
  if(is.null(error_safe_read_parquet(transformed_file))) {
    file.remove(transformed_file)
    stop(glue::glue("{basename(transformed_file)} could not be read."))
  }
  
  # Clean up raw grib files
  file.remove(raw_files$raw_file)
  
  # Clean up in-memory objects
  rm(list=setdiff(ls(), "transformed_file"))
  
  # Return location of transformed file
  return(transformed_file)
}
