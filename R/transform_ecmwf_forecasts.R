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
#' checks file validity at multiple stages. Notes: Must accept licenses by manually downloading a dataset from here: https://cds-beta.climate.copernicus.eu/datasets/seasonal-postprocessed-single-levels?tab=overview
#' 
#' @export
transform_ecmwf_forecasts <- function(ecmwf_forecasts_api_parameters,
                                      local_folder = ecmwf_forecasts_transformed_directory,
                                      continent_raster_template,
                                      n_workers = 2,
                                      time_out = 3600) {
  
  # Check that ecmwf_forecasts_api_parameters is only one row
  stopifnot(nrow(ecmwf_forecasts_api_parameters) == 1)
  
  # Extract necessary details from the ecmwf paramters
  system <- ecmwf_forecasts_api_parameters$system
  year <- ecmwf_forecasts_api_parameters$year
  month <- unlist(ecmwf_forecasts_api_parameters$month)
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  transformed_file <- gsub("\\.grib", "\\.gz\\.parquet", raw_file)
  
  # Check if transformed file already exists and can be loaded. 
  # If so return file name and path unless it's the current year
  if(!is.null(error_safe_read_parquet(transformed_file)) && year < year(Sys.time())) return(transformed_file)
  
  # ensure local_folder ends in '/'
  
  # If the transformed file doesn't exist download what we need from ECMWF
  # The metadata returned in grib formats sucks and requires an externam dependancy
  # so download each piece separtatly. 
  raw_files <- expand.grid(product_type = unlist(ecmwf_forecasts_api_parameters$product_types), 
                           variable = unlist(ecmwf_forecasts_api_parameters$variables)) |>
    rowwise() |>
    mutate(raw_file = file.path(local_folder, glue::glue("ecmwf_seasonal_forecast_sys{system}_{year}_{product_type}_{variable}.grib")))

  request_list <- purrr::pmap(raw_files, function(product_type, variable, raw_file) {

    list(
      originating_centre = "ecmwf",
      system = system,
      variable = variable, # This can't (easily) be extracted from terra::describe()
      product_type = product_type,  # This can't be extracted from terra::describe()
      year = year,
      month = month,
      leadtime_month = unlist(ecmwf_forecasts_api_parameters$leadtime_months), # This can be extracted from terra::describe()
      area = round(unlist(ecmwf_forecasts_api_parameters$spatial_bounds), 1),  # This can be extracted from terra::describe()
      format = "grib",
      dataset_short_name = "seasonal-monthly-single-levels",
      target = basename(raw_file)
    )
    
  })
  
  ecmwfr::wf_set_key(user = Sys.getenv("ECMWF_USERID"), key = Sys.getenv("ECMWF_TOKEN"))
  
  # https://cds-beta.climate.copernicus.eu/datasets/seasonal-postprocessed-single-levels?tab=overview
  ecmwf_files <- ecmwfr::wf_request_batch(request = request_list,
                                          user = Sys.getenv("ECMWF_USERID"), 
                                          workers = n_workers,
                                          path = local_folder,
                                          time_out = time_out,
                                          total_timeout = length(request_list) * time_out/n_workers)
  
  # Verify that terra can open all the saved grib files. If not return NULL to try again next time
  error_safe_read_rast <- possibly(terra::rast, NULL)
  raw_gribs = map(ecmwf_files, ~error_safe_read_rast(.x))
  
  # If not remove the files and stop
  if(any(is.null(raw_gribs))) {
    file.remove(raw_files$raw_file)
    stop(glue::glue("At least one of the grib files for {raw_file} could not be loaded after download."))
  }
  
  # Get associated metadata and remove non-df rows
  grib_meta <- system(paste("grib_ls", raw_file), intern = TRUE)
  remove <- c(1, (length(grib_meta)-2):length(grib_meta))
  grib_meta <- grib_meta[-remove]
  
  # Almost got rid of grib_ls dependency but can't get data_type from gdalinfo.
  # Though it looks like we're only using the mean?
  grib_meta <- pmap_dfr(raw_files, function(product_type, variable, raw_file) {
    get_grib_metadata(raw_file) |>
    mutate(step_range = as.numeric(GRIB_FORECAST_SECONDS) / 3600, # forecast step in hours from seconds
           data_date = as.POSIXct(as.numeric(GRIB_REF_TIME), origin = "1970-01-01", tz = "UTC"), # Forecasting out from
           short_name = stringr::str_to_sentence(GRIB_ELEMENT),
           data_type = product_type,
           variable = variable,
           variable_id = as.character(glue::glue("{data_date}_step{step_range}_{data_type}_{short_name}"))) |>
      dplyr::select(data_date, step_range, data_type, variable, short_name, variable_id)
  })
  
  raw_grib <- terra::rast(raw_gribs)
  # Rename layers with the metadata names
  names(raw_grib) <- grib_meta$variable_id
  
  # Units conversions
  # 2d = "2m_dewpoint_temperature" = 	2 metre dewpoint temperature K
  # 2t = "2m_temperature" = 2 metre temperature K
  # tprate = "total_precipitation" =  Total precipitation m/s
  # https://cds.climate.copernicus.eu/cdsapp#!/dataset/seasonal-monthly-single-levels?tab=overview  
  
  # Convert kelvin to celsius (note these columns are mislabeled as C)
  temp_cols <- which(str_ends(names(grib_subset), "2d|2t"))
  grib_temp <- subset(grib_subset, temp_cols)
  grib_temp <- app(grib_temp, function(i) i - 273.15)
  
  # Convert precipitation from m/second to mm/day
  precip_cols <- which(str_ends(names(grib_subset), "tprate"))
  grib_precip <- subset(grib_subset, precip_cols)
  grib_precip <- ifel(grib_precip < 0, 0, grib_precip) # some very small negative values
  grib_precip <- app(grib_precip, function(i) i * 8.64e+7)
  
  # Calculate per-pixel mean for each unique combination, across all models
  # transform to template
  grib_converted <- c(grib_precip, grib_temp)
  grib_index <- as.integer(factor(names(grib_converted), levels = unique(names(grib_converted))))
  grib_means <- terra::tapp(grib_converted, grib_index, "mean", cores = n_workers) |> 
    setNames(unique(names(grib_converted))) |> 
    transform_raster(template = continent_raster_template) |> 
    as.data.frame(xy = TRUE) 
  
  # Using the means, calculate relative humidity from dew point and temp
  dp_cols <- names(grib_means)[str_ends(names(grib_means), "2d")] |> sort() # dew point
  t_cols <- names(grib_means)[str_ends(names(grib_means), "2t")] |> sort() # temp
  assertthat::assert_that(identical(str_remove(dp_cols, "_2d"), str_remove(t_cols, "_2t")))
  
  rel_humidity <- map2_dfc(dp_cols, t_cols, function(dp, t){
    dp <- grib_means[,dp]
    t <- grib_means[, t]
    rh <- 100 * exp((17.625 * dp)/(243.04+dp))/exp((17.625 * t)/(243.04+t))
    assertthat::assert_that(all(rh <= 100 & rh >=0))
    return(rh)
  }) |> set_names(str_replace(dp_cols, "_2d", "_rh"))
  
  # Remove dewpoint temperature and add relative humidity
  grib_means <- grib_means |> 
    select(-dp_cols) |> 
    bind_cols(rel_humidity)|> 
    reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "mean") 
   
  # Not using Sds at the moment
  # grib_sds <- terra::tapp(grib_converted, grib_index, "sd", cores = n_workers) |> 
  #   setNames(unique(names(grib_converted))) |> 
  #   transform_raster(template = continent_raster_template) |> 
  #   as.data.frame(xy = TRUE) |> 
  #   reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "std") |> 
  #   pull(std)
  
  # Add relative humidity to metafile 
  meta_out <- meta |> 
    distinct() |> 
    mutate(short_name = if_else(short_name == "2d", "rh", short_name)) |> 
    mutate(variable_id = str_replace(variable_id, "_2d", "_rh"))
    
  # Link means with metadata for export
  grib_means <- grib_means |> 
    left_join(meta_out, by = "variable_id") |> 
    arrange(variable_id, x, y) |> 
    select(x, y, mean, data_date, step_range, short_name)
  
  # Standardize leads
  n_leads <- grib_means |> 
    group_by(data_date) |> 
    summarize(n = n_distinct(step_range)) |> 
    distinct(n)
  assertthat::are_equal(6, n_leads$n)
  
  grib_means <- grib_means |> 
    group_by(data_date) |> 
    mutate(lead_month = factor(rank(step_range), labels = 1:6)) |> 
    select(-step_range) |> 
    ungroup()

  # Save as parquet 
  write_parquet(grib_means, here::here(ecmwf_forecasts_transformed_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(ecmwf_forecasts_transformed_directory, save_filename))
}
