#' Fetch and Transform NASA Weather Data
#'
#' This function downloads daily weather data from NASA POWER for a given month, 
#' transforms it to match a continental raster template, and saves the processed 
#' data as a parquet file.
#'
#' @author [Your Name]
#'
#' @param months_to_process Character. The year-month (YYYY-MM format) for which 
#' to download and transform weather data.
#' @param nasa_weather_variables Named character vector. Variables to download from 
#' NASA POWER, with names being the column names in the output and values being the 
#' NASA POWER parameter codes. Default: c("relative_humidity" = "RH2M", "temperature" = "T2M", "precipitation" = "PRECTOTCORR").
#' @param continent_raster_template SpatRaster. A wrapped raster template used for 
#' cropping and resampling the NASA weather data to the desired spatial extent and resolution.
#' @param local_folder Character. The directory where the transformed data will be saved. 
#' Default: "data/nasa_weather_transformed".
#' @param basename_template Character. Template for the output filename. 
#' Default: glue::glue("nasa_weather_raw_{months_to_process}.parquet").
#' @param endpoint Character. URL template for downloading NASA POWER NetCDF files. 
#' Default: "https://power-datastore.s3.amazonaws.com/v10/daily/{year}/{month}/power_10_daily_{yyyymmdd}_merra2_lst.nc".
#' @param overwrite Logical. Whether to overwrite existing transformed data files. Default: FALSE.
#' @param ... Additional parameters passed to internal functions.
#'
#' @return Character. The file path to the transformed NASA weather data parquet file.
#'
#' @export
fetch_and_transform_nasa_weather <- function(months_to_process,
  nasa_weather_variables = c("relative_humidity" = "RH2M", "temperature" = "T2M", "precipitation" = "PRECTOTCORR"),
  continent_raster_template,
  local_folder = "data/nasa_weather_transformed",
  basename_template = "nasa_weather_transformed_{months_to_process}.parquet",
  endpoint = "https://power-datastore.s3.amazonaws.com/v10/daily/{year}/{month}/power_10_daily_{yyyymmdd}_merra2_lst.nc",
  overwrite = FALSE,
  ...) {

  continent_raster_template <- terra::unwrap(continent_raster_template)

  # Check that nasa_weather_variables has names
  stopifnot(!is.null(names(nasa_weather_variables)))
  
  # Check that months_to_process is only one value
  stopifnot(length(months_to_process) == 1)
  
  # Create date for the first of the month
  start_date <- lubridate::ymd(paste0(months_to_process, "-01"))
  
  # Get the last day of the month
  end_date <- lubridate::ceiling_date(start_date, "month") - lubridate::days(1)
  if (end_date > Sys.Date()) {
    end_date <- Sys.Date()
  }
  
  # Extract the three variables you need
  year <- lubridate::year(start_date)
  month <- format(start_date, "%m")
  dates <- format(seq(start_date, end_date, by = "day"), "%Y%m%d")

  message(glue::glue("Processing NASA weather data for month {months_to_process}"))

  # Establish filename
  transformed_file <- file.path(local_folder, glue::glue(basename_template))
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  
  existing_data <- error_safe_read_parquet(transformed_file)
  
  # Check if transformed file already exists and can be loaded. If so return file name and path
  if(!is.null(existing_data) & overwrite == FALSE) {
    message("File already exists and can be loaded, skipping processing")
    return(transformed_file)
  }

  # Track if any downloads failed
  failed_downloads <- c()
  
  # This errors if any of the files in the month are wrong. 
  nasa_recorded_weather <- map_df(dates, .progress = TRUE, function(yyyymmdd) {
    
    # Establish NetCDF filename
    nc_file <- file.path(local_folder, glue::glue(endpoint) |> basename())
    
    # Try to download file - returns TRUE if successful, FALSE if failed
    download <- tryCatch({
      curl::curl_download(glue::glue(endpoint), nc_file)
      TRUE
    }, error = function(e) {
      FALSE
    })

    if(!download) {
      failed_downloads <<- c(failed_downloads, yyyymmdd)
      return(NULL)
    }

    # Map across variable types
    results <- imap(nasa_weather_variables, function(var, name) {

      # Read in raw raster of given var and set CRS
      raw_raster <- terra::rast(nc_file, subds = var)
      terra::crs(raw_raster) <- "EPSG:4326"

      # Transform and resample raster to template
      raw_raster <- terra::crop(raw_raster, continent_raster_template)
      transformed_raster <- transform_raster(raw_raster, continent_raster_template)
      
      # Convert to XY table
      names(transformed_raster) <- name
      terra::as.data.frame(transformed_raster, xy = TRUE)
    }) |> 
      plyr::join_all(by = c("x", "y")) |>
      mutate(date = lubridate::ymd(yyyymmdd))

      # Clean up file
      file.remove(nc_file)
      
      results
  }) |>
  mutate(year = year,
         month = month,
         day = lubridate::day(date),
         doy = lubridate::yday(date)) |>
  dplyr::select(x, y, date, year, month, day, doy, dplyr::everything())

  nasa_recorded_weather |> arrow::write_parquet(transformed_file, compression = "gzip", compression_level = 5)

  # Test if transformed parquet file can be loaded. If not clean up and return NULL
  if(is.null(error_safe_read_parquet(transformed_file))) {
    file.remove(transformed_file)
    stop(glue::glue("{basename(transformed_file)} could not be read."))
  }

  if(length(failed_downloads) > 0) stop(glue::glue("Some NASA POWER netcdf files failed to download: {paste(failed_downloads, collapse = ', ')}"))
  
  # If it can be loaded return file name and path of transformed parquet
  return(transformed_file)
}
