#' Fetch NASA Weather Data
#'
#' This function downloads weather data from NASA POWER for a given set of coordinates and time period.
#'
#' @author Nathan Layman, Emma Mendelsohn
#'
#' @param nasa_weather_coordinates Dataframe. A dataframe containing columns of country and nested coordinates 
#'        for the bounding box to download weather data.
#' @param months_to_process Character. The year-month (YYYY-MM format) for which to download weather data.
#' @param nasa_weather_variables Named character vector. Variables to download from NASA POWER, with names being 
#'        the column names in the output and values being the NASA POWER parameter codes.
#'        Default: c("relative_humidity" = "RH2M", "temperature" = "T2M", "precipitation" = "PRECTOTCORR").
#' @param local_folder Character. The directory where the raw data will be saved.
#' @param basename_template Character. Template for the output filename. Default: glue::glue("nasa_weather_raw_{months_to_process}.parquet").
#' @param overwrite Logical. Whether to overwrite existing data files. Default: FALSE.
#' @param ... Additional parameters passed to internal functions.
#'
#' @return Character. The file path to the raw NASA weather data parquet file.
#' 
#' @export
fetch_nasa_weather <- function(nasa_weather_coordinates,
                               months_to_process,
                               nasa_weather_variables = c("relative_humidity" = "RH2M",
                                                          "temperature" = "T2M",
                                                          "precipitation" = "PRECTOTCORR"),
                               local_folder,
                               basename_template = glue::glue("nasa_weather_raw_{months_to_process}.parquet"),
                               overwrite = FALSE,
                               ...) {
  
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
  
  message(glue::glue("Processing NASA weather data for month {months_to_process}"))
  
  # Establish filename
  raw_file <- file.path(local_folder, basename_template)
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  
  existing_data <- error_safe_read_parquet(raw_file)
  
  # Check if transformed file already exists and can be loaded. If so return file name and path
  if(!is.null(existing_data) & overwrite == FALSE) {
    message("File already exists and can be loaded, skipping processing")
    return(raw_file)
  }
  
  # Extract the coordinates and prepare to download data from nasapower
  coords <- nasa_weather_coordinates |> 
    select(country, coords) |> 
    unnest(coords) |>
    unnest_wider(x, names_sep = "_") |> 
    unnest_wider(y, names_sep = "_")
  
  # Download data from nasa power chunking by coordinate blocks and variables
  # Meteorological data sources are ½° x ⅝° latitude/longitude grid from GMAO MERRA-2
  nasa_recorded_weather <- map_dfr(1:nrow(coords), .progress = TRUE, function(i) {
    map(nasa_weather_variables, function(j) {
      nasapower::get_power(community = "ag",
                           lonlat = c(coords[i,]$x_1,
                                      coords[i,]$y_1,
                                      coords[i,]$x_2,
                                      coords[i,]$y_2), # xmin (W), ymin (S), xmax (E), ymax (N)
                           pars = j,
                           dates = c(start_date, end_date),
                           temporal_api = "daily")
    }) |>
      plyr::join_all() |>
      as_tibble() |>
      suppressMessages()
  })
  
  # Rename columns and clean up schema
  nasa_weather_raw <- nasa_recorded_weather |>
    distinct() |> # Distinct is necessary because there is a bit of overlap when gridding into 4.5 x 4.5 degree chunks
    rename_all(tolower) |> 
    dplyr::rename(!!!setNames(tolower(nasa_weather_variables), names(nasa_weather_variables)),
           month = mm, day = dd, x = lon, y = lat) |>
    mutate(across(c(year, month, day, doy), as.integer)) |> 
    mutate(date = lubridate::make_date(year, month, day)) |> 
    select(x, y, everything(), -yyyymmdd)
  
  # Write the transformed data to parquet
  nasa_weather_raw |> arrow::write_parquet(raw_file, compression = "gzip", compression_level = 5)
  
  # Test if transformed parquet file can be loaded. If not clean up and return NULL
  if(is.null(error_safe_read_parquet(raw_file))) {
    file.remove(raw_file)
    stop(glue::glue("{basename(raw_file)} could not be read."))
  }
  
  # If it can be loaded return file name and path of raw nasa weather parquet file
  return(raw_file)
}



#' Transform NASA Weather Data
#'
#' This function transforms NASA weather data based on a continent raster template and saves
#' the resulting dataset as a parquet file. It checks if the transformed file already exists 
#' and avoids redundant data processing.
#'
#' @author Nathan Layman, Emma Mendelsohn
#'
#' @param nasa_weather_raw Character. File path to the raw NASA weather data parquet file from fetch_nasa_weather().
#' @param continent_raster_template Character. The file path to the template raster used to resample and transform the weather data.
#' @param local_folder Character. The directory where the transformed data will be saved.
#' @param overwrite Logical. Whether to overwrite existing transformed data files. Default: FALSE.
#' @param ... Additional parameters passed to internal functions.
#'
#' @return Character. The file path to the transformed NASA weather data parquet file.
#' 
#' @export
transform_nasa_weather <- function(nasa_weather_raw,
                                   continent_raster_template,
                                   local_folder,
                                   overwrite = FALSE,
                                   ...) {
  
  # Check that nasa_weather_raw is only one value
  stopifnot(length(nasa_weather_raw) == 1)
  
  # Establish filename using local_folder and basename
  transformed_filename <- gsub("raw", "transformed", basename(nasa_weather_raw))
  transformed_file <- file.path(local_folder, transformed_filename)
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  
  existing_data <- error_safe_read_parquet(transformed_file)
  
  # Check if transformed file already exists and can be loaded. If so return file name and path
  if(!is.null(existing_data) & overwrite == FALSE) {
      message("File already exists and can be loaded, skipping processing")
      return(transformed_file)
  }
  
  nasa_weather_raw <- arrow::read_parquet(nasa_weather_raw)
  
  # Identify weather variable columns by excluding known coordinate and time columns
  weather_vars <- nasa_weather_raw |>
    select(-c(x, y, year, month, day, doy, date)) |>
    names()
  
  # Check for even spatial and temporal data coverage
  check_rows <- nasa_weather_raw |> dplyr::group_by(x, y) |> dplyr::count() |> dplyr::ungroup() |> dplyr::distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  check_rows <- nasa_weather_raw |> dplyr::group_by(date) |> dplyr::count() |> dplyr::ungroup() |> dplyr::distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  
  # Transform point data into standardized raster grid format and back to tabular data:
  # Process each variable separately, standardize to template raster for each date,
  # then join all variables together with consistent spatial and temporal structure
  dat_out <- purrr::map(weather_vars, function(var) {
    purrr::map_dfr(unique(nasa_weather_raw$date),
                   ~standardize_points_to_raster(nasa_weather_raw |> dplyr::filter(date == .x),
                                                 template_raster = terra::unwrap(continent_raster_template),
                                                 value_col = var,
                                                 fill_na = TRUE) |>
                     terra::as.data.frame(xy = TRUE) |>
                     dplyr::rename(!!var := names(.)[3]) |>  # Ensure correct column name
                     dplyr::mutate(date = .x,
                                   year = lubridate::year(date),
                                   month = lubridate::month(date),
                                   day = lubridate::day(date),
                                   doy = lubridate::yday(date)))  # Add date information
  }) |>
    plyr::join_all(by = c("x", "y", "date", "year", "month", "day", "doy")) |>  # Include date in join columns
    dplyr::as_tibble() |>
    dplyr::select(x, y, date, year, month, day, doy, dplyr::everything())
  
  # Write the transformed data to parquet
  dat_out |> arrow::write_parquet(transformed_file, compression = "gzip", compression_level = 5)
  
  # Test if transformed parquet file can be loaded. If not clean up and return NULL
  if(is.null(error_safe_read_parquet(transformed_file))) {
    file.remove(transformed_file)
    stop(glue::glue("{basename(transformed_file)} could not be read."))
  }
  
  # If it can be loaded return file name and path of transformed parquet
  return(transformed_file)
}
