#' Transform and Download NASA Weather Data
#'
#' This function downloads weather data from NASA POWER for a given set of coordinates and year, transforms it
#' based on a continent raster template, and saves the resulting dataset as a parquet file. It checks if the 
#' transformed file already exists and avoids redundant data downloads and processing.
#'
#' @author Nathan Layman, Emma Mendelsohn
#'
#' @param nasa_weather_coordinates Dataframe. A dataframe containing columns of coordinates for the bounding box (x_min, y_min, x_max, y_max) to download weather data.
#' @param nasa_weather_years Integer. The year for which to download and transform the weather data.
#' @param continent_raster_template Character. The file path to the template raster used to resample and transform the weather data.
#' @param local_folder Character. The directory where the transformed data will be saved.
#' @param ... 
#'
#' @return A list of transformed NASA weather data parquet files
#' 
#' @export
transform_nasa_weather <- function(nasa_weather_coordinates,
                                   nasa_weather_years,
                                   nasa_weather_variables = c("RH2M", "T2M", "PRECTOTCORR"),
                                   continent_raster_template,
                                   local_folder,
                                   overwrite = FALSE,
                                   ...) {
  
  # Extract start and end dates to make filename
  start <- paste0(nasa_weather_years, "-01-01")
  end <- paste0(nasa_weather_years, "-12-31")
  current_time <- Sys.Date()
  if(nasa_weather_years == year(current_time)) end <- as.character(current_time-1)
  dates <- c(start, end)
  
  transformed_file <- file.path(local_folder, glue::glue("nasa_weather_{nasa_weather_years}.parquet"))
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  # Check if transformed file already exists and can be loaded. If so return file name and path
  if(!is.null(error_safe_read_parquet(transformed_file)) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(transformed_file)
  }
  
  # If not extract the coordinates and prepare to re-download data from nasapower
  coords <- nasa_weather_coordinates |> 
    select(coords) |> 
    unnest(coords) |>
    unnest_wider(x, names_sep = "_") |> 
    unnest_wider(y, names_sep = "_")
  
  # nasapower seems like it only does one call per point?
  # Why can't we just do the entire continent?
  nasa_recorded_weather <- map_dfr(1:nrow(coords), .progress = T, function(i) {

    map(nasa_weather_variables, function(j) {

      nasapower::get_power(community = "ag",
                        lonlat = c(coords[i,]$x_1,
                                   coords[i,]$y_1,
                                   coords[i,]$x_2,
                                   coords[i,]$y_2), # xmin (W), ymin (S), xmax (E), ymax (N)
                        pars = j,
                        dates = dates,
                        temporal_api = "daily")
    }) |>
    plyr::join_all() |>
    as_tibble() |> 
    suppressMessages()
  })

  nasa_weather_transformed <- nasa_recorded_weather |>
    distinct() |>
    rename_all(tolower) |> 
    rename(relative_humidity = rh2m, temperature = t2m, precipitation= prectotcorr,
           month = mm, day = dd, x = lon, y = lat, doy = doy) |> 
    mutate(across(c(year, month, day, doy), as.integer)) |> 
    mutate(date = lubridate::make_date(year, month, day)) |> 
    select(x, y, everything(), -yyyymmdd) |>  # terra::rast - the first with x (or longitude) and the second with y (or latitude) coordinates 
    mutate(year = as.integer(year(date)))
  
  assertthat::assert_that(names(nasa_weather_transformed)[1]=="x")
  assertthat::assert_that(names(nasa_weather_transformed)[2]=="y")
  
  # Check for even data coverage
  check_rows <- nasa_weather_transformed |> group_by(x, y) |> count() |> ungroup() |> distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  check_rows <- nasa_weather_transformed |> group_by(doy) |> count() |> ungroup() |> distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  
  nasa_weather_transformed <- drop_na(nasa_weather_transformed)
  
  # Read in continent template raster
  continent_raster_template <- terra::rast(continent_raster_template)
  
  # Split by day of year and transform with template raster. Return as a row-bound dataframe
  dat_out <- nasa_weather_transformed |> 
    group_split(month, day, year, doy, date) |> 
    map_dfr(function(daily){
      daily_info <- distinct(daily, month, day, year, doy, date)
      daily_rast <- select(daily, -month, -day, -year, -doy, -date)
      raw_raster <- terra::rast(daily_rast) 
      terra::crs(raw_raster) <- terra::crs(terra::rast()) 
      transformed_raster <- transform_raster(raw_raster = raw_raster,
                                             template = continent_raster_template)
      cbind(daily_info, as.data.frame(transformed_raster, xy = TRUE)) |> 
        select(x, y, everything())
    }) 
  
  dat_out |> arrow::write_parquet(transformed_file, compression = "gzip", compression_level = 5)
  
  # Test if transformed parquet file can be loaded. If not clean up and return NULL
  if(is.null(error_safe_read_parquet(transformed_file))) {
    file.remove(transformed_file)
    stop(glue::glue("{basename(transformed_file)} could not be read."))
  }
  
  # If it can be loaded return file name and path of transformed parquet
  return(transformed_file)
}
