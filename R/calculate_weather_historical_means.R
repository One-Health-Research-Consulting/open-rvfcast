#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nasa_weather_transformed
#' @param nasa_weather_transformed_directory
#' @param weather_historical_means_directory
#' @return
#' @author Emma Mendelsohn
#' @export
calculate_weather_historical_means <- function(nasa_weather_transformed, # enforce dependency
                                               nasa_weather_transformed_directory,
                                               weather_historical_means_directory,
                                               days_of_year,
                                               lag_intervals,
                                               lead_intervals,
                                               overwrite = FALSE) {
  
  interval_length <- unique(c(diff(lag_intervals), diff(lead_intervals)))
  assertthat::are_equal(length(interval_length), 1)
  
  # Set filename
  # use dummy dates to keep date logic
  doy_start <- days_of_year
  dummy_date_start  <- ymd("20210101") + doy_start - 1
  dummy_date_end  <- dummy_date_start + interval_length - 1
  doy_end <- yday(dummy_date_end)
      
  doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
  doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")
  
  save_filename <- glue::glue("historical_weather_mean_doy_{doy_start_frmt}_to_{doy_end_frmt}.gz.parquet")
  message(paste("calculating historical weather means and standard deviations for doy", doy_start_frmt, "to", doy_end_frmt))
  
  # Check if file already exists
  existing_files <- list.files(weather_historical_means_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(weather_historical_means_directory, save_filename))
  }
  # Open dataset to transformed data
  weather_transformed_dataset <- open_dataset(nasa_weather_transformed_directory)
  
  # Filter for relevant days of the year and calculate historical means and standard deviations
  doy_select <- yday(seq(dummy_date_start, dummy_date_end, by = "day"))
  
  historical_means <- weather_transformed_dataset |> 
    filter(day_of_year %in% doy_select) |> 
    group_by(x, y) |> 
    summarize(historical_relative_humidity_mean = mean(relative_humidity),
              historical_temperature_mean = mean(temperature),
              historical_precipitation_mean = mean(precipitation),
              historical_relative_humidity_sd = sd(relative_humidity),
              historical_temperature_sd = sd(temperature),
              historical_precipitation_sd = sd(precipitation)) |> 
    ungroup() 
  
  # Save as parquet 
  write_parquet(historical_means, here::here(weather_historical_means_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(weather_historical_means_directory, save_filename))
  
  
}
