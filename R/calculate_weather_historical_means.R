#' Calculate historical weather means
#'
#' This function calculates the historical weather means based on the given range of days and preprocessed NASA weather data
#' and saves the result in the specified directory. It can also overwrite existing files if specified.
#'
#' @author Emma Mendelsohn
#'
#' @param nasa_weather_transformed_directory Directory containing the transformed NASA weather data.
#' @param weather_historical_means_directory Directory where the processed historical weather means will be saved.
#' @param days_of_year Vector of days of the year for which to calculate the mean.
#' @param lag_intervals Lag intervals which represent the range of days to calculate the lag. It should have the same length as lead_intervals.
#' @param lead_intervals Lead intervals which represent the range of days to calculate the lead. It should have the same length as lag_intervals.
#' @param overwrite Boolean flag indicating if existing files should be overwritten. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return String indicating the file path to the saved historical weather means.
#'
#' @note This function calculates the historical weather means based on various variables. 
#' If a file already exists and the overwrite flag is FALSE, it returns the existing file.
#'
#' @examples
#' calculate_weather_historical_means(nasa_weather_transformed_directory = './data',
#'                    weather_historical_means_directory = 'weather_means',
#'                    days_of_year = c(1:365),
#'                    lag_intervals = c(1:10), 
#'                    lead_intervals = c(11:20),
#'                    overwrite = TRUE)
#'
#' @export
calculate_weather_historical_means <- function(nasa_weather_transformed_directory,
                                               weather_historical_means_directory,
                                               days_of_year,
                                               lag_intervals,
                                               lead_intervals,
                                               overwrite = FALSE,
                                               ...) {
  
  # Check that we're only working with one interval length.
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
  
  # Create an error safe way to test if the parquet file can be read, if it exists
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)

  # Check if the save_file already exists and can be loaded. If so return file name and path
  if(!is.null(error_safe_read_parquet(save_filename))) return(save_filename)
  
  # Check if file already exists
  existing_files <- list.files(weather_historical_means_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists and can be loaded, skipping calculation")
    return(file.path(weather_historical_means_directory, save_filename))
  }
  
  # Open dataset to transformed data
  weather_transformed_dataset <- arrow::open_dataset(nasa_weather_transformed_directory)
  
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
