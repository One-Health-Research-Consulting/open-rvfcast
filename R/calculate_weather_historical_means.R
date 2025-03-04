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
calculate_weather_historical_means <- function(nasa_weather_transformed,
                                               weather_historical_means_directory,
                                               basename_template = "weather_historical_mean_doy_{i}.parquet",
                                               overwrite = FALSE,
                                               ...) {
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  # Open dataset can handle multi-file datasets larger than can
  # fit in memory
  nasa_weather_data <- arrow::open_dataset(nasa_weather_transformed)

  # Fast because we can avoid collecting until write_parquet
  weather_historical_means <- map_vec(1:366, .progress = TRUE, function(i) {
    
    filename <- file.path(weather_historical_means_directory, glue::glue(basename_template))
    
    # Check if glw files exist and can be read and that we don't want to overwrite them.
    if(!is.null(error_safe_read_parquet(filename)) & !overwrite) {
      message(glue::glue("{filename} already exists and can be loaded, skipping"))
      return(filename)
    }
    
    mean_vals <- nasa_weather_data |> 
      filter(doy == i) |>
      group_by(x, y, doy) |> 
      summarize(across(matches("temperature|precipitation|humidity"), ~mean(.x, na.rm = T)), 
                       .groups = "drop")
                
    sd_vals <- nasa_weather_data |> 
      filter(doy == i) |>
      group_by(x, y, doy) |> 
      summarize(across(matches("temperature|precipitation|humidity"), ~sd(.x, na.rm = T), 
                .names = "{.col}_sd"),
                .groups = "drop")
    
    mean_vals |> left_join(sd_vals) |> arrow::write_parquet(filename, compression = "gzip", compression_level = 5)
    
    filename
  })
  
  return(weather_historical_means)
}
