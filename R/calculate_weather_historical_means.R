#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nasa_weather_transformed
#' @param nasa_weather_directory_transformed
#' @param weather_historical_means_directory
#' @return
#' @author Emma Mendelsohn
#' @export
calculate_weather_historical_means <- function(nasa_weather_transformed,
                                               nasa_weather_directory_transformed,
                                               weather_historical_means_directory,
                                               days_of_year,
                                               overwrite = FALSE) {
  
  # Set filename
  doy <- days_of_year
  doy_frmt <- str_pad(doy,width = 3, side = "left", pad = "0")
  save_filename <- glue::glue("historical_weather_mean_doy_{doy_frmt}.gz.parquet")
  message(paste("calculating historical weather means and standard deviations for doy", doy_frmt))
  
  # Check if file already exists
  existing_files <- list.files(weather_historical_means_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(weather_historical_means_directory, save_filename))
  }
  # Open dataset to transformed data
  weather_transformed_dataset <- open_dataset(nasa_weather_directory_transformed)
  
  # Filter for day of year and calculate historical means and standard deviations
  historical_means <- weather_transformed_dataset |> 
    filter(day_of_year == doy) |> 
    group_by(x, y, day_of_year) |> 
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
