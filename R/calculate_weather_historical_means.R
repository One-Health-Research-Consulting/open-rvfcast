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
                                               weather_historical_means_directory) {
  
  weather_dataset <- open_dataset(nasa_weather_directory_transformed) #|> to_duckdb(table_name = "weather")
  
  historical_means <- weather_dataset |> 
    group_by(x, y, day_of_year) |> 
    summarize(historical_relative_humidity = mean(relative_humidity),
              historical_temperature = mean(temperature),
              historical_precipitation = mean(precipitation)) |> 
    ungroup() |> 
    group_by(day_of_year) |> 
    write_dataset(weather_historical_means_directory)
  
  return(list.files(weather_historical_means_directory, full.names = TRUE, recursive = TRUE))
  

}
