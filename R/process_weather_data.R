#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sentinel_ndvi_transformed
#' @param nasa_weather_transformed
#' @return
#' @author Emma Mendelsohn
#' @export
process_weather_data <- function(nasa_weather_directory_dataset, nasa_weather_dataset, model_dates) {
  
  weather_dataset <- open_dataset(nasa_weather_directory_dataset) |> to_duckdb(table_name = "weather")
  
  weather_dataset <- weather_dataset |> 
    mutate(across(c(year, month, day, day_of_year), as.integer)) |> 
    mutate(year_day_of_year = paste(year, day_of_year, sep = "_"))  
  
  # generate the weather dataset - get the lagged anomolies for selected dates
  # TODO: do this for each lag internal
  outt <-  map(model_dates$date[model_dates$select_date], function(date_selected){
    
    row_select <- which(model_dates$date == date_selected)
    lag_dates <- model_dates |> slice((row_select - 30):(row_select - 1))
    
    # lag: calculate mean by pixel for the preceeding 30 days
    lagged_means <- weather_dataset |> 
      filter(year_day_of_year %in% !!lag_dates$year_day_of_year) |> 
      group_by(x, y) |> 
      summarize(lag_relative_humidity = mean(relative_humidity),
                lag_temperature = mean(temperature),
                lag_precipitation = mean(precipitation)) |> 
      ungroup() 
    
    # overall: calculate mean across the full dataset for the days of the year covered by the lag period
    # note when 366 is included, we'll have less overall data going into the mean. This is okay since it's one of 30 values
    # the same this would happen if we did this by date (we'd have sparse data for feb-29)
    # it would be avoided if we did weighted monthly means
    overall_means <- weather_dataset |> 
      filter(day_of_year %in% !!lag_dates$day_of_year ) |> 
      group_by(x, y) |> 
      summarize(overall_relative_humidity = mean(relative_humidity),
                overall_temperature = mean(temperature),
                overall_precipitation = mean(precipitation)) |> 
      ungroup() 
    
    anomolies <- full_join(lagged_means, overall_means, by = c("x", "y")) |> 
      mutate(year_day_of_year = date_selected)
    
    
    #TODO  
    # join the lagged mean with the historical mean and calculate anomoly
    # join with the data from the actual day, one row for each x, y, selected date
    
  })
  
}
