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
process_weather_data <- function(nasa_weather_dataset, # enforce dependency
                                 nasa_weather_directory_dataset,
                                 nasa_weather_anomalies_directory_dataset,
                                 model_dates,
                                 model_dates_selected,
                                 lag_intervals,
                                 overwrite = FALSE) {
  
  date_selected <- model_dates_selected
  save_filename <- glue::glue("{date_selected}.gz.parquet")
  
  existing_files <- list.files(nasa_weather_anomalies_directory_dataset)
  
  message(paste0("Calculating anomalies for ", date_selected))
  
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(nasa_weather_anomalies_directory_dataset, save_filename))
  }
  
  weather_dataset <- open_dataset(nasa_weather_directory_dataset) #|> to_duckdb(table_name = "weather")
  
  # TODO this could go into create_nasa_weather_dataset() to avoid repeating it on each branch
  weather_dataset <- weather_dataset |> 
    mutate(across(c(year, month, day, day_of_year), as.integer)) |> 
    mutate(year_day_of_year = paste(year, day_of_year, sep = "_"))  |> 
    mutate(date = lubridate::make_date(year, month, day)) |> 
    select(x, y, date, year, month, day, day_of_year, year_day_of_year, relative_humidity, temperature, precipitation)
  
  # generate the weather dataset - get the lagged anomolies for selected dates
  # map over the lag intervals
  row_select <- which(model_dates$date == date_selected)
  
  lag_intervals_start <- c(1 , 1+lag_intervals[-length(lag_intervals)])
  lag_intervals_end <- lag_intervals
  
  anomalies <- map2(lag_intervals_start, lag_intervals_end, function(start, end){
    lag_dates <- model_dates |> slice((row_select - start):(row_select - end))
    
    # lag: calculate mean by pixel for the preceding x days
    lagged_means <- weather_dataset |> 
      filter(date %in% !!lag_dates$date) |> 
      group_by(x, y) |> 
      summarize(!!paste0("lag_relative_humidity_", end) := mean(relative_humidity),
                !!paste0("lag_temperature_", end) := mean(temperature),
                !!paste0("lag_precipitation_", end) := mean(precipitation)) |> 
      ungroup() 
    
    # historical: calculate mean across the full dataset for the days of the year covered by the lag period
    # note when 366 is included, we'll have less historical data going into the mean. This is okay since it's one of 30 values
    # the same this would happen if we did this by date (we'd have sparse data for feb-29)
    # it would be avoided if we did weighted monthly means
    historical_means <- weather_dataset |> 
      filter(day_of_year %in% !!lag_dates$day_of_year ) |> 
      group_by(x, y) |> 
      summarize(!!paste0("historical_relative_humidity_", end) := mean(relative_humidity),
                !!paste0("historical_temperature_", end) := mean(temperature),
                !!paste0("historical_precipitation_", end) := mean(precipitation)) |> 
      ungroup() 
    
    # anomaly
    full_join(lagged_means, historical_means, by = c("x", "y")) |> 
      mutate(!!paste0("anomaly_relative_humidity_", end) := !!sym(paste0("lag_relative_humidity_", end))  - !!sym(paste0("historical_relative_humidity_", end)),
             !!paste0("anomaly_temperature_", end) := !!sym(paste0("lag_temperature_", end))  -  !!sym(paste0("historical_temperature_", end)),
             !!paste0("anomaly_precipitation_", end) := !!sym(paste0("lag_precipitation_", end)) - !!sym(paste0("historical_precipitation_", end)))
  }) |> 
    reduce(left_join, by = c("x", "y"))
  
  # get selected day info and pull in all calculated data
  date_selected_all_dat <- weather_dataset |> 
    filter(date == !!date_selected) |> 
    full_join(anomalies,  by = c("x", "y"))
  
  # Save as parquet 
  write_dataset(date_selected_all_dat, here::here(nasa_weather_anomalies_directory_dataset, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(nasa_weather_anomalies_directory_dataset, save_filename))
  
  
}
