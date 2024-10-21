#' Calculate and Save Anomalies from Forecast Data
#'
#' This function takes transformed ECMWF forecast and historical weather mean data, 
#' calculates anomalies, and saves them in a specified directory. If the file already exists and `overwrite` is FALSE,
#' the existing file's path is returned. Otherwise, the existing file is overwritten.
#'
#' @author Emma Mendelsohn
#'
#' @param ecmwf_forecasts_transformed_directory Directory containing the transformed forecasts.
#' @param weather_historical_means Filepath to the historical weather means data.
#' @param forecasts_anomalies_directory Directory in which to save the anomalies data.
#' @param model_dates_selected Dates for models that have been selected.
#' @param lead_intervals Lead times for forecasts, which will determine the interval over which anomalies are averaged.
#' @param overwrite Boolean flag indicating whether existing file should be overwritten. Default is FALSE.
#' @param ... Additional unused arguments for future extensibility and function compatibility.
#'
#' @return A string containing the filepath to the anomalies data.
#'
#' @note The returned path either points to an existing file (when overwrite is FALSE and the file already exists) 
#' or to a newly created file with calculated anomalies (when overwrite is TRUE or the file didn't exist).
#'
#' @examples
#' calculate_forecasts_anomalies(ecmwf_forecasts_transformed_directory = './forecasts',
#'                               weather_historical_means='./historical_means.parquet',
#' forecast_anomalies_directory = './anomalies',
#' model_dates_selected = as.Date('2000-01-01'),
#' lead_intervals = c(1, 10),
#' overwrite = TRUE)
#'
#' @export
calculate_forecasts_anomalies <- function(ecmwf_forecasts_transformed_directory,
                                          weather_historical_means,
                                          forecasts_anomalies_directory,
                                          model_dates_selected,
                                          lead_intervals,
                                          overwrite = FALSE,
                                          ...) {
  
  # Set filename
  date_selected <- model_dates_selected
  save_filename <- glue::glue("forecast_anomaly_{date_selected}.gz.parquet")
  message(paste0("Calculating forecast anomalies for ", date_selected))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  if(!is.null(error_safe_read_parquet(file.path(forecasts_validate_directory, save_filename))) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(file.path(forecasts_anomalies_directory, save_filename))
  }
  
  # Open dataset to transformed data
  forecasts_transformed_dataset <- open_dataset(ecmwf_forecasts_transformed_directory)
  
  # Get the forecasts anomalies for selected dates, mapping over the lead intervals
  lead_intervals_start <- c(0 , lead_intervals[-length(lead_intervals)]) # 0 to include current day in forecast
  lead_intervals_end <- lead_intervals - 1 # -1 for 30 days total include start day
  
  anomalies <- map(1:length(lead_intervals_start), function(i){
    
    # subset to start and end day of interval
    start <- lead_intervals_start[i]
    end <- lead_intervals_end[i]
    
    lead_start_date <- date_selected + start
    lead_end_date <- date_selected + end 
    
    # lead months for subsetting
    lead_months <- as.character(c(i, i+1))
    
    # this is the date from which the forecasts are made
    baseline_date <- floor_date(date_selected, unit = "month")
    
    # calculate weights
    weight_a <- as.integer(days_in_month(lead_start_date) - day(lead_start_date)) + 1 # include current date
    weight_b <- day(lead_end_date) - 1
    
    # get weighted mean of forecasts means
    lead_means <- forecasts_transformed_dataset |> 
      filter(data_date == baseline_date) |> 
      filter(lead_month %in% lead_months) |> 
      mutate(weight = case_when(lead_month == !!lead_months[1] ~ !!weight_a, 
                                lead_month == !!lead_months[2] ~ !!weight_b)) |> 
      group_by(x, y, short_name) |>
      summarize(lead_mean = sum(mean * weight)/ sum(weight)) |>
      ungroup() 
    
    # get historical means for lead period, removing doy 366
    lead_dates <- seq(lead_start_date, lead_end_date, by = "day")
    lead_doys <- yday(lead_dates)
    if(366 %in% lead_doys) {
      if(tail(lead_doys, 1) == 366){
        lead_doys <- lead_doys[lead_doys!=366]
        lead_doys <- c(lead_doys, 1)
      }else{
        lead_doys <- lead_doys[lead_doys!=366]
        lead_doys <- c(lead_doys, tail(lead_doys, 1) + 1)
      }
    }
    
    doy_start <- head(lead_doys, 1)
    doy_end <- tail(lead_doys, 1)
    doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
    doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")
    doy_range <- glue::glue("{doy_start_frmt}_to_{doy_end_frmt}")
    
    historical_means <- read_parquet(weather_historical_means[str_detect(weather_historical_means, doy_range)]) 
    assertthat::assert_that(nrow(historical_means) > 0)
    
    # calculate anomalies - a bit inefficient because arrow doesn't allow reshaping (should have done so in the transform function)
    # NAs are expected because forecasts are for the whole continent, weather is just for areas of interest
    
    temp_anomalies <- lead_means |> 
      filter(short_name == "2t") |> 
      left_join(historical_means |> select(x, y, contains("temperature")), by = c("x", "y")) |> 
      mutate(!!paste0("anomaly_temperature_forecast_", end) := lead_mean  -  historical_temperature_mean,
             !!paste0("anomaly_temperature_scaled_forecast_", end) := (lead_mean  -  historical_temperature_mean)/historical_temperature_sd) |> 
      select(-short_name, -lead_mean, -starts_with("historical")) |> 
      filter(!is.na(!!sym(paste0("anomaly_temperature_forecast_", end))))
    
    rh_anomalies <- lead_means |> 
      filter(short_name == "rh") |> 
      left_join(historical_means |> select(x, y, contains("humidity")), by = c("x", "y")) |> 
      mutate(!!paste0("anomaly_relative_humidity_forecast_", end) := lead_mean - historical_relative_humidity_mean,
             !!paste0("anomaly_relative_humidity_scaled_forecast_", end) := (lead_mean  -  historical_relative_humidity_mean)/historical_relative_humidity_sd) |> 
      select(-short_name, -lead_mean, -starts_with("historical")) |> 
      filter(!is.na(!!sym(paste0("anomaly_relative_humidity_forecast_", end))))
    
    precip_anomalies <- lead_means |> 
      filter(short_name == "tprate") |> 
      left_join(historical_means |> select(x, y, contains("precipitation")), by = c("x", "y")) |> 
      mutate(!!paste0("anomaly_precipitation_forecast_", end) := lead_mean  -  historical_precipitation_mean,
             !!paste0("anomaly_precipitation_scaled_forecast_", end) := (lead_mean  -  historical_precipitation_mean)/historical_precipitation_sd) |> 
      select(-short_name, -lead_mean, -starts_with("historical")) |> 
      filter(!is.na(!!sym(paste0("anomaly_precipitation_forecast_", end))))
    
    left_join(temp_anomalies, rh_anomalies, by = c("x", "y")) |> 
      left_join(precip_anomalies, by = c("x", "y"))
    
  }) |> 
    reduce(left_join, by = c("x", "y")) |> 
    mutate(date = date_selected) |> 
    relocate(date)
  
  # Save as parquet 
  write_parquet(anomalies, here::here(forecasts_anomalies_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(forecasts_anomalies_directory, save_filename))
  
}
