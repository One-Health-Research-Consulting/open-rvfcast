#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ecmwf_forecasts_transformed
#' @param ecmwf_forecasts_transformed_directory
#' @param weather_historical_means
#' @param forecast_anomalies_directory
#' @param model_dates
#' @param model_dates_selected
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
calculate_forecasts_anomalies <- function(ecmwf_forecasts_transformed,
                                          ecmwf_forecasts_transformed_directory,
                                          weather_historical_means,
                                          forecasts_anomalies_directory,
                                          model_dates_selected,
                                          lead_intervals,
                                          overwrite = FALSE) {
  
  # Set filename
  date_selected <- model_dates_selected
  save_filename <- glue::glue("forecast_anomaly_{date_selected}.gz.parquet")
  message(paste0("Calculating forecast anomalies for ", date_selected))
  
  # Check if file already exists
  existing_files <- list.files(forecasts_anomalies_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
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
    
    # get historical means for lead period
    doy_start <- yday(lead_start_date)
    doy_end <- yday(lead_end_date)
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