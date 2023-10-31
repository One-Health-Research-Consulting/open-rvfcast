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
                                          model_dates, lead_intervals,
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
  lead_intervals_start <- c(0 , lead_intervals[-length(lead_intervals)])
  lead_intervals_end <- lead_intervals
  
  anomalies <- map(1:length(lead_intervals_start), function(i){
   
    start <- lead_intervals_start[i]
    end <- lead_intervals_end[i]
    
    start_date <- date_selected + start
    end_date <- date_selected + end
    
    lead_months <- as.character(c(i, i+1))
    baseline_date <- floor_date(start_date, unit = "month")
    
    # calculate weights
    weight_a <- as.integer(days_in_month(start_date) - day(start_date)) + 1 # include current date
    weight_b <- day(end_date) - 1
    
    # get weighted mean of forecast
    forecasts_transformed_dataset |> 
      filter(data_date == baseline_date) |> 
      filter(lead_month %in% lead_months) |> 
      mutate(weight = case_when(lead_month == lead_months[1] ~ weight_a, 
                                lead_month == lead_months[2] ~ weight_b)) |> 
      group_by(x, y, short_name) |>
      summarize(lead_mean = sum(mean * weight)/ sum(weight)) |>
      ungroup() |> 
      head(5) |> collect()
    
    # bring in historical means
    
    # reshape and label with interval

  }) |> 
    reduce(left_join, by = c("x", "y")) |> 
    mutate(date = date_selected) |> 
    relocate(date)
    
}