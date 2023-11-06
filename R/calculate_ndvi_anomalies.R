#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ndvi_date_lookup
#' @param ndvi_historical_means
#' @param ndvi_anomalies_directory
#' @param model_dates
#' @param model_dates_selected
#' @param lag_intervals
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
calculate_ndvi_anomalies <- function(ndvi_date_lookup, ndvi_historical_means,
                                     ndvi_anomalies_directory,
                                     model_dates_selected, lag_intervals,
                                     overwrite = FALSE) {
  
  # Set filename
  date_selected <- model_dates_selected
  save_filename <- glue::glue("ndvi_anomaly_{date_selected}.gz.parquet")
  message(paste0("Calculating NDVI anomalies for ", date_selected))
  
  # Check if file already exists
  existing_files <- list.files(ndvi_anomalies_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(ndvi_anomalies_directory, save_filename))
  }
  
  # Get the lagged anomalies for selected dates, mapping over the lag intervals
  lag_intervals_start <- c(1 , 1+lag_intervals[-length(lag_intervals)]) # 1 to start with previous day
  lag_intervals_end <- lag_intervals # 30 days total including end day
  
  anomalies <- map2(lag_intervals_start, lag_intervals_end, function(start, end){
    
    # get lag dates, removing doy 366
    lag_dates <- seq(date_selected - end, date_selected - start, by = "day")
    lag_doys <- yday(lag_dates)
    if(366 %in% lag_doys){
      lag_doys <- lag_doys[lag_doys!=366]
      lag_doys <- c(head(lag_doys, 1) - 1, lag_doys)
    }
    
    # Get historical means for lag period
    doy_start <- head(lag_doys, 1)
    doy_end <- tail(lag_doys, 1)
    doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
    doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")
    doy_range <- glue::glue("{doy_start_frmt}_to_{doy_end_frmt}")
    
    historical_means <- read_parquet(ndvi_historical_means[str_detect(ndvi_historical_means, doy_range)]) 
    assertthat::assert_that(nrow(historical_means) > 0)

    # get files and weights for the calculations
    weights <- ndvi_date_lookup |> 
      mutate(lag_date = map(lookup_dates, ~. %in% lag_dates)) |> 
      mutate(weight = unlist(map(lag_date, sum))) |> 
      filter(weight > 0) |> 
      select(start_date, filename, weight)
    
   ndvi_dataset <- open_dataset(weights$filename)
   
   # Lag: calculate mean by pixel for the preceding x days
   lagged_means <- ndvi_dataset |> 
     left_join(weights |> select(-filename))  |> 
        group_by(x, y) |>
        summarize(lag_ndvi_mean = sum(ndvi * weight)/ sum(weight)) |>
        ungroup()
       
      # Join in historical means to calculate anomalies raw and scaled
      full_join(lagged_means, historical_means, by = c("x", "y")) |>
        mutate(!!paste0("anomaly_ndvi_", end) := lag_ndvi_mean - historical_ndvi_mean,
               !!paste0("anomaly_ndvi_scaled_", end) := (lag_ndvi_mean - historical_ndvi_mean)/historical_ndvi_sd) |>
        select(-starts_with("lag"), -starts_with("historical"))
    }) |>
      reduce(left_join, by = c("x", "y")) |>
      mutate(date = date_selected) |>
      relocate(date)
    
    # Save as parquet 
    write_parquet(anomalies, here::here(ndvi_anomalies_directory, save_filename), compression = "gzip", compression_level = 5)
    
    return(file.path(ndvi_anomalies_directory, save_filename))
    
    
    
  }
  