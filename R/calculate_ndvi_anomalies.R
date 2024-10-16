#' Calculate NDVI Anomalies
#'
#' This function calculates the NDVI (Normalized Difference Vegetation Index) anomalies for a specified date.
#' The anomalies are calculated by comparing lagged, weighted daily NDVI values to historical means and standard deviations for overlapping days of the year.
#' The function writes the anomalies into a GZ parquet file and returns its path.
#'
#' @author Emma Mendelsohn
#'
#' @param ndvi_date_lookup A data frame containing the filenames for each day along with their respective dates for retrieval.
#' @param ndvi_historical_means A character vector of file paths to gz parquet files containing historical means for each grid cell and day of the year. 
#' @param ndvi_anomalies_directory The directory to write the gz parquet file of anomalies.
#' @param model_dates_selected Model dates selected.
#' @param lag_intervals A numeric vector defining the start day for each lag period.
#' @param overwrite A boolean indicating whether existing files in the directory for the specified date should be overwritten.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A string containing the path to the gz parquet file written by this function.
#'
#' @note If a file already exists in the directory for the specified date and 'overwrite' is 'FALSE', this function will return the existing file path without performing any calculations.
#'
#' @examples
#' calculate_ndvi_anomalies(ndvi_date_lookup=my_lookup,
#'                          ndvi_historical_means=historical_means,
#'                          ndvi_anomalies_directory='./anomalies',
#'                          model_dates_selected=as.Date('2020-05-01'),
#'                          lag_intervals=c(1, 7, 14, 21, 30),
#'                          overwrite=TRUE)
#'
#' @export
calculate_ndvi_anomalies <- function(ndvi_date_lookup, 
                                     ndvi_historical_means,
                                     ndvi_anomalies_directory,
                                     model_dates_selected, 
                                     lag_intervals,
                                     overwrite = FALSE,
                                     ...) {
  
  # Set filename
  date_selected <- model_dates_selected
  ndvi_anomalies_filename <- file.path(ndvi_anomalies_directory, glue::glue("ndvi_anomaly_{date_selected}.gz.parquet"))
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  # Check if outbreak_history file exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(ndvi_anomalies_filename)) & !overwrite) {
    message(glue::glue("{basename(ndvi_anomalies_filename)} already exists and can be loaded, skipping download and processing."))
    return(ndvi_anomalies_filename)
  }
  
  message(paste0("Calculating NDVI anomalies for ", date_selected))
  
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
    
    historical_means <- arrow::read_parquet(ndvi_historical_means[str_detect(ndvi_historical_means, doy_range)]) 
    assertthat::assert_that(nrow(historical_means) > 0)

    # get files and weights for the calculations
    weights <- ndvi_date_lookup |> 
      mutate(lag_date = map(lookup_dates, ~. %in% lag_dates)) |> 
      mutate(weight = unlist(map(lag_date, sum))) |> 
      filter(weight > 0) |> 
      select(start_date, filename, weight)
    
   ndvi_dataset <- arrow::open_dataset(weights$filename)
   
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
    arrow::write_parquet(anomalies, ndvi_anomalies_filename, compression = "gzip", compression_level = 5)
    
    return(ndvi_anomalies_filename)
  }
  