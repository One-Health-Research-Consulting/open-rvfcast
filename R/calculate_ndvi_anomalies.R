#' Calculate NDVI Anomalies
#'
#' This function calculates the NDVI anomalies using the transformed NDVI data from Sentinel and MODIS satellites,
#' historical NDVI means data, and dates provided for a selected model. The calculated anomalies are saved 
#' as a parquet file in a designated directory.
#'
#' @author Nathan Layman and Emma Mendelsohn
#'
#' @param sentinel_ndvi_transformed Transformed Sentinel NDVI data
#' @param modis_ndvi_transformed Transformed MODIS NDVI data
#' @param ndvi_historical_means Historical NDVI means data
#' @param ndvi_anomalies_directory Directory where the calculated NDVI anomalies should be saved
#' @param model_dates_selected Dates for the selected model
#' @param overwrite Boolean flag indicating whether existing file of NDVI anomalies should be overwritten; Default is FALSE
#' @param ... Additional arguments not used by this function but included for generic function compatibility
#'
#' @return A string containing the filepath to the calculated NDVI anomalies parquet file
#'
#' @note If the parquet file of anomalies already exists at the target filepath and overwrite is set to FALSE,
#' then, the existing file is returned without any calculations
#'
#' @examples
#' calculate_ndvi_anomalies('sentinel_data_transformed', 'modis_data_transformed',
#' 'historical_means', '/directory_path/', 'model_date', overwrite = TRUE)
#'
#' @export> 
calculate_ndvi_anomalies <- function(sentinel_ndvi_transformed,
                                     modis_ndvi_transformed,
                                     ndvi_historical_means,
                                     ndvi_anomalies_directory,
                                     model_dates_selected,
                                     overwrite = FALSE,
                                     ...) {
  
  # Check that we're only working on one date at a time
  stopifnot(length(model_dates_selected) == 1)
  
  # Set filename
  save_filename <- file.path(ndvi_anomalies_directory, glue::glue("ndvi_anomaly_{model_dates_selected}.gz.parquet"))
  message(paste0("Calculating ndvi anomalies for ", model_dates_selected))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(save_filename)
  }
  
  # Open dataset to transformed data
  ndvi_transformed_dataset <- arrow::open_dataset(c(sentinel_ndvi_transformed, modis_ndvi_transformed)) |> 
    filter(date == model_dates_selected)
  
  # Open dataset to historical ndvi data
  historical_means <- arrow::open_dataset(ndvi_historical_means) |> filter(doy == lubridate::yday(model_dates_selected)) 
  
  # Join the two datasets by day of year (doy)
  ndvi_transformed_dataset <- left_join(ndvi_transformed_dataset, historical_means, by = c("x","y","doy"), suffix = c("", "_historical"))
  
  # Calculate ndvi anomalies
  ndvi_transformed_dataset <- ndvi_transformed_dataset |>
    mutate(anomaly_ndvi = ndvi - ndvi_historical,
           anomaly_scaled_ndvi = anomaly_ndvi / ndvi_sd)
  
  # Remove intermediate columns
  ndvi_transformed_dataset <- ndvi_transformed_dataset |> 
    select(x, y, date, starts_with("anomaly"))
  
  # Save as parquet 
  arrow::write_parquet(ndvi_transformed_dataset, save_filename, compression = "gzip", compression_level = 5)
  
  return(save_filename)
}
  
  
  
# calculate_ndvi_anomalies <- function(ndvi_date_lookup, 
#                                      ndvi_historical_means,
#                                      ndvi_anomalies_directory,
#                                      model_dates_selected, 
#                                      lag_intervals,
#                                      overwrite = FALSE,
#                                      ...) {
#   # Set filename
#   date_selected <- model_dates_selected
#   ndvi_anomalies_filename <- file.path(ndvi_anomalies_directory, glue::glue("ndvi_anomaly_{date_selected}.gz.parquet"))
#   
#   # Set up safe way to read parquet files
#   error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
#   
#   # Check if outbreak_history file exist and can be read and that we don't want to overwrite them.
#   if(!is.null(error_safe_read_parquet(ndvi_anomalies_filename)) & !overwrite) {
#     message(glue::glue("{basename(ndvi_anomalies_filename)} already exists and can be loaded, skipping download and processing."))
#     return(ndvi_anomalies_filename)
#   }
#   
#   message(paste0("Calculating NDVI anomalies for ", date_selected))
#   
#   # Get the lagged anomalies for selected dates, mapping over the lag intervals
#   lag_intervals_start <- c(1 , 1+lag_intervals[-length(lag_intervals)]) # 1 to start with previous day
#   lag_intervals_end <- lag_intervals # 30 days total including end day
#   
#   anomalies <- map2(lag_intervals_start, lag_intervals_end, function(start, end){
#     
#     # get lag dates, removing doy 366
#     lag_dates <- seq(date_selected - end, date_selected - start, by = "day")
#     lag_doys <- yday(lag_dates)
#     if(366 %in% lag_doys){
#       lag_doys <- lag_doys[lag_doys!=366]
#       lag_doys <- c(head(lag_doys, 1) - 1, lag_doys)
#     }
#     
#     # Get historical means for lag period
#     doy_start <- head(lag_doys, 1)
#     doy_end <- tail(lag_doys, 1)
#     doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
#     doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")
#     doy_range <- glue::glue("{doy_start_frmt}_to_{doy_end_frmt}")
#     
#     historical_means <- arrow::read_parquet(ndvi_historical_means[str_detect(ndvi_historical_means, doy_range)]) 
#     assertthat::assert_that(nrow(historical_means) > 0)
# 
#     # get files and weights for the calculations
#     weights <- ndvi_date_lookup |> 
#       mutate(lag_date = map(lookup_dates, ~. %in% lag_dates)) |> 
#       mutate(weight = unlist(map(lag_date, sum))) |> 
#       filter(weight > 0) |> 
#       select(start_date, filename, weight)
#     
#    ndvi_dataset <- arrow::open_dataset(weights$filename)
#    
#    # Lag: calculate mean by pixel for the preceding x days
#    lagged_means <- ndvi_dataset |> 
#      left_join(weights |> select(-filename))  |> 
#         group_by(x, y) |>
#         summarize(lag_ndvi_mean = sum(ndvi * weight)/ sum(weight)) |>
#         ungroup()
#        
#       # Join in historical means to calculate anomalies raw and scaled
#       full_join(lagged_means, historical_means, by = c("x", "y")) |>
#         mutate(!!paste0("anomaly_ndvi_", end) := lag_ndvi_mean - historical_ndvi_mean,
#                !!paste0("anomaly_ndvi_scaled_", end) := (lag_ndvi_mean - historical_ndvi_mean)/historical_ndvi_sd) |>
#         select(-starts_with("lag"), -starts_with("historical"))
#     }) |>
#       reduce(left_join, by = c("x", "y")) |>
#       mutate(date = date_selected) |>
#       relocate(date)
#     
#     # Save as parquet 
#     arrow::write_parquet(anomalies, ndvi_anomalies_filename, compression = "gzip", compression_level = 5)
#     
#     return(ndvi_anomalies_filename)
#   }
#   