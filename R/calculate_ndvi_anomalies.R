#' Calculate NDVI Anomalies
#'
#' This function calculates the NDVI anomalies using the transformed NDVI data,
#' historical NDVI means data, and dates provided for a selected model. The calculated anomalies are saved
#' as a parquet file in a designated directory.
#'
#' @author Nathan Layman and Emma Mendelsohn
#'
#' @param ndvi_transformed A single month's transformed NDVI parquet file.
#' @param ndvi_historical_means Historical NDVI means data
#' @param ndvi_anomalies_directory Directory where the calculated NDVI anomalies should be saved
#' @param basename_template Template for the output filename with a placeholder for date
#' @param overwrite Boolean flag indicating whether existing file of NDVI anomalies should be overwritten; Default is FALSE
#' @param ... Additional arguments not used by this function but included for generic function compatibility
#'
#' @return A character vector containing filepaths to all files created for dates in this month.
#'
#' @note This function processes all dates within the month contained in ndvi_transformed.
#' It creates one output file per date.
#'
#' @examples
#' calculate_ndvi_anomalies(
#'   ndvi_transformed = "data/ndvi_transformed/ndvi_transformed_2020_01.parquet",
#'   ndvi_historical_means = './data/historical_means',
#'   ndvi_anomalies_directory = '/directory_path/',
#'   overwrite = TRUE
#' )
#'
#' @export
calculate_ndvi_anomalies <- function(ndvi_transformed,
                                     ndvi_historical_means,
                                     ndvi_anomalies_directory,
                                     basename_template = "ndvi_anomaly_{date}.parquet",
                                     overwrite = FALSE,
                                     ...) {

  # Check if input file exists
  if (length(ndvi_transformed) == 0 || is.na(ndvi_transformed)) {
    stop("ndvi_transformed is empty or NA - no NDVI file available for this month")
  }

  if (!file.exists(ndvi_transformed)) {
    stop(glue::glue("NDVI file does not exist: {ndvi_transformed}"))
  }

  # Read the NDVI data for this month
  ndvi_data <- arrow::read_parquet(ndvi_transformed)

  # Get all unique dates in the parquet file
  dates_in_month <- unique(ndvi_data$date)

  if (length(dates_in_month) == 0) {
    message("No dates found in month file")
    return(character(0))
  }

  message(paste0("Processing ", length(dates_in_month), " dates in month"))

  # Process each date and collect output filenames
  output_files <- purrr::map_chr(dates_in_month, function(date) {

    # Set filename for this date
    save_filename <- file.path(ndvi_anomalies_directory, glue::glue(basename_template))

    # Check if file already exists and can be read
    error_safe_read_parquet <- purrr::possibly(arrow::read_parquet, NULL)
    existing_dataset <- error_safe_read_parquet(save_filename)

    if(!is.null(existing_dataset) & !overwrite) {
      # Check if file has data - if zero rows, overwrite anyway
      if(nrow(existing_dataset) > 0) {
        message(glue::glue("{basename(save_filename)} already exists, has rows, and overwrite is not TRUE, skipping"))
        return(save_filename)
      } else {
        message(glue::glue("{basename(save_filename)} exists but has zero rows, overwriting"))
      }
    } else if(!is.null(existing_dataset) & overwrite) {
      # File exists and overwrite is TRUE
      message(glue::glue("Overwriting existing {basename(save_filename)} for ", date))
    } else {
      # File doesn't exist - we're calculating
      message(paste0("Calculating ndvi anomalies for ", date))
    }

    # Filter to this specific date
    ndvi_transformed_dataset <- ndvi_data |>
      dplyr::filter(date == !!date) |>
      # Round coordinates to guard against floating-point drift when the raster
      # template is recomputed across pipeline runs; 7dp is well within the
      # 0.1-degree grid resolution so no actual spatial information is lost
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    doy_to_process <- as.numeric(lubridate::yday(date))

    # Open dataset to historical ndvi data
    historical_means <- arrow::open_dataset(ndvi_historical_means) |>
      dplyr::filter(doy == doy_to_process) |>
      dplyr::collect() |>
      tidyr::drop_na() |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    # Join the two datasets by day of year (doy)
    ndvi_transformed_dataset <- dplyr::left_join(ndvi_transformed_dataset, historical_means,
                                          by = c("x", "y", "doy"),
                                          suffix = c("", "_historical")) |>
      tidyr::drop_na()

    # Calculate ndvi anomalies
    ndvi_transformed_dataset <- ndvi_transformed_dataset |>
      dplyr::mutate(
        anomaly_ndvi = ndvi - ndvi_historical,
        anomaly_scaled_ndvi = anomaly_ndvi / ndvi_sd
      )

    # Select and format final columns
    ndvi_transformed_dataset <- ndvi_transformed_dataset |>
      dplyr::mutate(
        doy = as.integer(lubridate::yday(date)),
        month = as.integer(lubridate::month(date)),
        year = as.integer(lubridate::year(date))
      ) |>
      dplyr::select(x, y, date, doy, month, year, dplyr::starts_with("anomaly"))

    # Save as parquet
    arrow::write_parquet(ndvi_transformed_dataset, save_filename,
                        compression = "gzip", compression_level = 5)

    return(save_filename)
  })

  return(output_files)
}
  
  
  
# calculate_ndvi_anomalies <- function(ndvi_date_lookup, 
#                                      ndvi_historical_means,
#                                      ndvi_anomalies_directory,
#                                      dates_to_process, 
#                                      lag_intervals,
#                                      overwrite = FALSE,
#                                      ...) {
#   # Set filename
#   date_selected <- dates_to_process
#   ndvi_anomalies_filename <- file.path(ndvi_anomalies_directory, glue::glue("ndvi_anomaly_{date_selected}.parquet"))
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
