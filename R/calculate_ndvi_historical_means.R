#' Calculate NDVI Historical Means
#'
#' This function calculates historical Normalized Difference Vegetation Index (NDVI) means and standard deviations for certain days of the year 
#' for a given interval range. The results are saved in a specified directory as a parquet file with gzip compression.
#'
#' @author Emma Mendelsohn
#'
#' @param ndvi_historical_means_directory Path to the directory where the output parquet files will be stored.
#' @param ndvi_date_lookup Table containing NDVI data lookup information.
#' @param days_of_year Days of the year for which the NDVI means are calculated (numeric vector, e.g. 1:365).
#' @param lag_intervals Vector of lag intervals for which the NDVI means are calculated.
#' @param overwrite Boolean flag indicating whether existing files should be overwritten. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for function compatibility.
#'
#' @return The string with path to the saved parquet file.
#'
#' @note If the output file already exists and the param overwrite is set to FALSE, the existing file is returned.
#'
#' @examples
#' calculate_ndvi_historical_means(ndvi_historical_means_directory = './data', ndvi_date_lookup = lookup_table,
#'                                 days_of_year = c(100:200), lag_intervals = c(30, 60, 90), overwrite = TRUE)
#'
#' @export
calculate_weather_historical_means <- function(nasa_weather_transformed,
                                               weather_historical_means_directory,
                                               ...) {
  
  # Open dataset can handle multi-file datasets larger than can
  # fit in memory
  nasa_weather_data <- arrow::open_dataset(nasa_weather_transformed)
  
  # Fast because we can avoid collecting until write_parquet
  weather_historical_means <- map_vec(1:366, .progress = TRUE, function(i) {
    filename <- file.path(weather_historical_means_directory, 
                          glue::glue("weather_historical_mean_doy_{i}"))
    nasa_weather_data |> 
      filter(doy == i) |>
      group_by(x, y, doy) |> 
      summarize(across(matches("temperature|precipitation|humidity"), ~mean(.x)),
                across(matches("temperature|precipitation|humidity"), ~sd(.x), .names = "{.col}_sd")) |>
      arrow::write_parquet(filename)
    
    filename
  })
  
  return(weather_historical_means)
}


calculate_ndvi_historical_means <- function(sentinel_ndvi_transformed,
                                            modis_ndvi_transformed,
                                            ndvi_historical_means_directory,
                                            ndvi_historical_means_AWS,
                                            ...) {

  ndvi_data <- arrow::open_dataset(c(sentinel_ndvi_transformed, modis_ndvi_transformed))
  
  # Fast because we can avoid collecting until write_parquet
  ndvu_historical_means <- map_vec(1:366, .progress = TRUE, function(i) {
    filename <- file.path(ndvi_historical_means_directory, 
                          glue::glue("ndvi_historical_mean_doy_{i}"))
    ndvi_data |> 
      filter(doy == i) |>
      group_by(x, y, doy) |> 
      summarize(across(matches("temperature|precipitation|humidity"), ~mean(.x)),
                across(matches("temperature|precipitation|humidity"), ~sd(.x), .names = "{.col}_sd")) |>
      arrow::write_parquet(filename)
    
    filename
  })
  
  return(weather_historical_means)
  
  interval_length <- unique(diff(lag_intervals))

  # Set filename
  # use dummy dates to keep date logic
  doy_start <- days_of_year
  dummy_date_start  <- ymd("20210101") + doy_start - 1
  dummy_date_end  <- dummy_date_start + interval_length - 1
  doy_end <- yday(dummy_date_end)

  doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
  doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")

  ndvi_historical_means_filename <- file.path(ndvi_historical_means_directory, glue::glue("historical_ndvi_mean_doy_{doy_start_frmt}_to_{doy_end_frmt}.gz.parquet"))

  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

  # Check if outbreak_history file exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(ndvi_historical_means_filename)) & !overwrite) {
    message(glue::glue("{basename(ndvi_historical_means_filename)} already exists and can be loaded, skipping download and processing."))
    return(ndvi_historical_means_filename)
  }

  message(glue::glue("processing {ndvi_historical_means_filename}"))

  # Get for relevant days of the year
  doy_select <- yday(seq(dummy_date_start, dummy_date_end, by = "day"))

  # Get relevant NDVI files and weights for the calculations
  weights <- ndvi_date_lookup |>
    mutate(lag_doy = map(lookup_day_of_year, ~. %in% doy_select)) |>
    mutate(weight = unlist(map(lag_doy, sum))) |>
    filter(weight > 0) |>
    select(start_date, filename, weight)

  ndvi_dataset <- arrow::open_dataset(weights$filename) |>
    left_join(weights |> select(-filename))

  # Calculate weighted means
  historical_means <- ndvi_dataset |>
    group_by(x, y) |>
    summarize(historical_ndvi_mean = sum(ndvi * weight)/ sum(weight))  |>
    ungroup()

  # Calculate weighted standard deviations, using weighted mean from previous step
  historical_sds <- ndvi_dataset |>
    left_join(historical_means) |>
    group_by(x, y) |>
    summarize(historical_ndvi_sd = sqrt(sum(weight * (ndvi - historical_ndvi_mean)^2) / (sum(weight)-1)) ) |>
    ungroup()

  historical_means <- left_join(historical_means, historical_sds) |>
    mutate(doy_start = doy_start,
           doy_end = doy_end)

  # Save as parquet
  write_parquet(historical_means, ndvi_historical_means_filename, compression = "gzip", compression_level = 5)

  return(ndvi_historical_means_filename)

}

