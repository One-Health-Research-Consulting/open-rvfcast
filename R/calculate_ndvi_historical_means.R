#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sentinel_ndvi_transformed
#' @param sentinel_ndvi_transformed_directory
#' @param modis_ndvi_transformed
#' @param modis_ndvi_transformed_directory
#' @param ndvi_date_lookup
#' @param days_of_year
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
calculate_ndvi_historical_means <- function(ndvi_historical_means_directory,
                                            ndvi_date_lookup, days_of_year,
                                            lag_intervals,
                                            overwrite = FALSE) {
  
  interval_length <- unique(diff(lag_intervals))

  # Set filename
  # use dummy dates to keep date logic
  doy_start <- days_of_year
  dummy_date_start  <- ymd("20210101") + doy_start - 1
  dummy_date_end  <- dummy_date_start + interval_length - 1
  doy_end <- yday(dummy_date_end)
  
  doy_start_frmt <- str_pad(doy_start, width = 3, side = "left", pad = "0")
  doy_end_frmt <- str_pad(doy_end, width = 3, side = "left", pad = "0")
  
  save_filename <- glue::glue("historical_ndvi_mean_doy_{doy_start_frmt}_to_{doy_end_frmt}.gz.parquet")
  message(paste("calculating historical ndvi means and standard deviations for doy", doy_start_frmt, "to", doy_end_frmt))
  
  # Check if file already exists
  existing_files <- list.files(ndvi_historical_means_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(ndvi_historical_means_directory, save_filename))
  }
  
  # Get for relevant days of the year
  doy_select <- yday(seq(dummy_date_start, dummy_date_end, by = "day"))
  
  # Get relevant NDVI files and weights for the calculations
  weights <- ndvi_date_lookup |> 
    mutate(lag_doy = map(lookup_day_of_year, ~. %in% doy_select)) |> 
    mutate(weight = unlist(map(lag_doy, sum))) |> 
    filter(weight > 0) |> 
    select(start_date, filename, weight)
  
  ndvi_dataset <- open_dataset(weights$filename) |> 
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

  historical_means <- left_join(historical_means, historical_sds)
  
  # Save as parquet 
  write_parquet(historical_means, here::here(ndvi_historical_means_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(ndvi_historical_means_directory, save_filename))
  
}
