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
calculate_ndvi_historical_means <- function(sentinel_ndvi_transformed,
                                            sentinel_ndvi_transformed_directory,
                                            modis_ndvi_transformed,
                                            modis_ndvi_transformed_directory,
                                            ndvi_historical_means_directory,
                                            ndvi_date_lookup, days_of_year,
                                            overwrite = FALSE) {
  
  # Set filename
  doy <- days_of_year
  doy_frmt <- str_pad(doy,width = 3, side = "left", pad = "0")
  save_filename <- glue::glue("historical_ndvi_mean_doy_{doy_frmt}.gz.parquet")
  message(paste("calculating historical ndvi means and standard deviations for doy", doy_frmt))
  
  # Check if file already exists
  existing_files <- list.files(ndvi_historical_means_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(ndvi_historical_means_directory, save_filename))
  }
  
  # Get relevant NDVI intervals
  doy_lookup <-  ndvi_date_lookup |> 
    filter(map_lgl(lookup_day_of_year, ~any(. == doy)))
  
  # Create dataset of relevant files
  ndvi_dataset <- open_dataset(doy_lookup$filename)
  
  # Calculate historical means and standard deviations
  historical_means <- ndvi_dataset |> 
    mutate(day_of_year = doy) |> 
    group_by(x, y, day_of_year) |> 
    summarize(historical_ndvi_mean = mean(ndvi),
              historical_ndvi_sd = sd(ndvi)) |> 
    ungroup() 

  # Save as parquet 
  write_parquet(historical_means, here::here(ndvi_historical_means_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(ndvi_historical_means_directory, save_filename))
  
}
