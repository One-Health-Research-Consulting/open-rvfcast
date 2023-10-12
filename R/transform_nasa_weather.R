#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nasa_weather_downloaded
#' @param continent_raster_template
#' @param nasa_weather_directory_transformed
#' @return
#' @author Emma Mendelsohn
#' @export
transform_nasa_weather <- function(nasa_weather_pre_transformed,
                                   nasa_weather_directory_transformed, 
                                   continent_raster_template,
                                   overwrite = FALSE) {
  
  
  # Get filename for saving from the pre-processed arrow dataset
  save_filename <-  sub(".*/(year=\\d{4}/part-\\d\\.parquet)", "\\1", nasa_weather_pre_transformed)
  message(paste0("Transforming ", save_filename))
  
  # Check if file already exists
  existing_files <- list.files(nasa_weather_directory_transformed)
  if(dirname(save_filename) %in% existing_files & !overwrite){
    message("files already exist, skipping transform")
    return(file.path(nasa_weather_directory_transformed, save_filename))
  }
  
  # Read in continent template raster
  continent_raster_template <- rast(continent_raster_template)
  
  # Read in pre-processed arrow dataset, make sure the first two columns are x and y
  raw_flat <- arrow::read_parquet(nasa_weather_pre_transformed) |> 
    mutate(year = as.integer(year(date)))
  assertthat::assert_that(names(raw_flat)[1]=="x")
  assertthat::assert_that(names(raw_flat)[2]=="y")
  
  # Check for even data coverage
  check_rows <- raw_flat |> group_by(x, y) |> count() |> ungroup() |> distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  check_rows <- raw_flat |> group_by(day_of_year) |> count() |> ungroup() |> distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  
  # For 2023, there are NAs for the last day of the year
  # TODO make this a less risky step
  raw_flat <- drop_na(raw_flat)
  
  # Split by day of year and transform with template raster. Return as a row-binded dataframe
  dat_out <- raw_flat |> 
    group_split(month, day, year, day_of_year, date) |> 
    map_dfr(function(daily){
      daily_info <- distinct(daily, month, day, year, day_of_year, date)
      daily_rast <- select(daily, -month, -day, -year, -day_of_year, -date)
      raw_raster <- terra::rast(daily_rast) 
      crs(raw_raster) <-  crs(rast()) 
      transformed_raster <- transform_raster(raw_raster = raw_raster,
                                             template = continent_raster_template)
      cbind(daily_info, as.data.frame(transformed_raster, xy = TRUE)) |> 
        select(x, y, everything())
    }) 
  

  # This crashes r
  # dat_out |> 
  #   group_by(year, month) |> 
  #   write_dataset(nasa_weather_directory_transformed)
  
  # Save as parquet 
  suppressWarnings(dir.create(here::here(nasa_weather_directory_transformed, dirname(save_filename)))) # unnecessary but matching structure of pretransformed dataset
  write_parquet(dat_out, here::here(nasa_weather_directory_transformed, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(nasa_weather_directory_transformed, save_filename))
  
}
