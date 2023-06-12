#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nasa_weather_downloaded
#' @param continent_raster_template
#' @param nasa_weather_directory_dataset
#' @return
#' @author Emma Mendelsohn
#' @export
transform_nasa_weather <- function(nasa_weather_downloaded,
                                   continent_raster_template,
                                   nasa_weather_directory_dataset,
                                   overwrite = FALSE) {
  
  filename <- basename(nasa_weather_downloaded)
  save_filename <- glue::glue("transformed_{filename}")
  
  existing_files <- list.files(nasa_weather_directory_dataset)
  
  message(paste0("Transforming ", save_filename))
  
  if(save_filename %in% existing_files & !overwrite){
    message("file already exists, skipping transform")
    return(file.path(nasa_weather_directory_dataset, save_filename))
  }
  
  raw_flat <- arrow::read_parquet(nasa_weather_downloaded)
  raw_flat <- as_tibble(raw_flat) |> 
    janitor::clean_names() |> 
    rename(relative_humidity = rh2m, temperature = t2m, precipitation= prectotcorr) |> 
    select(lon, lat, everything(), -yyyymmdd)   # terra::rast - the first with x (or longitude) and the second with y (or latitude) coordinates 
    
  check_rows <- raw_flat |> group_by(doy) |> count() |> ungroup() |> distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  
  dat_out <- raw_flat |> 
    group_split(doy) |> 
    map_dfr(function(daily){
      raw_raster <- terra::rast(daily) 
      crs(raw_raster) <-  crs(rast()) 
      transformed_raster <- transform_raster(raw_raster = raw_raster,
                                             template = rast(continent_raster_template))
      as.data.frame(transformed_raster, xy = TRUE) 
    })
  
  # Save as parquet 
  write_parquet(dat_out, here::here(nasa_weather_directory_dataset, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(nasa_weather_directory_dataset, save_filename))
  

}
