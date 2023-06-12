#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nasa_weather_downloaded
#' @param continent_raster_template
#' @param transform_directory
#' @return
#' @author Emma Mendelsohn
#' @export
transform_nasa_weather <- function(nasa_weather_downloaded,
                                   continent_raster_template,
                                   transform_directory =
                                   "data/nasa_weather_transformed") {

  filename <- basename(nasa_weather_downloaded)
  save_filename <- glue::glue("transformed_{filename}")
  
  suppressWarnings(dir.create(transform_directory, recursive = TRUE))
  existing_files <- list.files(transform_directory)
  
  message(paste0("Transforming ", save_filename))
  
  if(save_filename %in% existing_files){
    message("file already exists, skipping transform")
    return(file.path(transform_directory, save_filename))
  }
  
  raw_flat <- arrow::read_parquet(nasa_weather_downloaded)
  raw_flat <- as_tibble(raw_flat) |> 
    select(LON, LAT, everything(), -YYYYMMDD)  # terra::rast - the first with x (or longitude) and the second with y (or latitude) coordinates 
  raw_raster <- terra::rast(raw_flat) 
  crs(raw_raster) <-  crs(rast()) 

  transformed_raster <- transform_raster(raw_raster = raw_raster,
                                         template = rast(continent_raster_template))
  
  # Convert to dataframe
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    rename(relative_humidity = rh2m, temperature = t2m, precipitation= prectotcorr)

  # Save as parquet 
  write_parquet(dat_out, here::here(transform_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(transform_directory, save_filename))
  
  

}
