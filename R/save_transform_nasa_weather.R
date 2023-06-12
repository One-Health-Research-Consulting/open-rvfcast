#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nasa_weather_downloaded
#' @param transform_directory
#' @param verbose
#' @return
#' @author Emma Mendelsohn
#' @export
save_transform_nasa_weather <- function(nasa_weather_downloaded,
                                        template = continent_raster_template,
                                        transform_directory =
                                        paste0(nasa_weather_directory,
                                        "_transformed"), verbose = TRUE) {

  # if(verbose) cat(raster_file, "\n")
  template <- rast(template)
  # raw_raster <- terra::rast(raster_file)
  # filename <- paste0("transformed_", basename(raster_file))
  
  suppressWarnings(dir.create(transform_directory, recursive = TRUE))
  existing_files <- list.files(transform_directory)
  
  if(filename %in% existing_files){
    message("file already exists, skipping transform")
    return(file.path(transform_directory, filename))
  }
  
  dat <- arrow::read_parquet(nasa_weather_downloaded)
  dat <- as_tibble(dat) |> 
    select(LON, LAT, everything(), -YYYYMMDD)  # terra::rast - the first with x (or longitude) and the second with y (or latitude) coordinates 
  raw_raster <- terra::rast(dat) 
  crs(raw_raster) <-  crs(rast()) 
  
  
  
  
  
  
  if(!identical(crs(raw_raster), crs(template))) {
    raw_raster <- terra::project(raw_raster, template)
  }
  if(!identical(origin(raw_raster), origin(template)) ||
     !identical(res(raw_raster), res(template))) {
    raw_raster <- terra::resample(raw_raster, template, method = "cubicspline")
  } 
  
  save_function <- switch(tools::file_ext(filename), 
                          "tif" = terra::writeRaster,
                          "nc" = terra::writeCDF)
  
  save_function(raw_raster, here::here(transform_directory, filename), overwrite = T)
  return(file.path(transform_directory, filename))
  
}
