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
transform_nasa_weather <- function(nasa_weather_preprocess_files, 
                                   continent_raster_template) {
  
  raw_flat <- arrow::read_parquet(nasa_weather_preprocess_files)
  assertthat::assert_that(names(raw_flat)[1]=="x")
  assertthat::assert_that(names(raw_flat)[2]=="y")
  
  check_rows <- raw_flat |> group_by(x, y) |> count() |> ungroup() |> distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  check_rows <- raw_flat |> group_by(day_of_year) |> count() |> ungroup() |> distinct(n)
  assertthat::are_equal(1, nrow(check_rows))
  
  dat_out <- raw_flat |> 
    group_split(day_of_year) |> 
    map_dfr(function(daily){
      raw_raster <- terra::rast(daily) 
      crs(raw_raster) <-  crs(rast()) 
      transformed_raster <- transform_raster(raw_raster = raw_raster,
                                             template = rast(continent_raster_template))
     test= as.data.frame(transformed_raster, xy = TRUE) 
    })
  
  # Save as parquet 
  write_parquet(dat_out, nasa_weather_preprocess_files, compression = "gzip", compression_level = 5)
  
  return(nasa_weather_preprocess_files)

}
