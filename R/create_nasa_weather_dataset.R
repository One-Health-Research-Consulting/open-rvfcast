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
create_nasa_weather_dataset <- function(nasa_weather_downloaded,
                                        nasa_weather_directory_dataset, 
                                        continent_raster_template) {
  
  
  nasa_weather_directory_raw <- unique(dirname(nasa_weather_downloaded))
  
  # remove dupes due to having overlapping country bounding boxes
  # resave as arrow dataset, grouped by year
  open_dataset(nasa_weather_directory_raw) |> 
    distinct() |> 
    rename_all(tolower) |> 
    rename(relative_humidity = rh2m, temperature = t2m, precipitation= prectotcorr,
           month = mm, day = dd, x = lon, y = lat, day_of_year = doy) |> 
    select(x, y, everything(), -yyyymmdd) |>  # terra::rast - the first with x (or longitude) and the second with y (or latitude) coordinates 
    group_by(year) |> 
    write_dataset(nasa_weather_directory_dataset)
  
  nasa_weather_preprocess_files <- list.files(nasa_weather_directory_dataset, full.names = TRUE, recursive = TRUE)

  continent_raster_template <- rast(continent_raster_template)
  
  walk(nasa_weather_preprocess_files, function(file){
    
    raw_flat <- arrow::read_parquet(file)
    assertthat::assert_that(names(raw_flat)[1]=="x")
    assertthat::assert_that(names(raw_flat)[2]=="y")
    
    check_rows <- raw_flat |> group_by(x, y) |> count() |> ungroup() |> distinct(n)
    assertthat::are_equal(1, nrow(check_rows))
    check_rows <- raw_flat |> group_by(day_of_year) |> count() |> ungroup() |> distinct(n)
    assertthat::are_equal(1, nrow(check_rows))
    
    # theres probably a nicer way to do this - combine into stack then transform all (then back to parquet??)
    dat_out <- raw_flat |> 
      group_split(day_of_year) |>  
      map_dfr(function(daily){
        raw_raster <- terra::rast(daily) 
        crs(raw_raster) <-  crs(rast()) 
        transformed_raster <- transform_raster(raw_raster = raw_raster,
                                               template = continent_raster_template)
        as.data.frame(transformed_raster, xy = TRUE) 
      })
    
    # Save as parquets 
    write_parquet(dat_out, file, compression = "gzip", compression_level = 5)
    
  })
  
  return(nasa_weather_preprocess_files)
  
}
