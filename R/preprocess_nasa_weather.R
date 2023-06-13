#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nasa_weather_directory_raw
#' @param nasa_weather_downloaded
#' @param nasa_weather_directory_dataset
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
preprocess_nasa_weather <- function(nasa_weather_downloaded,
                                    nasa_weather_directory_dataset){
  
  
  nasa_weather_directory_raw <- unique(dirname(nasa_weather_downloaded))
  
  open_dataset(nasa_weather_directory_raw) |> 
    distinct() |> 
    rename_all(tolower) |> 
    rename(relative_humidity = rh2m, temperature = t2m, precipitation= prectotcorr,
           month = mm, day = dd, x = lon, y = lat, day_of_year = doy) |> 
    select(x, y, everything(), -yyyymmdd) |>  # terra::rast - the first with x (or longitude) and the second with y (or latitude) coordinates 
    group_by(year) |> 
    write_dataset(nasa_weather_directory_dataset)
  
  return(nasa_weather_directory_dataset)
  
}
