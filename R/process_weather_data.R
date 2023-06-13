#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sentinel_ndvi_transformed
#' @param nasa_weather_transformed
#' @return
#' @author Emma Mendelsohn
#' @export
process_weather_data <- function(nasa_weather_directory_dataset, nasa_weather_transformed, model_dates_random_select) {
  
  # connect to transformed data
  weather_conn <- open_dataset(nasa_weather_directory_dataset) 
  # or keep on aws: s3_bucket(nasa_weather_directory_dataset)
  
  # TODO this could be with the template
  weather_conn <- weather_conn |> 
    mutate(x = as.character(round(x, 3))) |> 
    mutate(y = as.character(round(y, 3)))
  
  test <- weather_conn |> 
    filter(year == 2019) |> 
    collect()
  
  test2 = 
    test |> 
    group_by(x, y) |> 
    count() |> 
    ungroup() |> 
    filter(n == 1095) |> 
    slice(1)
  
  test3 = test |> 
    filter(x == test2$x, y == test2$y) |> 
    filter(doy ==1)
    
  
  # calculate monthly averages by pixel
  # calculate anomalies for each day relative to monthly average
  weather_means <- weather_conn |> 
    group_by(x, y, mm) |> 
    summarize(mean_relative_humidity = mean(relative_humidity),
           mean_temperature = mean(temperature),
           mean_precipitation = mean(precipitation)) |> 
    ungroup() 
  test=weather_means |> filter(mm==1) |> collect()
  #TODO debug, why are we seeing NAs
  
  
    mutate(anomaly_relative_humidity = relative_humidity - mean_relative_humidity,
           anomaly_temperature = temperature - mean_temperature,
           anomaly_precipitation = precipitation - mean_precipitation)
  


  # randomly select two days per month and get 30, 60, 90 day lags
  test = weather_means |>  
    filter(year == 2019) |> 
    collect()
  
  
  
  test <- weather_anomalies |> 
    filter(year > 2015) |> 
    group_by(x, y) |> 
    arrange(year, doy) |> 
    ungroup() |> 
    head(100) |> 
    collect()
  
  test = tibble(test)
  
  test2 = test |> 
    filter(x == max(x)) |> 
    filter(y == max(y))
  
 out = test2 |> 
   select(doy, temperature, precipitation) |> 
    mutate(temp_30_day_avg = slide_dbl(temperature, .before = 29, .after = 0, .f = mean, na.rm = FALSE),
           precip_30_day_avg = slide_dbl(precipitation, .before = 29, .after = 0, .f = mean, na.rm = TRUE)) 
   
  
 out= zoo::rollmean(test2$temperature, k = 30, fill = NA, align = "right")
 #! day 30 includes the temp on day 30, so we need to offset by one
  
  # get anomaly lags
  
  # collect
  return(NULL)
  
}
