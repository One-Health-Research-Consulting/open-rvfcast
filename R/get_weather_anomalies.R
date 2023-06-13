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
get_weather_anomalies <- function(nasa_weather_directory_dataset, nasa_weather_transformed) {
  
  # connect to transformed data
  weather_conn <- open_dataset(nasa_weather_directory_dataset) 
  # or keep on aws: s3_bucket(nasa_weather_directory_dataset)
  
  # calculate monthly averages by pixel
  weather_means <- weather_conn |> 
    group_by(x, y, month) |> 
    summarize(mean_relative_humidity = mean(relative_humidity),
              mean_temperature = mean(temperature),
              mean_precipitation = mean(precipitation)) |> 
    ungroup()  
  
  # calculate anomalies for each day relative to monthly average
  weather_means <- weather_conn |> 
    left_join(weather_means, by = c("x", "y", "month")) |> 
    mutate(anomaly_relative_humidity = relative_humidity - mean_relative_humidity,
           anomaly_temperature = temperature - mean_temperature,
           anomaly_precipitation = precipitation - mean_precipitation) |> 
    group_by(year) |> 
    write_dataset(nasa_weather_directory_dataset)
  
  return(nasa_weather_directory_dataset)
  
  
  # ok read into memory to do lags 😬
 #  weather_dat <- weather_means |> 
 #    collect()
 #  
 #  weather_lag_groups <- weather_dat |> 
 #    group_split(x, y)
 #  
 # test <- weather_lag_groups[[1]] |> 
 #    arrange(year, month, day) |> 
 #    mutate(anomaly_temperature_30d_mean = slider::slide_dbl(anomaly_temperature, .before = 30, .after = 0, .f = mean, .complete  = TRUE))
  
  # out = test2 |> 
  #   select(doy, temperature, precipitation) |> 
  #    mutate(temp_30_day_avg = slide_dbl(temperature, .before = 29, .after = 0, .f = mean, na.rm = FALSE),
  #           precip_30_day_avg = slide_dbl(precipitation, .before = 29, .after = 0, .f = mean, na.rm = TRUE)) 
  #  
  # out= zoo::rollmean(test2$temperature, k = 30, fill = NA, align = "right")
  #! day 30 includes the temp on day 30, so we need to offset by one
  
  
  # collect
  #return(NULL)
  
}
