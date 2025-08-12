tar_load(ecmwf_forecasts_transformed)
tar_load(weather_historical_means)
tar_load(nasa_weather_transformed)
tar_load(forecasts_anomalies_directory)
tar_load(forecasts_anomalies)
tar_load(nasa_weather_raw)
basename_template <- "forecast_anomaly_{dates_to_process}.parquet"
dates_to_process <- tar_read(dates_to_process) |> pluck(1)
tar_load(forecast_intervals)
overwrite <- T
i <- 1

aaa <- arrow::read_parquet(ecmwf_forecasts_transformed[[1]])
bbb <- arrow::read_parquet(weather_historical_means[[1]])
ccc <- arrow::read_parquet(nasa_weather_transformed[[1]])
ddd <- arrow::read_parquet(nasa_weather_raw[[1]])
eee <- arrow::read_parquet(forecasts_anomalies[[1]])

all(unique(aaa$x) %in% unique(bbb$x))
all(unique(aaa$x) %in% unique(ccc$x))
all(unique(bbb$x) %in% unique(ccc$x))
all(unique(ddd$x) %in% unique(ccc$x))
all(unique(aaa$x) %in% unique(eee$x))
