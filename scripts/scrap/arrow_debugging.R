suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

forecasts_anomalies <- list.files("data/forecast_anomalies", full.names = TRUE)

failed <- map_dfr(forecasts_anomalies, ~{
  result <- tryCatch({
    arrow::read_parquet(.x) 
    NULL
  }, error = function(e) {
    cat("Error opening dataset for item:", .x, "\n")
    return(data.frame(failed_item = .x))
  })
  
  return(result) 
})

# 1 data/forecast_anomalies/forecast_anomaly_2010-03-27.gz.parquet
# 2 data/forecast_anomalies/forecast_anomaly_2010-08-08.gz.parquet
# 3 data/forecast_anomalies/forecast_anomaly_2010-08-24.gz.parquet
# 4 data/forecast_anomalies/forecast_anomaly_2010-11-17.gz.parquet
# 5 data/forecast_anomalies/forecast_anomaly_2010-11-28.gz.parquet
# 6 data/forecast_anomalies/forecast_anomaly_2011-01-11.gz.parquet
# 7 data/forecast_anomalies/forecast_anomaly_2011-03-14.gz.parquet

for(f in failed$failed_item) file.remove(f)

tar_invalidate(forecasts_anomalies)

tar_make(forecasts_anomalies)
