suppressPackageStartupMessages(source("packages.R"))
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Sentinel NDVI -----------------------------------------------------------
message("downloading sentinel_ndvi_directory_raw")
sentinel_ndvi_directory_raw <- tar_read(sentinel_ndvi_directory_raw)
aws_s3_download(path = sentinel_ndvi_directory_raw,
                bucket = aws_bucket ,
                key = sentinel_ndvi_directory_raw,
                check = TRUE)

# Modis NDVI -----------------------------------------------------------
message("downloading modis_ndvi_directory_raw")
modis_ndvi_directory_raw <- tar_read(modis_ndvi_directory_raw)
aws_s3_download(path = modis_ndvi_directory_raw,
                bucket = aws_bucket ,
                key = modis_ndvi_directory_raw,
                check = TRUE)

# NASA Weather ------------------------------------------------------------
message("downloading nasa_weather_directory_raw")
nasa_weather_directory_raw <- tar_read(nasa_weather_directory_raw)
aws_s3_download(path = nasa_weather_directory_raw,
                bucket = aws_bucket ,
                key =  nasa_weather_directory_raw,
                check = TRUE)

# ECMWF Forecasts -----------------------------------------------------------
message("downloading ecmwf_forecasts_directory_raw")
ecmwf_forecasts_directory_raw <- tar_read(ecmwf_forecasts_directory_raw)
aws_s3_download(path = ecmwf_forecasts_directory_raw,
                bucket = aws_bucket ,
                key = ecmwf_forecasts_directory_raw,
                check = TRUE)
