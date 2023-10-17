suppressPackageStartupMessages(source("packages.R"))
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Sentinel NDVI -----------------------------------------------------------
message("downloading sentinel_ndvi_raw_directory")
sentinel_ndvi_raw_directory <- tar_read(sentinel_ndvi_raw_directory)
aws_s3_download(path = sentinel_ndvi_raw_directory,
                bucket = aws_bucket ,
                key = sentinel_ndvi_raw_directory,
                check = TRUE)

# Modis NDVI -----------------------------------------------------------
message("downloading modis_ndvi_raw_directory")
modis_ndvi_raw_directory <- tar_read(modis_ndvi_raw_directory)
aws_s3_download(path = modis_ndvi_raw_directory,
                bucket = aws_bucket ,
                key = modis_ndvi_raw_directory,
                check = TRUE)

# NASA Weather ------------------------------------------------------------
message("downloading nasa_weather_raw_directory")
nasa_weather_raw_directory <- tar_read(nasa_weather_raw_directory)
aws_s3_download(path = nasa_weather_raw_directory,
                bucket = aws_bucket ,
                key =  nasa_weather_raw_directory,
                check = TRUE)

# ECMWF Forecasts -----------------------------------------------------------
message("downloading ecmwf_forecasts_raw_directory")
ecmwf_forecasts_raw_directory <- tar_read(ecmwf_forecasts_raw_directory)
aws_s3_download(path = ecmwf_forecasts_raw_directory,
                bucket = aws_bucket ,
                key = ecmwf_forecasts_raw_directory,
                check = TRUE)
