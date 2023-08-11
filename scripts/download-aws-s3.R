suppressPackageStartupMessages(source("packages.R"))

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Sentinel NDVI -----------------------------------------------------------
sentinel_ndvi_directory_raw <- tar_read(sentinel_ndvi_directory_raw)
aws_s3_download(path = sentinel_ndvi_directory_raw,
                bucket = aws_bucket ,
                key = sentinel_ndvi_directory_raw,
                check = TRUE)

sentinel_ndvi_directory_dataset <- tar_read(sentinel_ndvi_directory_dataset)
aws_s3_download(path = sentinel_ndvi_directory_dataset,
                bucket = aws_bucket ,
                key = sentinel_ndvi_directory_dataset,
                check = TRUE)

# Modis NDVI -----------------------------------------------------------
# modis_ndvi_directory <- tar_read(modis_ndvi_directory)
# aws_s3_download(path = modis_ndvi_directory,
#                 bucket = aws_bucket ,
#                 key = paste0("open-rvfcast/", modis_ndvi_directory),
#                 check = TRUE)

# NASA Weather ------------------------------------------------------------
nasa_weather_directory_raw <- tar_read(nasa_weather_directory_raw)
aws_s3_download(path = nasa_weather_directory_raw,
                bucket = aws_bucket ,
                key =  nasa_weather_directory_raw,
                check = TRUE)

nasa_weather_dataset <- tar_read(nasa_weather_dataset)
aws_s3_download(path = nasa_weather_dataset,
                bucket = aws_bucket ,
                key = nasa_weather_dataset,
                check = TRUE)
