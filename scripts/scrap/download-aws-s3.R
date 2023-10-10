suppressPackageStartupMessages(source("packages.R"))
library(furrr)
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Sentinel NDVI -----------------------------------------------------------
# sentinel_ndvi_directory_raw <- tar_read(sentinel_ndvi_directory_raw)
# aws_s3_download(path = sentinel_ndvi_directory_raw,
#                 bucket = aws_bucket ,
#                 key = sentinel_ndvi_directory_raw,
#                 check = TRUE)

# sentinel_ndvi_directory_dataset <- tar_read(sentinel_ndvi_directory_dataset)
# aws_s3_download(path = sentinel_ndvi_directory_dataset,
#                 bucket = aws_bucket ,
#                 key = sentinel_ndvi_directory_dataset,
#                 check = TRUE)

# Modis NDVI -----------------------------------------------------------
# modis_ndvi_directory <- tar_read(modis_ndvi_directory)
# aws_s3_download(path = modis_ndvi_directory,
#                 bucket = aws_bucket ,
#                 key = paste0("open-rvfcast/", modis_ndvi_directory),
#                 check = TRUE)

# NASA Weather ------------------------------------------------------------
# nasa_weather_directory_raw <- tar_read(nasa_weather_directory_raw)
# aws_s3_download(path = nasa_weather_directory_raw,
#                 bucket = aws_bucket ,
#                 key =  nasa_weather_directory_raw,
#                 check = TRUE)

message("downloading nasa_weather_dataset")

nasa_weather_dataset <- tar_read(nasa_weather_dataset)

plan(multisession, workers = 19)
future_map(nasa_weather_dataset, function(x){
  aws_s3_download(path = x,
                  bucket = aws_bucket ,
                  key = x,
                  check = TRUE)
})



# parallel?????
# aws.s3::s3sync(path = "data/nasa_weather_dataset",
#                bucket = "open-rvfcast-data",
#                prefix = "data/nasa_weather_dataset",
#                direction = "download")

