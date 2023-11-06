suppressPackageStartupMessages(source("packages.R"))
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")


#  Weather ------------------------------------------------------------
message("downloading weather_historical_means_directory")
weather_historical_means_directory <- tar_read(weather_historical_means_directory)
aws_s3_download(path = weather_historical_means_directory,
                bucket = aws_bucket ,
                key =  weather_historical_means_directory,
                check = TRUE)

#  NDVI ------------------------------------------------------------
message("downloading ndvi_historical_means_directory")
ndvi_historical_means_directory <- tar_read(ndvi_historical_means_directory)
aws_s3_download(path = ndvi_historical_means_directory,
                bucket = aws_bucket ,
                key =  ndvi_historical_means_directory,
                check = TRUE)

