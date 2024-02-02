suppressPackageStartupMessages(source("packages.R"))
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# #  Weather ------------------------------------------------------------
# message("downloading weather_anomalies_directory")
# weather_anomalies_directory <- tar_read(weather_anomalies_directory)
# aws_s3_download(path = weather_anomalies_directory,
#                 bucket = aws_bucket ,
#                 key =  weather_anomalies_directory,
#                 check = TRUE)
# 
# #  NDVI ------------------------------------------------------------
# message("downloading ndvi_anomalies_directory")
# ndvi_anomalies_directory <- tar_read(ndvi_anomalies_directory)
# aws_s3_download(path = ndvi_anomalies_directory,
#                 bucket = aws_bucket ,
#                 key =  ndvi_anomalies_directory,
#                 check = TRUE)

#  Forecasts ------------------------------------------------------------
# message("downloading forecasts_anomalies_directory")
# forecasts_anomalies_directory <- tar_read(forecasts_anomalies_directory)
# aws_s3_download(path = forecasts_anomalies_directory,
#                 bucket = aws_bucket ,
#                 key =  forecasts_anomalies_directory,
#                 check = TRUE)

message("downloading forecasts_validate_directory")
forecasts_validate_directory <- tar_read(forecasts_validate_directory)
aws_s3_download(path = forecasts_validate_directory,
                bucket = aws_bucket ,
                key =  forecasts_validate_directory,
                check = TRUE)


