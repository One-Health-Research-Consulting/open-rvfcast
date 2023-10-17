suppressPackageStartupMessages(source("packages.R"))
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")


#  Weather ------------------------------------------------------------
message("downloading weather_anomalies_directory")
weather_anomalies_directory <- tar_read(weather_anomalies_directory)
aws_s3_download(path = weather_anomalies_directory,
                bucket = aws_bucket ,
                key =  weather_anomalies_directory,
                check = TRUE)

