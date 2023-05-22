suppressPackageStartupMessages(source("packages.R"))

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Sentinel NDVI -----------------------------------------------------------
sentinel_ndvi_directory <- tar_read(sentinel_ndvi_directory)
aws_s3_download(path = sentinel_ndvi_directory,
                bucket = aws_bucket ,
                key = paste0("open-rvfcast/", sentinel_ndvi_directory),
                check = TRUE)



# Modis NDVI -----------------------------------------------------------
modis_ndvi_directory <- tar_read(modis_ndvi_directory)
aws_s3_download(path = modis_ndvi_directory,
                bucket = aws_bucket ,
                key = paste0("open-rvfcast/", modis_ndvi_directory),
                check = TRUE)

# NASA Weather ------------------------------------------------------------
nasa_weather_directory <- tar_read(nasa_weather_directory)
aws_s3_download(path = nasa_weather_directory,
                bucket = aws_bucket ,
                key = paste0("open-rvfcast/", nasa_weather_directory),
                check = TRUE)

# ECMWF Forecasts ------------------------------------------------------------
ecmwf_forecasts_directory <- tar_read(ecmwf_forecasts_directory)
aws_s3_download(path = ecmwf_forecasts_directory,
                bucket = aws_bucket ,
                key = paste0("open-rvfcast/", ecmwf_forecasts_directory),
                check = TRUE)

# Test --------------------------------------------------------------------
# dir.create("data/aws_test")
# file.create("data/aws_test/.gitkeep")
# write_csv(mtcars, "data/aws_test/mtcars.csv")
# 
# aws_s3_upload(path = "data/aws_test",
#               bucket =  aws_bucket ,
#               key = "data/aws_test", 
#               prefix = "open-rvfcast/",
#               check = TRUE)
# file.remove("data/aws_test/mtcars.csv")
# aws_s3_download(path = "data/aws_test",
#                 bucket = aws_bucket ,
#                 key = paste0("open-rvfcast/", "data/aws_test"),
#                 check = TRUE)