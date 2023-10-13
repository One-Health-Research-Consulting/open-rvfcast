suppressPackageStartupMessages(source("packages.R"))
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Sentinel NDVI -----------------------------------------------------------
message("downloading sentinel_ndvi_directory_transformed")
sentinel_ndvi_directory_transformed <- tar_read(sentinel_ndvi_directory_transformed)
aws_s3_download(path = sentinel_ndvi_directory_transformed,
                bucket = aws_bucket ,
                key = sentinel_ndvi_directory_transformed,
                check = TRUE)
sentinel_ndvi_directory_transformed_files <- list.files(sentinel_ndvi_directory_transformed)
tar_load(sentinel_ndvi_transformed)
assertthat::are_equal(length(sentinel_ndvi_transformed), length(sentinel_ndvi_directory_transformed_files))

# Modis NDVI -----------------------------------------------------------
message("downloading modis_ndvi_directory_transformed")
modis_ndvi_directory_transformed <- tar_read(modis_ndvi_directory_transformed)
aws_s3_download(path = modis_ndvi_directory_transformed,
                bucket = aws_bucket ,
                key = modis_ndvi_directory_transformed,
                check = TRUE)
modis_ndvi_directory_transformed_files <- list.files(modis_ndvi_directory_transformed)
tar_load(modis_ndvi_transformed)
assertthat::are_equal(length(modis_ndvi_transformed), length(modis_ndvi_directory_transformed_files))

# NASA Weather ------------------------------------------------------------
message("downloading nasa_weather_directory_transformed")
nasa_weather_directory_transformed <- tar_read(nasa_weather_directory_transformed)
# cycle through files to deal with subdirectories
nasa_weather_directory_transformed_files <- aws.s3::get_bucket(aws_bucket, prefix = "data/nasa_weather_transformed")
walk(nasa_weather_directory_transformed_files, function(ff){
  aws_s3_download(path = ff$Key,
                  bucket = aws_bucket ,
                  key =  ff$Key,
                  check = TRUE)
})
nasa_weather_directory_transformed_files <- list.files(nasa_weather_directory_transformed)
tar_load(nasa_weather_transformed)
assertthat::are_equal(length(nasa_weather_transformed), length(nasa_weather_directory_transformed_files))

# ECMWF Forecasts -----------------------------------------------------------
# message("downloading ecmwf_forecasts_directory_transformed")
# ecmwf_forecasts_directory_transformed <- tar_read(ecmwf_forecasts_directory_transformed)
# aws_s3_download(path = ecmwf_forecasts_directory_transformed,
#                 bucket = aws_bucket ,
#                 key = ecmwf_forecasts_directory_transformed,
#                 check = TRUE)
