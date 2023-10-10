suppressPackageStartupMessages(source("packages.R"))
library(furrr)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

message("downloading nasa weather dataset")

nasa_weather_dataset <- tar_read(nasa_weather_dataset)

# PARALLEL set number of workers
n_workers <- length(nasa_weather_dataset)

plan(multisession, workers = n_workers)
future_map(nasa_weather_dataset, function(x){
  aws_s3_download(path = x,
                  bucket = aws_bucket ,
                  key = x,
                  check = TRUE)
})