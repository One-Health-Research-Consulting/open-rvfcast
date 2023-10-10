suppressPackageStartupMessages(source("packages.R"))
library(furrr)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

message("downloading ecmwf forecasts dataset")

ecmwf_forecasts_dataset <- tar_read(ecmwf_forecasts_dataset)

# PARALLEL set number of workers
n_workers <- length(ecmwf_forecasts_dataset)

plan(multisession, workers = n_workers)
future_map(ecmwf_forecasts_dataset, function(x){
  aws_s3_download(path = x,
                  bucket = aws_bucket ,
                  key = x,
                  check = TRUE)
})