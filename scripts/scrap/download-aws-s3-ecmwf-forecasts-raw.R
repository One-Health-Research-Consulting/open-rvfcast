suppressPackageStartupMessages(source("packages.R"))
library(furrr)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

message("downloading ecmwf forecasts raw")

ecmwf_forecasts_downloaded <- tar_read(ecmwf_forecasts_downloaded)

# PARALLEL set number of workers
n_workers <- length(ecmwf_forecasts_downloaded)

plan(multisession, workers = n_workers)
future_map(ecmwf_forecasts_downloaded, function(x){
  aws_s3_download(path = x,
                  bucket = aws_bucket,
                  key = x,
                  check = TRUE)
})