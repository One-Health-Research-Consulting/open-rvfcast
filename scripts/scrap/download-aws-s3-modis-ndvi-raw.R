suppressPackageStartupMessages(source("packages.R"))
library(furrr)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

message("downloading modis ndvi raw")

modis_ndvi_downloaded <- tar_read(modis_ndvi_downloaded)

# PARALLEL set number of workers
n_workers <- 40

plan(multisession, workers = n_workers)
future_map(modis_ndvi_downloaded, function(x){
  aws_s3_download(path = x,
                  bucket = aws_bucket,
                  key = x,
                  check = TRUE)
})