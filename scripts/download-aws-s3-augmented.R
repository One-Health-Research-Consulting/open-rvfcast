suppressPackageStartupMessages(source("packages.R"))
aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

#  Forecasts ------------------------------------------------------------
message("downloading augmented directory")
augmented_data_directory <- tar_read(augmented_data_directory)

#TODO figure this out
bucket_contents <- aws.s3::get_bucket(bucket = aws_bucket, prefix = augmented_data_directory)
keys <- unname(map_chr(bucket_contents, ~.[["Key"]]))

for(key in keys){
  message(key)
  aws_s3_download(path = key,
                  bucket = aws_bucket ,
                  key =  key,
                  check = TRUE)
  
}
