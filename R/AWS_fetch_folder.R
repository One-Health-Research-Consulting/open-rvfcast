# Function to download files from S3 only if they don't exist locally, handling pagination
AWS_fetch_folder <- function(local_folder) {
  
  # Check if AWS credentials and region are set in the environment
  if (any(Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION")) == "")) {
    msg <- paste(
      "AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION environment variables",
      "must all be set to access AWS. Please ensure they are configured correctly,",
      "probably in the .env file or system environment."
    )
    stop(msg)
  }
  
  # Create an S3 client
  s3 <- paws::s3()
  
  # List all objects in the S3 bucket, handling pagination
  s3_files <- c()  # Initialize an empty list to hold all files
  continuation_token <- NULL
  
  repeat {
    response <- s3$list_objects_v2(
      Bucket = Sys.getenv("AWS_BUCKET_ID"),
      Prefix = local_folder,
      ContinuationToken = continuation_token)
    
    # Append the files from this response to the main list
    s3_files <- c(s3_files, map_vec(response$Contents, ~.x$Key))
    
    # Check if there's a continuation token for further pages
    if (!length(response$NextContinuationToken)) break  # No more pages to fetch, exit the loop
    continuation_token <- response$NextContinuationToken
  }
  
  # Check if S3 has files to download
  if (length(s3_files) == 0) {
    cat("No files found in the specified S3 bucket and prefix.\n")
    return(NULL)
  }
  
  # List local files in your folder
  local_files <- list.files(local_folder, recursive = TRUE, full.names = TRUE)
  downloaded_files <- c()
  
  # Loop through S3 files and download if they don't exist locally
  for (file in s3_files) {
    
    # Check if file already exists locally
    if (!file %in% local_files) {
      
      # Download the file from S3
      s3_download <- s3$get_object(
        Bucket = Sys.getenv("AWS_BUCKET_ID"),
        Key = file)
      
      # Write output to file
      writeBin(s3_download$Body, con = file)
      
      cat("Downloaded:", file, "\n")
      
      # Create an error safe way to test if the parquet file can be read
      error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
      
      # Check if transformed file can be loaded. 
      # If not clean it up and also remove it from AWS
      # It'll be picked up next time.
      if(is.null(error_safe_read_parquet(file))) {
        unlist(file)
        s3$delete_object(
          Bucket = Sys.getenv("AWS_BUCKET_ID"),
          Key = file)
      } else {
        downloaded_files <- c(downloaded_files, file) 
      }
    }
  }
  
  downloaded_files
}
