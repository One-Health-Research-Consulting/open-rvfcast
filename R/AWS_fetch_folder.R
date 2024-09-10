#' Download files from an AWS S3 bucket to a local folder
#'
#' This function downloads files from a specified S3 bucket and prefix to a local folder.
#' It only downloads files that are not already present in the local folder.
#' Additionally, it ensures that AWS credentials and region are set in the environment.
#'
#' @author Nathan Layman
#'
#' @param local_folder Character. The path to the local folder where files should be downloaded and the AWS prefix
#'
#' @return A list of files downloaded from AWS
#' 
#' @note
#' The AWS environment variables `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_REGION`, and `AWS_BUCKET_ID`
#' must be set correctly in order to access the S3 bucket. If any of these are missing, the function will stop with an error.
#' Files in the S3 bucket will be deleted if they cannot be successfully read as parquet files.
#'
#'
#' @examples
#' \dontrun{
#'   # Ensure the AWS environment variables are set in your system or .env file:
#'   # AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_REGION, and AWS_BUCKET_ID
#'
#'   # Download files from an S3 bucket folder to a local directory
#'   downloaded_files <- AWS_fetch_folder("my/local/folder")
#' }
#' 
#' @export
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


#' Upload and Sync Files with AWS S3
#'
#' This function synchronizes a local folder with an AWS S3 bucket. It checks for AWS credentials, 
#' lists existing files in the S3 bucket, and compares them with the local files to upload new files
#' or remove files that are no longer needed.
#' 
#' @author Nathan Layman
#'
#' @param transformed_file_list A character vector of file paths that should be present on AWS S3.
#' @param local_folder A character string specifying the path to the local folder to be synced with AWS S3.
#'
#' @examples
#' \dontrun{
#'   AWS_put_files(transformed_file_list = c("file1.parquet", "file2.parquet"), 
#'                  local_folder = "path/to/local/folder")
#' }
#'
#' @return A list of actions taken
#'
#' @export
AWS_put_files <- function(transformed_file_list,
                          local_folder) {
  
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
  
  # Get files in local folder
  local_folder_files <- list.files(path = local_folder, recursive = TRUE, full.names = TRUE)
  
  # Collect outcomes
  outcome <- c()
  
  # Walk through local_folder_files
  for(file in local_folder_files) {
    
    # Is the file in the transformed_file_list?
    if(file %in% transformed_file_list) {
      
      # Is the file already on AWS?
      if(file %in% s3_files) {
        
        outcome <- c(outcome, glue::glue("{file} already present on AWS"))
        
      } else {
        
        outcome <- c(outcome, glue::glue("Uploading {file} to AWS"))
        
        # Put the file on S3
        s3_download <- s3$get_object(
          Bucket = Sys.getenv("AWS_BUCKET_ID"),
          Key = file)
        
      }
    } else {
      
      # Remove the file from AWS if it's present in the folder and on AWS
      # but not in the list of successfully transformed files. This file is
      # not relevant to the pipeline
      if(file %in% s3_files) {
        
        outcome <- c(outcome, glue::glue("Cleaning up dangling file {file} from AWS"))
        
        # Remove the file from AWS
        s3_download <- s3$delete_object(
          Bucket = Sys.getenv("AWS_BUCKET_ID"),
          Key = file)
      }
    }
  }
  
  outcome
  
}
