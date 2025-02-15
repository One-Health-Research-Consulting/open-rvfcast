#' AWS S3 Bucket File Retrieval
#'
#' The function retrieves files stored in an AWS S3 bucket to a local folder and removes faulty parquet files.
#'
#' @author Nathan C. Layman
#'
#' @param local_folder A string representing the local folder where the files will be downloaded
#' @param ... additional arguments not used by this function, included for generic function compatibility
#'
#' @return A vector of strings representing the paths of the downloaded files. If no files are found, NULL is returned.
#'
#' @note This function requires the AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION environment variables to be set.
#' Access AWS S3 bucket contents, download files locally, clean faulty data, and return the list of downloaded files.
#'
#' @examples
#' # Ensure to set AWS environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_REGION)
#' AWS_get_folder(local_folder = "./data")
#'
#' @export
AWS_get_folder <- function(local_folder, 
                           skip_fetch = FALSE,
                           ...) {
  
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
      Prefix = paste0(local_folder,"/"), # Arggg! Without the "/" you can mix 'ndvi_anomaly' with 'ndvi_anomaly_lagged'....
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
  # or if the local copy can't be read
  for (file in s3_files) {
    
    # Check if file already exists locally
    if (!file %in% local_files) {
      
      if(skip_fetch) {
        cat("Skipping:", file, "\n")
        downloaded_files <- c(downloaded_files, file) 
        next
      }

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


#' Upload Transformed Files to AWS
#'
#' This function uploads transformed files to AWS given a list of files and a target local folder. It supports continuation token for handling large quantities of files. It also checks for existing files on AWS and in local folder before upload, providing informative messages about the uploading process.
#'
#' @author Nathan C. Layman
#'
#' @param transformed_file_list A character vector of filenames that have been transformed and are to be uploaded to AWS S3. Filenames should be base names, not full paths.
#' @param local_folder A character string indicating the local directory that contains the transformed files to be uploaded to AWS S3.
#' @param ... Additional arguments not used by this function.
#'
#' @return A character vector of messages indicating the outcomes of trying to upload each file in the transformed_file_list to AWS.
#'
#' @note The function uses AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION environment variables to access AWS. These should be set prior to running this function.
#'
#' @examples
#' AWS_put_files(transformed_file_list = c("file1.csv", "file2.csv"), 
#'               local_folder = "./transformed_data")
#'
#' @export
AWS_put_files <- function(transformed_file_list,
                          local_folder,
                          ...) {
  
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
  local_folder_files <- list.files(path = local_folder, recursive = TRUE, full.names = T)
  
  # Collect outcomes
  outcomes <- c()
  
  # Walk through local_folder_files
  for(file in local_folder_files) {
    
    # Is the file in the transformed_file_list?
    if(file %in% transformed_file_list) {
      
      # Put the file on S3
      s3_upload <- s3$put_object(
        Body = file,
        Bucket = Sys.getenv("AWS_BUCKET_ID"),
        Key = file)
      
      outcome <- glue::glue("Uploading {file} to AWS")
      
    } else {
      
      # Remove the file from AWS if it's present in the folder and on AWS
      # but not in the list of successfully transformed files. This file is
      # not relevant to the pipeline
      if(file %in% s3_files) {
        
        outcome <- glue::glue("Cleaning up dangling file {file} from AWS")
        
        # Remove the file from AWS
        s3_delete_receipt <- s3$delete_object(
          Bucket  = Sys.getenv("AWS_BUCKET_ID"),
          Key = file.path(local_folder, file))
      } else {
        next
      }
    }
    message(outcome)
    outcomes <- c(outcomes, outcome)
  }
  
  outcome
  
}
