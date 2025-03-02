#' Fetch Files from AWS Bucket
#'
#' This function fetches files from a specified AWS S3 bucket and downloads them to a local directory.
#' If skip_fetch is TRUE, the function will only return the names of the files available for download
#' in the S3 bucket without actually downloading them.
#'
#' @author Nathan C. Layman
#'
#' @param local_folder String specifying the local directory where the files will be downloaded.
#' @param skip_fetch Boolean indicating whether to download the files. If TRUE, no files will be downloaded.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A vector of strings containing the paths to the downloaded files. If skip_fetch is TRUE
#' this will instead contain the names of available files in the S3 bucket.
#'
#' @note This function requires the AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION environment variables
#' to be set. These are typically set in the .env file or system environment. The function will stop and display
#' an error message if these environment variables are not set.
#'
#' @examples
#' AWS_get_folder(
#'   local_folder = "local/directory",
#'   skip_fetch = FALSE
#' )
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
  
  # Get files from S3 bucket with prefix
  s3_files <- aws.s3::get_bucket_df(
    bucket = Sys.getenv("AWS_BUCKET_ID"),
    prefix = paste0(local_folder, "/"),
    max = Inf  # This ensures it gets ALL objects (default is 1000)
  ) |>
    pull(Key)
  
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
      if (skip_fetch) {
        cat("Skipped fetching AWS file: ", file, ".\n", sep = "")
        downloaded_files <- c(downloaded_files, file)
        next
      }
      
      # Download the file from S3 using aws.s3
      aws.s3::save_object(
        object = file,
        bucket = Sys.getenv("AWS_BUCKET_ID"),
        file = file
      )
      
      cat("Downloaded AWS file:", file, "\n")
      
      # Create an error safe way to test if the parquet file can be read
      error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
      
      # Check if transformed file can be loaded.
      # If not clean it up and also remove it from AWS
      # It'll be picked up next time.
      if (is.null(error_safe_read_parquet(file))) {
        unlink(file)
        aws.s3::delete_object(
          object = file,
          bucket = Sys.getenv("AWS_BUCKET_ID")
        )
      } else {
        downloaded_files <- c(downloaded_files, file)
      }
    }
  }
  
  downloaded_files
}


#' Upload Transformed Files to AWS
#'
#' This function uploads transformed files to AWS given a list of files and a target local folder.
#' It supports pagination for handling large quantities of files. It also checks for existing files 
#' on AWS and in local folder before upload, providing informative messages about the uploading process.
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
#' AWS_put_files(
#'   transformed_file_list = c("file1.csv", "file2.csv"),
#'   local_folder = "./transformed_data"
#' )
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
  
  # Get files from S3 bucket with prefix
  s3_files <- aws.s3::get_bucket_df(
    bucket = Sys.getenv("AWS_BUCKET_ID"),
    prefix = local_folder,
    max = Inf
  ) |>
    pull(Key)
  
  # Get files in local folder
  local_folder_files <- list.files(path = local_folder, recursive = TRUE, full.names = TRUE)
  
  # Collect outcomes
  outcomes <- c()
  
  # Walk through local_folder_files
  for (file in local_folder_files) {
    # Is the file in the transformed_file_list?
    if (file %in% transformed_file_list) {
      # Put the file on S3 using aws.s3
      aws.s3::put_object(
        file = file,
        object = file,
        multipart = TRUE,
        part_size = 10485760,
        bucket = Sys.getenv("AWS_BUCKET_ID")
      )
      
      outcome <- glue::glue("Uploading {file} to AWS")
    } else {
      # Remove the file from AWS if it's present in the folder and on AWS
      # but not in the list of successfully transformed files. This file is
      # not relevant to the pipeline
      if (file %in% s3_files) {
        outcome <- glue::glue("Cleaning up dangling file {file} from AWS")
        
        # Remove the file from AWS using aws.s3
        aws.s3::delete_object(
          object = file.path(file),
          bucket = Sys.getenv("AWS_BUCKET_ID")
        )
      } else {
        next
      }
    }
    message(outcome)
    outcomes <- c(outcomes, outcome)
  }
  
  outcomes
}