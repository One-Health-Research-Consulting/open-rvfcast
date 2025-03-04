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
#' @param sync_with_remote Boolean indicating whether to delete corrupted files from AWS S3 to maintain consistency. Local corrupted files are always removed. Default is TRUE.
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
#'   skip_fetch = FALSE,
#'   sync_with_remote = TRUE
#' )
#'
#' @export
AWS_get_folder <- function(local_folder,
                           skip_fetch = FALSE,
                           sync_with_remote = FALSE,
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
      # Always clean up local files on failure, but only remove from AWS if cleanup_failed_files is TRUE
      if (is.null(error_safe_read_parquet(file))) {
        # Always clean up local corrupted files
        unlink(file)
        cat("Removed local corrupted file:", file, "\n")
        
        # Only remove from AWS if sync_with_remote is TRUE
        if (sync_with_remote) {
          aws.s3::delete_object(
            object = file,
            bucket = Sys.getenv("AWS_BUCKET_ID")
          )
          cat("Synced by removing file from AWS bucket\n")
        }
      } else {
        downloaded_files <- c(downloaded_files, file)
      }
    }
  }
  
  downloaded_files
}
