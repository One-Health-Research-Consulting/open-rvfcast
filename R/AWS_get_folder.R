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
  
  aws_region = if (Sys.getenv("AWS_REGION") == "auto") "" else Sys.getenv("AWS_REGION")

  # Create a comprehensive validation function that checks both readability and row count
  error_safe_validate_file <- possibly(
    function(file) {
      # Try to open as dataset (more memory efficient for large files)
      dataset <- arrow::open_dataset(file)
      row_count <- dataset |> 
        arrow::compute_count() |> 
        arrow::as_vector()
      
      # Return row count if successful and has data
      if (row_count > 0) {
        return(row_count)
      } else {
        return(0)  # Empty file
      }
    },
    otherwise = NULL
  )

  # Get files from S3 bucket with prefix
  df_bucket_data <- aws.s3::get_bucket(bucket = Sys.getenv("AWS_BUCKET_ID"),
                                        prefix = paste0(local_folder, "/"),
                                       region = aws_region)
  s3_files <- map_chr(df_bucket_data, pluck, "Key")

  # Check if S3 has files to download
  if (length(s3_files) == 0) {
    cat("No files found in the specified S3 bucket and prefix.\n")
    return(NULL)
  }

  # List local files in your folder
  local_files <- list.files(local_folder, recursive = TRUE, full.names = TRUE)
  downloaded_files <- c()

  # Loop through S3 files and download if needed
  for (file in s3_files) {
    # Only download if file doesn't exist locally AND skip_fetch is FALSE
    if (!(file %in% local_files || skip_fetch)) {
      # Download the file from S3 using aws.s3
      aws.s3::save_object(
        object = file,
        bucket = Sys.getenv("AWS_BUCKET_ID"),
        region = aws_region,
        file = file
      )

      cat("Downloaded AWS file:", file, "\n")

      # Validate file - check if it's readable and has rows > 0
      validation_result <- error_safe_validate_file(file)
      if (is.null(validation_result) || validation_result == 0) {
        # Clean up local corrupted or empty files
        unlink(file)
        if (is.null(validation_result)) {
          cat("Removed local corrupted file:", basename(file), "\n")
        } else {
          cat("Removed local empty file:", basename(file), "\n")
        }

        # Only remove from AWS if sync_with_remote is TRUE
        if (sync_with_remote) {
          aws.s3::delete_object(
            object = file,
            bucket = Sys.getenv("AWS_BUCKET_ID")
          )
          cat("Synced by removing corrupt/empty file from AWS bucket\n")
        }
      } else {
        # Add to downloaded_files if file was successfully downloaded and has data
        downloaded_files <- c(downloaded_files, file)
        cat("Validated file with", validation_result, "rows\n")
      }
    } else {
      cat("Skipped file:", basename(file), "\n")
    }
  }

  downloaded_files
}


#' Upload Transformed Files to AWS S3
#'
#' This function uploads transformed files to an AWS S3 bucket, handling large file quantities 
#' through pagination and providing comprehensive file management capabilities.
#'
#' @details The function performs several key operations:
#' \itemize{
#'   \item Checks for existing AWS credentials
#'   \item Verifies file schemas before uploading
#'   \item Supports selective file upload based on schema matching
#'   \item Optionally overwrites existing files on AWS
#'   \item Cleans up dangling files from the S3 bucket
#' }
#'
#' @author Nathan C. Layman
#'
#' @param transformed_file_list A character vector of filenames to be uploaded to AWS S3.
#'   These should be base filenames (not full paths) that have been transformed and are 
#'   ready for upload.
#' @param local_folder A character string specifying the local directory containing 
#'   the transformed files to be uploaded to AWS S3.
#' @param overwrite Logical. If \code{TRUE}, files will be uploaded even if they 
#'   already exist in the S3 bucket with matching schemas. Defaults to \code{FALSE}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A character vector of messages describing the outcomes of file upload attempts, 
#'   including successful uploads, skipped files, and cleanup operations.
#'
#' @note 
#' Required environment variables:
#' \itemize{
#'   \item \code{AWS_ACCESS_KEY_ID}: AWS access key
#'   \item \code{AWS_SECRET_ACCESS_KEY}: AWS secret access key
#'   \item \code{AWS_REGION}: AWS region
#'   \item \code{AWS_BUCKET_ID}: S3 bucket identifier
#' }
#' These environment variables must be set prior to calling the function, typically 
#' in a .env file or system environment.
#'
#' @examples
#' \dontrun{
#' # Upload transformed CSV files from a local directory
#' AWS_put_files(
#'   transformed_file_list = c("file1.csv", "file2.csv"),
#'   local_folder = "./transformed_data"
#' )
#' 
#' # Upload with overwrite option
#' AWS_put_files(
#'   transformed_file_list = c("file1.csv", "file2.csv"),
#'   local_folder = "./transformed_data",
#'   overwrite = TRUE
#' )
#' }
#'
#' @importFrom aws.s3 get_bucket put_object delete_object
#' @importFrom arrow open_dataset
#' @importFrom glue glue
#' @importFrom purrr possibly
#' @importFrom dplyr pull
#'
#' @export
AWS_put_files <- function(transformed_file_list,
                          local_folder,
                          overwrite = FALSE,
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

  # Create a possibly-wrapped version of the function
  error_safe_open_dataset <- possibly(
    function(file) {
      arrow::open_dataset(file)$schema
    },
    otherwise = NULL
  )

  aws_region = if (Sys.getenv("AWS_REGION") == "auto") "" else Sys.getenv("AWS_REGION")

  # Get files from S3 bucket with prefix
  df_bucket_data <- aws.s3::get_bucket(bucket = Sys.getenv("AWS_BUCKET_ID"),
                                       prefix = paste0(local_folder, "/"),
                                       region = aws_region)
                                        
  s3_files <- map_chr(df_bucket_data, pluck, "Key")

  # Get files in local folder
  local_folder_files <- list.files(path = local_folder, recursive = TRUE, full.names = TRUE)

  # Collect outcomes
  outcomes <- c()

  # Walk through local_folder_files
  for (file in local_folder_files) {
    # Is the file in the transformed_file_list?
    if (file %in% transformed_file_list) {
      # Check that schemas match
      remote_file <- paste0("s3://", Sys.getenv("AWS_BUCKET_ID"), "/", file)
      remote_schema <- error_safe_open_dataset(remote_file)
      local_schema <- error_safe_open_dataset(file)

      if (is.null(remote_schema) || !remote_schema$Equals(local_schema) || overwrite == TRUE) {
        # Put the file on S3 using aws.s3
        aws.s3::put_object(
          file = file,
          object = file,
          multipart = TRUE,
          part_size = 10485760,
          bucket = Sys.getenv("AWS_BUCKET_ID"),
          region = aws_region
        )

        outcome <- glue::glue("Uploading {basename(file)} to AWS")
      } else {
        outcome <- glue::glue("{basename(file)} with matching schema already present on AWS and overwrite set to FALSE")
      }
    } else {
      # Remove the file from AWS if it's present in the folder and on AWS
      # but not in the list of successfully transformed files. This file is
      # not relevant to the pipeline
      if (file %in% s3_files) {
        outcome <- glue::glue("Cleaning up dangling file {basename(file)} from AWS")

        # Remove the file from AWS using aws.s3
        aws.s3::delete_object(
          object = file.path(file),
          bucket = Sys.getenv("AWS_BUCKET_ID"),
          region = aws_region
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
