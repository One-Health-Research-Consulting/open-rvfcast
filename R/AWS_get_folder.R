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
  # Return immediately if skip_fetch is TRUE
  if (skip_fetch) {
    return(character(0))
  }

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
      row_count <- nrow(dataset)

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
                                       region = aws_region,
                                       base_url = Sys.getenv("AWS_S3_ENDPOINT"))

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
      # Download the file from S3 using aws.s3.
      # aws.s3 can throw a spurious "if (content != '')" error when the HTTP
      # response body is empty/NA even though the file downloaded successfully;
      # catch it and re-throw only if the file is missing.
      tryCatch(
        aws.s3::save_object(
          object = file,
          bucket = Sys.getenv("AWS_BUCKET_ID"),
          region = aws_region,
          file = file
        ),
        error = function(e) {
          if (!file.exists(file)) stop(e)
          invisible(NULL)
        }
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
#'   \item Verifies file schemas and row counts before uploading
#'   \item Supports selective file upload based on schema matching and data changes
#'   \item Optionally overwrites existing files on AWS
#'   \item Cleans up dangling files from the S3 bucket when requested
#' }
#'
#' @author Nathan C. Layman
#'
#' @param transformed_file_list A character vector of filenames to be uploaded to AWS S3.
#'   These should be full file paths that have been transformed and are ready for upload.
#' @param local_folder A character string specifying the local directory containing
#'   the transformed files to be uploaded to AWS S3.
#' @param overwrite Logical. If \code{TRUE}, files will be uploaded even if they
#'   already exist in the S3 bucket with matching schemas and row counts. Defaults to \code{FALSE}.
#' @param clean_remote Logical. If \code{TRUE}, files present on AWS but not in the
#'   \code{transformed_file_list} will be deleted from the S3 bucket. Defaults to \code{FALSE}.
#'   Use with caution as this can delete files during testing.
#' @param ... Additional arguments (currently unused).
#'
#' @return A character vector of messages describing the outcomes of file upload attempts,
#'   including successful uploads, failed uploads, skipped files, and cleanup operations.
#'
#' @note
#' Required environment variables:
#' \itemize{
#'   \item \code{AWS_ACCESS_KEY_ID}: AWS access key
#'   \item \code{AWS_SECRET_ACCESS_KEY}: AWS secret access key
#'   \item \code{AWS_REGION}: AWS region (can be "auto" for automatic detection)
#'   \item \code{AWS_BUCKET_ID}: S3 bucket identifier
#'   \item \code{AWS_S3_ENDPOINT}: S3 endpoint URL (optional, for custom S3-compatible services)
#' }
#' These environment variables must be set prior to calling the function, typically
#' in a .env file or system environment.
#'
#' @examples
#' \dontrun{
#' # Upload transformed Parquet files from a local directory
#' AWS_put_files(
#'   transformed_file_list = c("./data/file1.parquet", "./data/file2.parquet"),
#'   local_folder = "./data"
#' )
#'
#' # Upload with overwrite option
#' AWS_put_files(
#'   transformed_file_list = c("./data/file1.parquet", "./data/file2.parquet"),
#'   local_folder = "./data",
#'   overwrite = TRUE
#' )
#'
#' # Upload and clean remote files not in the transformed list
#' AWS_put_files(
#'   transformed_file_list = c("./data/file1.parquet"),
#'   local_folder = "./data",
#'   clean_remote = TRUE
#' )
#' }
#'
#' @importFrom aws.s3 get_bucket put_object delete_object
#' @importFrom arrow open_dataset s3_bucket ParquetFileReader
#' @importFrom glue glue
#' @importFrom purrr possibly map_chr pluck
#'
#' @export
AWS_put_files <- function(transformed_file_list,
                          local_folder,
                          overwrite = FALSE,
                          ...) {

  library(arrow)

  # Check if AWS credentials and region are set in the environment
  if (any(Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION")) == "")) {
    msg <- paste(
      "AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION environment variables",
      "must all be set to access AWS. Please ensure they are configured correctly,",
      "probably in the .env file or system environment."
    )
    stop(msg)
  }

  # Create a error tolerant version of the function
  error_safe_open_dataset <- function(file, fs = NULL) {
    tryCatch({
      arrow::open_dataset(file, filesystem = fs)
    }, error = function(e) {
      cat("Error opening dataset for file:", file, "\n")
      cat("Error message:", e$message, "\n")
      return(NULL)
    })
  }

  aws_region = if (Sys.getenv("AWS_REGION") == "auto") "" else Sys.getenv("AWS_REGION")

  s3_fs <- arrow::S3FileSystem$create(
    endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"),
    region = aws_region,
    access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY")
  )

  # Collect outcomes
  outcomes <- c()

  # Walk through transformed_file_list (can be a single file or vector of files)
  for (file in transformed_file_list) {

    # Get dataset object
    remote_dataset <- error_safe_open_dataset(paste0(Sys.getenv("AWS_BUCKET_ID"), "/", file), fs = s3_fs)
    local_dataset <- error_safe_open_dataset(file)

    if (is.null(remote_dataset) || !remote_dataset$schema$Equals(local_dataset$schema) || remote_dataset$num_rows != local_dataset$num_rows || overwrite == TRUE) {

      # Put the file on S3 using aws.s3
      upload_result <- aws.s3::put_object(
        file = file,
        object = file,
        multipart = TRUE,
        part_size = 10485760,
        bucket = Sys.getenv("AWS_BUCKET_ID"),
        region = aws_region
      )

      if (upload_result) {
        outcome <- glue::glue("Successfully uploaded {basename(file)} to AWS")
      } else {
        outcome <- glue::glue("Failed to upload {basename(file)} to AWS")
      }
    } else {
      outcome <- glue::glue("{basename(file)} already exists on AWS with matching rows and schema and overwrite is not TRUE, skipping upload")
    }

    message(outcome)
    outcomes <- c(outcomes, outcome)
  }

  outcomes
}
