#' Generate MODIS NDVI task end dates, skipping years with complete local data
#'
#' Produces the same yearly end-date sequence as the original pipeline
#' (Dec 31 of every year from 2005 through today, plus today itself), but
#' removes years whose 16-day composites are already fully downloaded.
#' A past year is considered complete when at least one December parquet file
#' exists in the transformed directory. The current year is always included so
#' the pipeline keeps pulling new composites as they are published.
#'
#' @param modis_ndvi_transformed_directory Path to the local directory
#'   containing transformed MODIS NDVI parquet files.
#'
#' @return A Date vector of end dates to request from AρρEEARS, with complete
#'   past years excluded.
#'
#' @examples
#' get_modis_task_end_dates("data/modis_ndvi_transformed")
#'
#' @export
get_modis_task_end_dates <- function(modis_ndvi_transformed_directory, dates_to_process = NULL, ...) {

  all_end_dates <- c(seq(as.Date("2005-12-31"), Sys.Date(), by = "year"), Sys.Date()) |> unique()

  # When dates_to_process is provided, restrict to only years containing those
  # dates. This avoids submitting AppEEARS tasks for the full 2005-to-present
  # backlog on a clean machine where no local files exist.
  if (!is.null(dates_to_process) && length(dates_to_process) > 0) {
    needed_years  <- unique(lubridate::year(as.Date(dates_to_process)))
    all_end_dates <- all_end_dates[lubridate::year(all_end_dates) %in% needed_years]
  }

  complete_years <- modis_years_with_complete_data(modis_ndvi_transformed_directory)
  current_year   <- as.integer(format(Sys.Date(), "%Y"))

  # Always keep the current year so new composites are fetched each run
  years_to_skip <- setdiff(complete_years, current_year)

  all_end_dates <- all_end_dates[!(lubridate::year(all_end_dates) %in% years_to_skip)]

  # If dates_to_process is provided, check whether local parquets already cover all
  # needed dates (dates_to_process + 21-day NDVI lookback). modis_ndvi_transformed_AWS
  # runs before this function and downloads any needed files from S3, so local files
  # found here reflect the current state of the S3 bucket. If covered, return an NA
  # sentinel so the AppEEARS polling chain is skipped entirely.
  if (!is.null(dates_to_process) && length(dates_to_process) > 0 && length(all_end_dates) > 0) {
    dates_needed <- seq.Date(
      min(as.Date(dates_to_process)) - 21L
    , max(as.Date(dates_to_process))
    , by = "day")

    local_files <- list.files(
      modis_ndvi_transformed_directory
    , pattern = "^transformed_modis_NDVI_\\d{4}-\\d{2}-\\d{2}\\.parquet$"
    )

    if (length(local_files) > 0) {
      parquet_starts <- as.Date(sub(
        "transformed_modis_NDVI_(\\d{4}-\\d{2}-\\d{2})\\.parquet$"
      , "\\1", local_files))

      all_covered <- all(vapply(dates_needed, function(d) {
        any(parquet_starts <= d & d <= parquet_starts + 15L)
      }, logical(1L)))

      if (all_covered) {
        message("All needed MODIS NDVI dates covered by local parquets; skipping AppEEARS request")
        return(as.Date(NA_character_))
      }
    }
  }

  all_end_dates

}

# Returns integer years that have at least one December parquet file locally,
# indicating all 16-day composites for that year have been downloaded.
modis_years_with_complete_data <- function(modis_ndvi_transformed_directory) {

  existing_files <- list.files(
    modis_ndvi_transformed_directory,
    pattern = "^transformed_modis_NDVI_\\d{4}-\\d{2}-\\d{2}\\.parquet$"
  )

  if (length(existing_files) == 0) return(integer(0))

  # Extract year and month from "transformed_modis_NDVI_YYYY-MM-DD.parquet"
  file_years  <- as.integer(sub("transformed_modis_NDVI_(\\d{4})-\\d{2}-\\d{2}\\.parquet", "\\1", existing_files))
  file_months <- as.integer(sub("transformed_modis_NDVI_\\d{4}-(\\d{2})-\\d{2}\\.parquet", "\\1", existing_files))

  unique(file_years[file_months == 12L])

}
