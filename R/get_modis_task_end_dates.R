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
get_modis_task_end_dates <- function(modis_ndvi_transformed_directory, ...) {

  all_end_dates <- c(seq(as.Date("2005-12-31"), Sys.Date(), by = "year"), Sys.Date()) |> unique()

  complete_years <- modis_years_with_complete_data(modis_ndvi_transformed_directory)
  current_year   <- as.integer(format(Sys.Date(), "%Y"))

  # Always keep the current year so new composites are fetched each run
  years_to_skip <- setdiff(complete_years, current_year)

  all_end_dates[!(lubridate::year(all_end_dates) %in% years_to_skip)]

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
