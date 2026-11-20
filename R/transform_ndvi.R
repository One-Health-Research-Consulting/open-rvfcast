transform_ndvi <- function(ndvi_transformed_sources,
                           ndvi_transformed_directory,
                           basename_template = "ndvi_transformed_{.y}_{.m}.parquet",
                           overwrite = FALSE,
                           ...,
                           ## c(intercept, slope) mapping Sentinel NDVI onto the MODIS scale before
                            ## blending; after ... so a positional targets dependency never lands here
                           sentinel_calibration = NULL) {

  # Extract month and file lists from the single-row tibble
  stopifnot(nrow(ndvi_transformed_sources) == 1)
  months_to_process <- ndvi_transformed_sources$month

  # Parse year-month string (format: "YYYY-MM") - with dynamic branching, this is a single value
  .y <- as.numeric(substr(months_to_process, 1, 4))
  .m <- as.numeric(substr(months_to_process, 6, 7))

  # Set filename
  save_filename <- file.path(ndvi_transformed_directory, glue::glue(basename_template))

  # Check for an existing output file BEFORE touching source files. Historical months
  # may have NULL source files because modis_task_end_dates now skips complete years,
  # but their processed parquets are already present locally (or synced from S3 via
  # ndvi_transformed_AWS). Returning early here avoids a spurious "no source files" error.
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  existing_dataset <- error_safe_read_parquet(save_filename)

  if(!is.null(existing_dataset) & !overwrite) {
    # Check if file has data - if zero rows, overwrite anyway
    row_count <- existing_dataset |> count() |> collect() |> pull(n)
    # Days of the month actually present; a month built from incomplete sources
    # (e.g., missing Sentinel 10-day products) must be rebuilt rather than skipped forever
    n_dates_present <- existing_dataset |> distinct(date) |> collect() |> nrow()
    n_days_in_month <- lubridate::days_in_month(as.Date(sprintf("%d-%02d-01", .y, .m)))
    if(row_count > 0 && n_dates_present < n_days_in_month) {
      message(glue::glue("{basename(save_filename)} covers only {n_dates_present} of {n_days_in_month} days, rebuilding"))
    } else if(row_count > 0) {
      message(glue::glue("{basename(save_filename)} already exists, has rows, and overwrite is not TRUE, skipping"))
      return(save_filename)
    } else {
      message(glue::glue("{basename(save_filename)} exists but has zero rows, overwriting"))
    }
  } else if(!is.null(existing_dataset) & overwrite) {
    # File exists and overwrite is TRUE
    message(glue::glue("Overwriting existing {basename(save_filename)} for {months_to_process}"))
  } else {
    # File doesn't exist - we're transforming
    message(paste("Combining ndvi sources for", .y, "month", .m))
  }

  # Extract and unlist file paths, handling NULL values
  modis_files <- ndvi_transformed_sources$modis_files[[1]]
  sentinel_files <- ndvi_transformed_sources$sentinel_files[[1]]

  # Combine files, removing any NAs or NULLs
  all_ndvi_files <- c(
    if(!is.null(modis_files)) modis_files else character(0),
    if(!is.null(sentinel_files)) sentinel_files else character(0)
  )
  all_ndvi_files <- all_ndvi_files[!is.na(all_ndvi_files)]

  if (length(all_ndvi_files) == 0) {
    stop(glue::glue("No NDVI files available for month {months_to_process}"))
  }

  # Validate that files exist
  missing_files <- all_ndvi_files[!file.exists(all_ndvi_files)]
  if (length(missing_files) > 0) {
    stop(glue::glue("NDVI files do not exist: {paste(missing_files, collapse=', ')}"))
  }

  # NCL I noticed some issues with duplicates in modis_transformed. Check why.
  # There is a duplicate filename in the modis_ndvi_transformed target. WHY?
  # data/modis_ndvi_transformed/transformed_modis_NDVI_2018-12-19.parquet
  # Removing that file fixed the problem of duplicates with arrow::open_dataset at least for 2018

  # For sentinel the problem is duplication of boundary date data
  # "data/sentinel_ndvi_transformed/transformed_sentinel_NDVI_2018-10-12_to_2018-10-22.parquet" and
  # "data/sentinel_ndvi_transformed/transformed_sentinel_NDVI_2018-10-22_to_2018-11-01.parquet"
  # Has 2018-10-22 in both files.

  ## Read each source file for this month on its own and put it on the -1 to 1 scale.
   ## Scale is decided per file, not per month: a month can mix files stored on the raw
   ## digital-number scale with files that are already scaled (e.g. July 2026 drew on a raw
   ## MODIS composite starting in June plus already-scaled July composites), and a single
   ## month-wide check then divides the already-scaled files a second time
  ndvi_transformed_data <- purrr::map(all_ndvi_files, read_scaled_ndvi_file, .y = .y, .m = .m) |>
    dplyr::bind_rows()

  ## Put Sentinel on the MODIS scale so the blend does not step when Sentinel enters (2019) and
   ## the long MODIS-only baseline years stay comparable with the blended years
  if (!is.null(sentinel_calibration)) {
    ndvi_transformed_data <- ndvi_transformed_data |>
      dplyr::mutate(ndvi = ifelse(source == "modis", ndvi, sentinel_calibration[1] + sentinel_calibration[2] * ndvi))
  }

  message(paste("Processing", nrow(ndvi_transformed_data), "rows"))

  ndvi_transformed_data <- ndvi_transformed_data |>
    select(-source) |>
    # Round coordinates before grouping: MODIS and Sentinel source rasters occasionally
    # disagree on a pixel's x/y by a sub-1e-7 floating point epsilon, which otherwise
    # makes group_by() treat them as two separate pixels and silently skip the blend below
    mutate(x = round(x, 7), y = round(y, 7)) |>
    group_by(x, y, date, doy, month, year) |>
    summarize(ndvi = mean(ndvi), .groups = "drop")

  arrow::write_parquet(ndvi_transformed_data, save_filename, compression = "gzip", compression_level = 5)

  return(save_filename)

}

#' Read One NDVI Source File for a Month and Put It on the -1 to 1 Scale
#'
#' MODIS and Sentinel parquets are stored either on their raw digital-number scale
#' (MODIS x10000, Sentinel x200) or already scaled, depending on when they were built.
#' The scale is detected from this file's own values so files of both kinds can be
#' blended in one month.
#'
#' @param file Character. Path to a MODIS or Sentinel transformed NDVI parquet.
#' @param .y,.m Integer. Year and month to keep.
#'
#' @return Tibble with this file's rows for the month, ndvi on the -1 to 1 scale.
read_scaled_ndvi_file <- function(file, .y, .m) {

  # Rows of this file that fall in the requested month
  file_data <- arrow::open_dataset(file) |>
    dplyr::filter(year == .y, month == .m) |>
    dplyr::collect()

  if (nrow(file_data) == 0) return(file_data)

  # Values within +/-1.5 can only be already scaled; raw digital numbers run into the thousands
  already_scaled <- max(abs(file_data$ndvi), na.rm = TRUE) <= 1.5

  ## Sentinel parquets are written as true NDVI by read_sentinel_ndvi_nc and the older ones were
   ## corrected in place (2026-11-19). A raw-scale Sentinel file cannot be decoded safely here:
   ## older files used two different raw scales (DN and NDVI x 200) that overlap in range
  if (!already_scaled && any(file_data$source != "modis")) {
    stop(glue::glue("{basename(file)} holds raw-scale Sentinel NDVI; re-transform it with transform_sentinel_ndvi"))
  }

  # Divisor for raw values depends on the sensor
  raw_divisor <- ifelse(file_data$source == "modis", 10000, 200)

  file_data |>
    dplyr::mutate(ndvi = if (already_scaled) ndvi else ndvi / raw_divisor)

}
