transform_ndvi <- function(ndvi_transformed_sources,
                           ndvi_transformed_directory,
                           basename_template = "ndvi_transformed_{.y}_{.m}.parquet",
                           overwrite = FALSE,
                           ...) {

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
    if(row_count > 0) {
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

  # Open dataset for this month
  ndvi_transformed_dataset <- arrow::open_dataset(all_ndvi_files)

  # Process single month instead of all months
  ndvi_transformed_data <- ndvi_transformed_dataset |>
    dplyr::filter(year == .y,
           month == .m) |>
           collect()

  message(paste("Processing", nrow(ndvi_transformed_data), "rows"))

  ## It appears that sentinel NDVI after about Feb 2025 comes already scaled??
   ## Dividing an already-scaled value by 200 again silently corrupts it,
   ## so detect which format this month's Sentinel data is in before deciding whether to rescale.
  sentinel_vals <- ndvi_transformed_data$ndvi[ndvi_transformed_data$source != "modis"]
  sentinel_already_scaled <- length(sentinel_vals) > 0 && max(abs(sentinel_vals), na.rm = TRUE) <= 1.5

  ## Same issue observed for MODIS -- some months arrive already scaled to the standard
   ## -1 to 1 range rather than needing the /10000 correction, so check this source too.
  modis_vals <- ndvi_transformed_data$ndvi[ndvi_transformed_data$source == "modis"]
  modis_already_scaled <- length(modis_vals) > 0 && max(abs(modis_vals), na.rm = TRUE) <= 1.5

  ## Scale raw data appropriately.
  ndvi_transformed_data <- ndvi_transformed_data |>
    mutate(
      ndvi = case_when(
        modis_already_scaled & source == "modis" ~ ndvi, ## MODIS, already scaled upstream
        source == "modis" ~ ndvi / 10000,                ## MODIS, raw digital-number scale
        sentinel_already_scaled ~ ndvi,                  ## Sentinel, already scaled upstream
        TRUE ~ ndvi / 200                                ## Sentinel, raw digital-number scale
      )
    )

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
