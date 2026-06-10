#' Compute Per-Pixel ERA5T Bias Correction
#'
#' For each continental pixel and each of the three scaled weather anomaly variables
#' (temperature, precipitation, relative humidity), computes the mean signed difference
#' ERA5T_scaled − MERRA2_scaled over all overlapping months. This offset is the bias
#' correction: subtracting it from a future ERA5T scaled anomaly brings it onto the
#' MERRA-2 climatological scale that the model was trained on.
#'
#' Uses the same SD floor (0.1) and continental masking as compare_weather_sources and
#' calculate_forecast_anomalies so the correction is consistent with how anomalies enter
#' the model at prediction time.
#'
#' Processes one month at a time to keep peak memory use low, accumulating per-pixel running
#' sums rather than holding all months in memory simultaneously.
#'
#' @param era5t_weather_dir Character scalar or vector. One or more directories containing
#'   era5t_weather_transformed_*.parquet (or calibration_*.parquet) files. Multiple directories
#'   are combined so that extra historical calibration months can be passed alongside the live
#'   ERA5T folder without needing to copy files.
#' @param weather_anomalies_dir Character. Directory of pre-computed MERRA-2 anomaly parquets
#'   (one per date, from the weather_anomalies target).
#' @param weather_historical_means_dir Character. Directory of MERRA-2 historical mean parquets
#'   (one per DOY, from the weather_historical_means target).
#' @param continent_raster_template Wrapped or unwrapped SpatRaster defining valid analysis pixels.
#' @param output_file Character. Full path for the output parquet file.
#' @param overwrite Logical. Re-compute even if the output file already exists.
#'
#' @return Character. Path to the saved bias correction parquet, invisibly. The file contains
#'   columns x, y, variable, bias_offset, n_obs. Apply at forecast time as:
#'   ERA5T_scaled_corrected = ERA5T_scaled - bias_offset
#' @export
compute_era5t_bias_correction <- function(
  era5t_weather_dir
, weather_anomalies_dir
, weather_historical_means_dir
, continent_raster_template
, output_file  = "data/era5t_bias_correction.parquet"
, overwrite   = FALSE
) {

  if (file.exists(output_file) && !overwrite) {
    message(glue::glue("{basename(output_file)} already exists and overwrite is FALSE, skipping"))
    return(invisible(output_file))
  }

  continent_raster_template <- terra::unwrap(continent_raster_template)

  # Build continental pixel whitelist
  valid_xy <- terra::as.data.frame(continent_raster_template, xy = TRUE, na.rm = TRUE) |>
    dplyr::select(x, y) |>
    dplyr::mutate(x = round(x, 7), y = round(y, 7))

  ## Discover files and find overlapping months (era5t_weather_dir may be a vector of directories)
  era5t_files   <- unlist(lapply(era5t_weather_dir, list.files, pattern = "\\.parquet$", full.names = TRUE))
  anomaly_files <- list.files(weather_anomalies_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(era5t_files) == 0 || length(anomaly_files) == 0) {
    stop("ERA5T or MERRA-2 anomaly directory is empty — run the relevant targets first.")
  }

  era5t_months       <- stringr::str_extract(basename(era5t_files),   "\\d{4}-\\d{2}")
  merra2_months      <- stringr::str_extract(basename(anomaly_files), "\\d{4}-\\d{2}") |> unique()
  overlapping_months <- sort(base::intersect(na.omit(era5t_months), na.omit(merra2_months)))

  if (length(overlapping_months) == 0) {
    stop("No overlapping months found between ERA5T and MERRA-2 anomaly data.")
  }

  message(glue::glue("Computing bias correction from {length(overlapping_months)} month(s): {paste(overlapping_months, collapse = ', ')}"))

  scaled_vars <- c(
    "anomaly_scaled_temperature"
  , "anomaly_scaled_precipitation"
  , "anomaly_scaled_relative_humidity"
  )

  ## Accumulate per-pixel running sums (sum of ERA5T - MERRA2, and count) across all months.
  ## Keeping sums rather than concatenating rows avoids holding multiple months in memory.
  pixel_accum <- NULL

  for (m in overlapping_months) {

    era5t_file          <- era5t_files[stringr::str_detect(basename(era5t_files), m)]
    month_anomaly_files <- anomaly_files[stringr::str_detect(basename(anomaly_files), m)]

    if (length(era5t_file) == 0 || length(month_anomaly_files) == 0) next

    message(glue::glue("  {m}"))

    ## Compute ERA5T scaled anomalies using MERRA-2 historical means + SD floor
    era5t_df <- arrow::read_parquet(era5t_file[1]) |>
      dplyr::select(x, y, date, doy, temperature, relative_humidity, precipitation) |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    doys_in_month    <- unique(era5t_df$doy)
    historical_means <- arrow::open_dataset(weather_historical_means_dir) |>
      dplyr::filter(doy %in% doys_in_month) |>
      dplyr::collect() |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    era5t_anomalies <- era5t_df |>
      dplyr::left_join(historical_means, by = c("x", "y", "doy"), suffix = c("", "_historical")) |>
      dplyr::mutate(
        temperature_sd                   = pmax(temperature_sd,       0.1)
      , precipitation_sd                 = pmax(precipitation_sd,     0.1)
      , relative_humidity_sd             = pmax(relative_humidity_sd, 0.1)
      , anomaly_scaled_temperature       = (temperature       - temperature_historical)   / temperature_sd
      , anomaly_scaled_precipitation     = (precipitation     - precipitation_historical) / precipitation_sd
      , anomaly_scaled_relative_humidity = (relative_humidity - relative_humidity_historical) / relative_humidity_sd
      ) |>
      dplyr::select(x, y, date, dplyr::all_of(scaled_vars))

    rm(era5t_df)

    ## Compute MERRA-2 scaled anomalies with the same SD floor
    merra2_anomalies <- purrr::map(month_anomaly_files, arrow::read_parquet) |>
      dplyr::bind_rows() |>
      dplyr::select(x, y, date, doy, anomaly_temperature, anomaly_precipitation, anomaly_relative_humidity) |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7)) |>
      dplyr::left_join(
        dplyr::select(historical_means, x, y, doy, temperature_sd, precipitation_sd, relative_humidity_sd)
      , by = c("x", "y", "doy")) |>
      dplyr::mutate(
        anomaly_scaled_temperature          = anomaly_temperature       / pmax(temperature_sd,       0.1)
      , anomaly_scaled_precipitation        = anomaly_precipitation     / pmax(precipitation_sd,     0.1)
      , anomaly_scaled_relative_humidity    = anomaly_relative_humidity / pmax(relative_humidity_sd, 0.1)
      ) |>
      dplyr::select(x, y, date, dplyr::all_of(scaled_vars))

    rm(historical_means)

    ## Join, mask, and compute per-pixel running sums of ERA5T - MERRA2 for each variable
    joined_df <- dplyr::inner_join(
      merra2_anomalies
    , era5t_anomalies
    , by     = c("x", "y", "date")
    , suffix  = c("_merra2", "_era5t"))

    rm(merra2_anomalies, era5t_anomalies)

    if (nrow(valid_xy) > 0) {
      joined_df <- dplyr::semi_join(joined_df, valid_xy, by = c("x", "y"))
    }

    ## Aggregate to per-pixel sums for this month, then discard the full joined data
    month_sums <- purrr::map(scaled_vars, function(v) {
      merra2_col <- paste0(v, "_merra2")
      era5t_col  <- paste0(v, "_era5t")
      joined_df |>
        dplyr::transmute(x, y, diff = .data[[era5t_col]] - .data[[merra2_col]]) |>
        dplyr::filter(is.finite(diff)) |>
        dplyr::group_by(x, y) |>
        dplyr::summarise(sum_diff = sum(diff), n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(variable = v)
    }) |>
      dplyr::bind_rows()

    rm(joined_df)
    gc(verbose = FALSE)

    ## Merge this month's sums into the running accumulator
    if (is.null(pixel_accum)) {
      pixel_accum <- month_sums
    } else {
      pixel_accum <- dplyr::bind_rows(pixel_accum, month_sums) |>
        dplyr::group_by(x, y, variable) |>
        dplyr::summarise(sum_diff = sum(sum_diff), n = sum(n), .groups = "drop")
    }

    print(m)

  }

  if (is.null(pixel_accum) || nrow(pixel_accum) == 0) {
    stop("No valid pixel-pairs accumulated. Check that ERA5T and MERRA-2 anomaly data overlap spatially.")
  }

  ## Compute final per-pixel mean bias offset
  bias_correction <- pixel_accum |>
    dplyr::mutate(bias_offset = sum_diff / n) |>
    dplyr::select(x, y, variable, bias_offset, n_obs = n)

  ## Report summary statistics so the caller can sanity-check the result
  summary_stats <- bias_correction |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      mean_bias  = mean(bias_offset,   na.rm = TRUE)
    , sd_bias    = stats::sd(bias_offset,     na.rm = TRUE)
    , n_days_pp  = round(mean(n_obs) / length(overlapping_months))  # avg days per pixel per month
    , .groups    = "drop")

  message("\n=== Bias correction summary (mean pixel-level offset: ERA5T - MERRA2) ===")
  message("Apply at forecast time as: ERA5T_scaled_corrected = ERA5T_scaled - bias_offset\n")
  print(as.data.frame(summary_stats))
  message(glue::glue("\nCorrection derived from {length(overlapping_months)} months ({overlapping_months[1]} to {tail(overlapping_months, 1)})"))

  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(bias_correction, output_file, compression = "gzip", compression_level = 5)
  message(glue::glue("Saved to {output_file}"))

  invisible(output_file)

}
