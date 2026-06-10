#' Calculate ERA5T Weather Anomalies with Bias Correction
#'
#' Converts one monthly ERA5T raw weather parquet into per-date scaled anomaly parquets
#' that can serve as a drop-in replacement for the NASA POWER anomaly files produced by
#' calculate_weather_anomalies. The conversion has three steps:
#'
#'   1. Compute raw anomalies (ERA5T - MERRA-2 historical mean) and scale by the MERRA-2
#'      historical SD with a floor of 0.1 to prevent extreme values at arid pixels.
#'   2. Join the per-pixel bias correction (ERA5T_scaled - MERRA2_scaled mean over a
#'      calibration period) and subtract it from each scaled anomaly.
#'   3. Write one parquet per date to weather_anomalies_era5t_directory in the same schema
#'      as weather_anomaly_{date}.parquet files produced by calculate_weather_anomalies.
#'
#' The output column names are identical to the NASA POWER anomaly files so the predictor
#' data assembly step can use either source without modification.
#'
#' @param era5t_weather_transformed_file Character. Path to one monthly ERA5T parquet from
#'   fetch_and_transform_era5t_weather.
#' @param bias_correction_file Character. Path to the parquet produced by
#'   get_or_compute_era5t_bias_correction (columns: x, y, variable, bias_offset, n_obs).
#' @param weather_historical_means_dir Character. Directory of MERRA-2 historical mean parquets.
#' @param weather_anomalies_era5t_directory Character. Output directory for per-date anomaly
#'   parquets. Files are named weather_anomaly_{date}.parquet.
#' @param basename_template Character. Glue template used to construct per-date output filenames.
#' @param overwrite Logical. Re-compute even if output files already exist.
#' @param ... Ignored; allows a targets dependency (e.g. era5t_weather_anomalies_AWS) to be
#'   passed positionally without triggering an "unused argument" error.
#'
#' @return Character vector of file paths written (one per date in the monthly parquet). NULL
#'   entries are removed so the vector can be used directly with format = "file" in targets.
#' @export
calculate_era5t_weather_anomalies <- function(
  era5t_weather_transformed_file
, bias_correction_file
, weather_historical_means_dir
, weather_anomalies_era5t_directory
, ...
, basename_template = "weather_anomaly_{date}.parquet"
, overwrite         = FALSE
) {

  if (!file.exists(era5t_weather_transformed_file)) {
    message(glue::glue("ERA5T weather file not found: {era5t_weather_transformed_file}"))
    return(character(0))
  }

  if (!file.exists(bias_correction_file)) {
    message(glue::glue("Bias correction file not found: {bias_correction_file}"))
    return(character(0))
  }

  ## Load monthly ERA5T raw weather
  era5t_monthly <- arrow::read_parquet(era5t_weather_transformed_file) |>
    dplyr::select(x, y, date, doy, temperature, relative_humidity, precipitation) |>
    dplyr::mutate(x = round(x, 4), y = round(y, 4))

  dates_in_file <- sort(unique(era5t_monthly$date))
  if (length(dates_in_file) == 0) return(character(0))

  ## Load MERRA-2 historical means for every DOY present in this month.
  ## open_dataset pushes the doy filter down before collecting to keep RAM use low.
  doys_in_file      <- unique(era5t_monthly$doy)
  historical_means <- arrow::open_dataset(weather_historical_means_dir) |>
    dplyr::filter(doy %in% doys_in_file) |>
    dplyr::collect() |>
    dplyr::mutate(x = round(x, 4), y = round(y, 4))

  ## Reshape bias correction from long (x, y, variable, bias_offset) to wide (x, y, bias_*) for
  ## a single join. Coalesce to 0 so pixels missing from the calibration data are not shifted.
  bias_wide <- arrow::read_parquet(bias_correction_file) |>
    dplyr::mutate(
      bias_col = dplyr::case_when(
        variable == "anomaly_scaled_temperature"       ~ "bias_temperature"
      , variable == "anomaly_scaled_precipitation"     ~ "bias_precipitation"
      , variable == "anomaly_scaled_relative_humidity" ~ "bias_relative_humidity"
      , TRUE                                           ~ NA_character_
      )) |>
    dplyr::filter(!is.na(bias_col)) |>
    dplyr::select(x, y, bias_col, bias_offset) |>
    dplyr::mutate(x = round(x, 4), y = round(y, 4)) |>
    tidyr::pivot_wider(id_cols = c(x, y), names_from = bias_col, values_from = bias_offset)

  ## Compute anomalies for the full month at once so historical_means is only joined once,
  ## then split and write one parquet per date
  anomalies_monthly <- era5t_monthly |>
    dplyr::left_join(historical_means, by = c("x", "y", "doy"), suffix = c("", "_historical")) |>
    dplyr::mutate(
      temperature_sd                   = pmax(temperature_sd,       0.1)
    , precipitation_sd                 = pmax(precipitation_sd,     0.1)
    , relative_humidity_sd             = pmax(relative_humidity_sd, 0.1)
    , anomaly_temperature              = temperature       - temperature_historical
    , anomaly_precipitation            = precipitation     - precipitation_historical
    , anomaly_relative_humidity        = relative_humidity - relative_humidity_historical
    , anomaly_scaled_temperature       = anomaly_temperature       / temperature_sd
    , anomaly_scaled_precipitation     = anomaly_precipitation     / precipitation_sd
    , anomaly_scaled_relative_humidity = anomaly_relative_humidity / relative_humidity_sd
    , year                             = as.integer(lubridate::year(date))
    , month                            = as.integer(lubridate::month(date))
    ) |>
    dplyr::select(x, y, date, doy, month, year,
                  anomaly_temperature, anomaly_scaled_temperature,
                  anomaly_precipitation, anomaly_scaled_precipitation,
                  anomaly_relative_humidity, anomaly_scaled_relative_humidity) |>
    dplyr::left_join(bias_wide, by = c("x", "y")) |>
    dplyr::mutate(
      ## Apply bias correction only to scaled anomalies (correction is defined in SD units).
      ## Raw anomaly columns (°C, mm/day, %) are kept as-is to preserve schema compatibility.
      anomaly_scaled_temperature       = anomaly_scaled_temperature       - dplyr::coalesce(bias_temperature,       0)
    , anomaly_scaled_precipitation     = anomaly_scaled_precipitation     - dplyr::coalesce(bias_precipitation,     0)
    , anomaly_scaled_relative_humidity = anomaly_scaled_relative_humidity - dplyr::coalesce(bias_relative_humidity, 0)
    ) |>
    dplyr::select(-dplyr::starts_with("bias_"))

  ## Write one parquet per date; return file paths for targets to track
  output_files <- purrr::map_chr(dates_in_file, function(date) {

    output_file <- file.path(weather_anomalies_era5t_directory, glue::glue(basename_template))

    if (file.exists(output_file) && !overwrite) {
      message(glue::glue("{basename(output_file)} already exists, skipping"))
      return(output_file)
    }

    date_df <- dplyr::filter(anomalies_monthly, .data$date == .env$date)
    if (nrow(date_df) == 0) return(NA_character_)

    arrow::write_parquet(date_df, output_file, compression = "gzip", compression_level = 5)
    output_file

  })

  stats::na.omit(output_files)

}
