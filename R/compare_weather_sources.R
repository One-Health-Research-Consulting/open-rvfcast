#' Compare ERA5T and MERRA-2 Weather Anomalies
#'
#' For months where both ERA5T and MERRA-2 data exist, computes ERA5T anomalies using the same
#' MERRA-2 historical means the model was trained on, then compares them to the pre-computed
#' MERRA-2 anomalies. This is the meaningful comparison because anomalies — not raw values — are
#' the actual model inputs. A directional bias here would cause ERA5T to appear anomalous relative
#' to the MERRA-2 climatology even in normal conditions, systematically shifting model predictions.
#'
#' Processes one month at a time and discards joined data immediately to keep memory use low.
#'
#' @param era5t_weather_dir Character. Directory containing era5t_weather_transformed_*.parquet files.
#' @param weather_anomalies_dir Character. Directory containing pre-computed MERRA-2 anomaly parquets
#'   (one file per date, from the weather_anomalies target).
#' @param weather_historical_means_dir Character. Directory containing MERRA-2 historical mean parquets
#'   (one file per DOY, from the weather_historical_means target). Used to compute ERA5T anomalies.
#' @param continent_raster_template Wrapped or unwrapped SpatRaster defining the continental pixel grid.
#'   If the template has non-NA values those pixels form an explicit whitelist; if all-NA masking
#'   falls back to dropping rows where MERRA-2 anomaly values are NA.
#' @param max_months Integer. Most recent overlapping months to include (default 12).
#' @param output_dir Character or NULL. If provided, the per-month stats tibble is saved here.
#'
#' @return Invisibly returns a tibble of per-month, per-variable comparison statistics with columns:
#'   month, variable, n_pairs, mean_bias, rmse, mae, correlation, mean_merra2, mean_era5t.
#'   Variables include both raw anomalies (e.g. anomaly_temperature, in original units) and
#'   scaled anomalies (e.g. anomaly_scaled_temperature, in units of historical SD) for each of
#'   temperature, precipitation, and relative humidity.
#' @export
compare_weather_sources <- function(
  era5t_weather_dir
, weather_anomalies_dir
, weather_historical_means_dir
, continent_raster_template
, max_months = 12
, output_dir = NULL
) {

  continent_raster_template <- terra::unwrap(continent_raster_template)

  # Build a continental pixel whitelist from the template.
  # terra::as.data.frame with na.rm = TRUE returns only non-NA cells; if the template is blank
  # (all NA) valid_xy will be empty and masking falls back to the MERRA-2 NA pattern instead.
  valid_xy <- terra::as.data.frame(continent_raster_template, xy = TRUE, na.rm = TRUE) |>
    dplyr::select(x, y) |>
    dplyr::mutate(x = round(x, 7), y = round(y, 7))

  ## Discover ERA5T monthly parquets and MERRA-2 anomaly per-date parquets
  era5t_files   <- list.files(era5t_weather_dir,     pattern = "\\.parquet$", full.names = TRUE)
  anomaly_files <- list.files(weather_anomalies_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(era5t_files) == 0 || length(anomaly_files) == 0) {
    message("ERA5T or MERRA-2 anomaly directory is empty. Run the relevant targets first.")
    return(invisible(NULL))
  }

  ## Extract YYYY-MM to find months present in both sources.
  ## ERA5T filenames contain the month directly; anomaly filenames contain a full date (YYYY-MM-DD)
  ## so str_extract("\\d{4}-\\d{2}") picks up the leading year-month portion.
  era5t_months  <- stringr::str_extract(basename(era5t_files),   "\\d{4}-\\d{2}")
  merra2_months <- stringr::str_extract(basename(anomaly_files), "\\d{4}-\\d{2}") |> unique()

  overlapping_months <- base::intersect(na.omit(era5t_months), na.omit(merra2_months))

  if (length(overlapping_months) == 0) {
    message("No overlapping months found. Ensure ERA5T and MERRA-2 anomaly data share at least one month.")
    return(invisible(NULL))
  }

  overlapping_months <- tail(sort(overlapping_months), max_months)
  message(glue::glue("Comparing {length(overlapping_months)} month(s): {paste(overlapping_months, collapse = ', ')}"))

  ## Both raw anomalies (original units) and scaled anomalies (units of historical SD) are compared.
  ## Scaled anomalies are the direct model inputs; raw anomalies help interpret the magnitude of any bias.
  anomaly_vars <- c(
    "anomaly_temperature",          "anomaly_scaled_temperature"
  , "anomaly_precipitation",        "anomaly_scaled_precipitation"
  , "anomaly_relative_humidity",    "anomaly_scaled_relative_humidity"
  )

  ## Process one month at a time: compute ERA5T anomalies, join to MERRA-2 anomalies, compute
  ## stats, then immediately discard the joined data to keep memory use low
  monthly_stats <- purrr::map(overlapping_months, function(m) {

    era5t_file <- era5t_files[stringr::str_detect(basename(era5t_files), m)]

    ## Anomaly files are one per date; match all days in this month
    month_anomaly_files <- anomaly_files[stringr::str_detect(basename(anomaly_files), m)]

    if (length(era5t_file) == 0 || length(month_anomaly_files) == 0) return(NULL)

    ## Load ERA5T raw weather for the month
    era5t_df <- arrow::read_parquet(era5t_file[1]) |>
      dplyr::select(x, y, date, doy, temperature, relative_humidity, precipitation) |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    ## Load MERRA-2 historical means for the DOYs present in this month.
    ## Using open_dataset allows the filter to be pushed down before collecting.
    doys_in_month    <- unique(era5t_df$doy)
    historical_means <- arrow::open_dataset(weather_historical_means_dir) |>
      dplyr::filter(doy %in% doys_in_month) |>
      dplyr::collect() |>
      dplyr::mutate(x = round(x, 7), y = round(y, 7))

    ## Compute ERA5T anomalies using the same MERRA-2 historical means and SDs that the model
    ## was trained on — this is the directly comparable quantity to the pre-computed MERRA-2 anomalies.
    ## Apply the same SD floor (0.1) used in calculate_forecast_anomalies.R to prevent extreme or
    ## infinite scaled values at pixels with near-zero historical variance (typically arid-zone precipitation).
    era5t_anomalies <- era5t_df |>
      dplyr::left_join(historical_means, by = c("x", "y", "doy"), suffix = c("", "_historical")) |>
      dplyr::mutate(
        temperature_sd               = pmax(temperature_sd,           0.1)
      , precipitation_sd             = pmax(precipitation_sd,         0.1)
      , relative_humidity_sd         = pmax(relative_humidity_sd,     0.1)
      , anomaly_temperature          = temperature       - temperature_historical
      , anomaly_scaled_temperature   = anomaly_temperature       / temperature_sd
      , anomaly_precipitation        = precipitation     - precipitation_historical
      , anomaly_scaled_precipitation = anomaly_precipitation     / precipitation_sd
      , anomaly_relative_humidity    = relative_humidity - relative_humidity_historical
      , anomaly_scaled_relative_humidity = anomaly_relative_humidity / relative_humidity_sd
      ) |>
      dplyr::select(x, y, date, dplyr::starts_with("anomaly_"))

    rm(era5t_df)

    ## Load pre-computed MERRA-2 anomalies for all dates in this month.
    ## Include doy so we can rejoin to historical_means and recompute the scaled anomalies
    ## with the same SD floor applied to the ERA5T side — the stored anomaly_scaled_* columns
    ## in the parquet files were written without any floor, leaving extreme values at
    ## arid pixels where precipitation_sd is near-zero (not exactly 0, so not caught by is.finite).
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
      dplyr::select(x, y, date, dplyr::starts_with("anomaly_"))

    rm(historical_means)

    ## Inner join on pixel-date
    joined_df <- dplyr::inner_join(
      merra2_anomalies
    , era5t_anomalies
    , by     = c("x", "y", "date")
    , suffix  = c("_merra2", "_era5t"))

    rm(merra2_anomalies, era5t_anomalies)

    ## Mask to continental pixels
    if (nrow(valid_xy) > 0) {
      joined_df <- dplyr::semi_join(joined_df, valid_xy, by = c("x", "y"))
    } else {
      joined_df <- tidyr::drop_na(joined_df, dplyr::ends_with("_merra2"))
    }

    if (nrow(joined_df) == 0) {
      message(glue::glue("No valid continental rows for {m} after masking, skipping"))
      return(NULL)
    }

    ## Compute per-variable stats then discard joined data
    stats_m <- purrr::map(anomaly_vars, ~compute_source_comparison_stats(joined_df, .x)) |>
      purrr::compact() |>
      dplyr::bind_rows() |>
      dplyr::mutate(month = m)

    rm(joined_df)
    gc(verbose = FALSE)

   print(m)

    stats_m

  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(monthly_stats) == 0) {
    message("No comparison statistics could be computed.")
    return(invisible(NULL))
  }

  message("\n=== Anomaly Comparison: ERA5T vs MERRA-2 (using MERRA-2 historical means) ===")
  message("Positive mean_bias = ERA5T anomaly is higher than MERRA-2 anomaly")
  message("For scaled variables: bias is in units of historical SD (the direct model input scale)")
  print(monthly_stats, n = Inf)

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    arrow::write_parquet(
        monthly_stats
      , file.path(output_dir, "weather_source_comparison_stats.parquet")
      , compression = "gzip", compression_level = 5)
    message(glue::glue("Comparison stats saved to {output_dir}"))
  }

  invisible(monthly_stats)
}

#' Plot Spatial Bias Maps for Weather Anomaly Comparison
#'
#' Produces a faceted ggplot of mean per-pixel anomaly bias (ERA5T minus MERRA-2) for the six
#' anomaly variables. Requires the raw joined data frame built by the caller for month(s) of
#' interest (compare_weather_sources does not retain it to save RAM).
#'
#' Example to build joined_data for one month:
#'   era5t <- arrow::read_parquet("data/era5t_weather_transformed/era5t_weather_transformed_2024-01.parquet")
#'   # compute ERA5T anomalies using MERRA-2 historical means (see compare_weather_sources internals)
#'   merra2 <- dplyr::bind_rows(lapply(list.files("data/weather_anomalies", "2024-01", full=TRUE), arrow::read_parquet))
#'   joined <- dplyr::inner_join(merra2, era5t_anomalies, by=c("x","y","date"), suffix=c("_merra2","_era5t"))
#'   plot_weather_source_comparison(joined)
#'
#' @param comparison_data Data frame with paired _merra2 and _era5t anomaly columns plus x, y.
#' @param continent_polygon sf object. If provided, overlaid as a coastline on the map.
#' @param scaled Logical. If TRUE (default) plot scaled anomalies; if FALSE plot raw anomalies.
#' @return A ggplot object.
#' @export
plot_weather_source_comparison <- function(comparison_data, continent_polygon = NULL, scaled = TRUE) {

  prefix <- if (scaled) "anomaly_scaled_" else "anomaly_"
  vars   <- c("temperature", "relative_humidity", "precipitation")

  bias_maps <- purrr::map_df(vars, function(v) {
    merra2_col <- paste0(prefix, v, "_merra2")
    era5t_col  <- paste0(prefix, v, "_era5t")
    if (!all(c(merra2_col, era5t_col) %in% names(comparison_data))) return(NULL)
    comparison_data |>
      dplyr::group_by(x, y) |>
      dplyr::summarise(
        mean_bias = mean(.data[[era5t_col]] - .data[[merra2_col]], na.rm = TRUE)
      , .groups = "drop") |>
      dplyr::mutate(variable = v)
  })

  units_label <- if (scaled) "Bias (SDs)" else "Bias (original units)"
  max_abs     <- max(abs(bias_maps$mean_bias), na.rm = TRUE)

  p <- ggplot2::ggplot(bias_maps, ggplot2::aes(x = x, y = y, fill = mean_bias)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2(
      low = "#2166ac", mid = "white", high = "#d6604d"
    , midpoint = 0, limits = c(-max_abs, max_abs)
    , name = paste0(units_label, "\n(ERA5T - MERRA-2)")) +
    ggplot2::facet_wrap(~variable, ncol = 3,
      labeller = ggplot2::labeller(variable = c(
        temperature       = "Temperature"
      , relative_humidity = "Relative Humidity"
      , precipitation     = "Precipitation"))) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Mean pixel-level anomaly bias: ERA5T minus MERRA-2 —",
                    if (scaled) "scaled (model input units)" else "raw (original units)")
    , x = NULL, y = NULL)

  if (!is.null(continent_polygon)) {
    p <- p + ggplot2::geom_sf(data = continent_polygon, fill = NA,
                               colour = "black", linewidth = 0.3, inherit.aes = FALSE)
  }

  p
}

#' Compute Comparison Statistics for One Anomaly Variable
#'
#' @param df Data frame with paired _merra2 and _era5t columns.
#' @param variable Character. Base variable name (e.g. "anomaly_temperature").
#' @return One-row tibble of comparison metrics, or NULL if the variable is absent.
compute_source_comparison_stats <- function(df, variable) {

  merra2_col <- paste0(variable, "_merra2")
  era5t_col  <- paste0(variable, "_era5t")

  if (!merra2_col %in% names(df) || !era5t_col %in% names(df)) {
    warning(glue::glue("Variable '{variable}' not found in comparison data, skipping"))
    return(NULL)
  }

  x     <- df[[merra2_col]]
  y     <- df[[era5t_col]]
  ## Exclude NA and Inf: the pre-computed MERRA-2 anomalies have no SD floor applied so
  ## pixels where precipitation_sd == 0 can produce Inf values that must be filtered here
  valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  x     <- x[valid]
  y     <- y[valid]

  if (length(x) < 2) {
    warning(glue::glue("Fewer than 2 valid pairs for '{variable}' after removing NAs"))
    return(NULL)
  }

  tibble::tibble(
    variable    = variable
  , n_pairs     = length(x)
  ## Positive mean_bias means ERA5T anomaly is higher than MERRA-2 anomaly
  , mean_bias   = mean(y - x)
  , rmse        = sqrt(mean((y - x)^2))
  , mae         = mean(abs(y - x))
  , correlation = cor(x, y)
  , mean_merra2 = mean(x)
  , mean_era5t  = mean(y)
  )
}
