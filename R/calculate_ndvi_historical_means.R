#' Calculate historical means for Normalized Difference Vegetation Index (NDVI)
#'
#' This function calculates historical means for NDVI based on the already-blended,
#' already-scaled ndvi_transformed data, then saves the results in a specified directory.
#'
#' @author Nathan Layman
#'
#' @param ndvi_transformed The blended, scaled monthly NDVI data (see transform_ndvi).
#' @param ndvi_historical_means_directory The directory where the results will be saved.
#' @param ndvi_historical_means_AWS The historical NDVI means from AWS.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A vector of filepaths to saved parquet files of calculated historical NDVI means.
#'
#' @note
#' This function works by grouping the provided NDVI data by x,y coordinates and day of year (doy),
#' then calculating the mean and standard deviation for each group. The results are then saved
#' into parquet files (one for each day of the year).
#'
#' Reads from ndvi_transformed rather than the raw modis_ndvi_transformed/sentinel_ndvi_transformed
#' sources directly, so that the historical baseline is computed on the same scaled, source-blended
#' values as the "current" side of the anomaly calculation (see calculate_ndvi_anomalies) -- pooling
#' raw MODIS and Sentinel values (on different, and at times inconsistent, native scales) without
#' first scaling them, as this function previously did, produces a baseline mean/sd that is not
#' comparable to the current value at all.
#'
#' @examples
#' calculate_ndvi_historical_means(ndvi_transformed = "path_to_ndvi_transformed_data",
#'                                 ndvi_historical_means_directory = "path_to_output_directory",
#'                                 ndvi_historical_means_AWS = "path_to_AWS_means")
#'
#' @export
calculate_ndvi_historical_means <- function(ndvi_transformed,
                                            ndvi_historical_means_directory,
                                            basename_template = "ndvi_historical_mean_doy_{i}.parquet",
                                            overwrite = FALSE,
                                            ndvi_transformed_directory = NULL,
                                            dates_to_process = NULL,
                                            ...) {

  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

  # ndvi_data is opened lazily below — only when a DOY file actually needs to be
  # (re)computed. Opening it unconditionally would fail or give wrong results when the
  # pipeline is running in incremental monthly mode and ndvi_transformed only contains
  # the few recently-processed files. NULL here signals the dataset has not been opened yet.
  ndvi_data <- NULL

  # When dates_to_process is provided restrict to only the DOYs that appear in those
  # dates; this prevents opening the full NDVI dataset for DOYs that are irrelevant
  # to the current pipeline run and avoids producing corrupted means from incomplete
  # source data on a clean machine.
  doys_to_process <- if (!is.null(dates_to_process) && length(dates_to_process) > 0) {
    unique(lubridate::yday(as.Date(dates_to_process)))
  } else {
    1:366
  }

  # Fast because we can avoid collecting until write_parquet
  ndvi_historical_means <- map_vec(doys_to_process, .progress = TRUE, function(i) {

    filename <- file.path(ndvi_historical_means_directory, glue::glue(basename_template))

    # Check if glw files exist and can be read and that we don't want to overwrite them.
    if (!is.null(error_safe_read_parquet(filename)) && !isTRUE(overwrite)) {
      message(glue::glue("{filename} already exists and can be loaded, skipping"))
      return(filename)
    }

    # Open the full ndvi_transformed directory the first time a DOY file needs recomputing.
    # The explicit directory argument is preferred over inferring from the passed file-path
    # vector: in incremental monthly runs ndvi_transformed may be empty (all months already
    # exist so 0 branches are dispatched), making dirname() return character(0) and losing
    # the entire dataset. This matters especially when overwrite = TRUE forces a full
    # recompute from all available historical data.
    if (is.null(ndvi_data)) {
      ndvi_dir  <- if (!is.null(ndvi_transformed_directory)) ndvi_transformed_directory else
                     unique(dirname(ndvi_transformed[!is.na(ndvi_transformed)]))
      all_ndvi  <- if (length(ndvi_dir) > 0) list.files(ndvi_dir, pattern = "\\.parquet$", full.names = TRUE) else character(0)
      ndvi_data <<- arrow::open_dataset(all_ndvi)
    }

    ndvi_data |>
      filter(doy == i) |>
      # Round coordinates before grouping so that cells from different pipeline
      # runs (where continent_raster_template was recomputed and floating-point
      # accumulation shifted cell centres by ~1e-15) are treated as the same cell
      mutate(x = round(x, 7), y = round(y, 7)) |>
      group_by(x, y, doy) |>
      summarize(ndvi_sd = sd(ndvi, na.rm = TRUE),
                ndvi = mean(ndvi, na.rm = TRUE),
                .groups = "drop") |>
      filter(!is.na(ndvi_sd)) |> # Drop constant values (ndvi_sd == NA)
      arrow::write_parquet(filename, compression = "gzip", compression_level = 5)

      # Check plot
      # ggplot(ndvi_data, aes(x = x, y = y)) +
      #   geom_tile(aes(fill = ndvi), size = 5) +  # Points colored by NDVI
      #   scale_fill_viridis_c() +  # Gradient for NDVI values
      #   labs(
      #     title = glue::glue("Combined NDVI Historical Means, doy: {i}"),
      #     x = "Longitude",
      #     y = "Latitude",
      #     color = "NDVI"
      #   ) +
      #   theme_minimal()

    filename
  })

  ndvi_historical_means
}
