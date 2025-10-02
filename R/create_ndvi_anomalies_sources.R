#' Create NDVI Anomalies Sources Lookup
#'
#' Creates a tibble pairing each date with its corresponding MODIS and Sentinel NDVI files.
#' Uses non-equi joins to match dates with files based on the date ranges present in each file.
#' Files are matched by querying Arrow datasets for their min/max dates.
#'
#' @author Assistant and Nathan Layman
#'
#' @param modis_ndvi_transformed Character vector of MODIS NDVI transformed file paths
#' @param sentinel_ndvi_transformed Character vector of Sentinel NDVI transformed file paths
#' @param dates_to_process Vector of dates to process
#'
#' @return A tibble with columns: date, modis_file, sentinel_file
#'
#' @export
create_ndvi_anomalies_sources <- function(modis_ndvi_transformed,
                                          sentinel_ndvi_transformed,
                                          dates_to_process) {

  # Reusable function to build date range lookup
  # Opens all files as a single dataset for faster metadata reading
  # Uses basename matching to convert absolute paths back to relative paths
  build_date_range_lookup <- function(file_list, column_name) {
    # Create lookup table: basename → original relative path
    basename_lookup <- tibble::tibble(
      original_path = file_list,
      basename = basename(file_list)
    )

    arrow::open_dataset(file_list) |>
      dplyr::select(date) |>
      dplyr::mutate(!!column_name := arrow:::add_filename()) |>
      dplyr::group_by(!!sym(column_name)) |>
      dplyr::summarise(
        start_date = min(date),
        end_date = max(date)
      ) |>
      dplyr::collect() |>
      dplyr::mutate(basename = basename(!!sym(column_name))) |>
      dplyr::left_join(basename_lookup, by = "basename") |>
      dplyr::select(-basename, -!!sym(column_name)) |>
      dplyr::rename(!!column_name := original_path)
  }

  # Build lookup tables with date ranges
  modis_lookup <- build_date_range_lookup(modis_ndvi_transformed, "modis_file")
  sentinel_lookup <- build_date_range_lookup(sentinel_ndvi_transformed, "sentinel_file")

  # Non-equi join: match dates to files based on date ranges
  tibble::tibble(date = dates_to_process) |>
    dplyr::left_join(
      modis_lookup,
      dplyr::join_by(date >= start_date, date <= end_date)
    ) |>
    dplyr::left_join(
      sentinel_lookup,
      dplyr::join_by(date >= start_date, date <= end_date),
      suffix = c("_modis", "_sentinel")
    ) |>
    dplyr::select(date, modis_file, sentinel_file)
}
