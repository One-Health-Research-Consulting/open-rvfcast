#' Create NDVI Transformed Sources Lookup
#'
#' Creates a tibble pairing each month with its relevant MODIS and Sentinel NDVI files.
#' Files are matched by extracting actual year-month values from the data.
#'
#' @author Assistant and Nathan Layman
#'
#' @param modis_ndvi_transformed Character vector of MODIS NDVI file paths
#' @param sentinel_ndvi_transformed Character vector of Sentinel NDVI file paths
#' @param months_to_process Character vector of months in "YYYY-MM" format
#'
#' @return A tibble with columns: month, modis_files (list), sentinel_files (list)
#'
#' @export
create_ndvi_transformed_sources <- function(modis_ndvi_transformed,
                                            sentinel_ndvi_transformed,
                                            months_to_process) {

  # Reusable function to summarise files by year-month
  summarise_file <- function(file_list) {
    purrr::map_dfr(file_list, function(path) {
      arrow::open_dataset(path) |>
        dplyr::mutate(
          year  = lubridate::year(date),
          month = lubridate::month(date)
        ) |>
        dplyr::distinct(year, month) |>
        dplyr::collect() |>
        dplyr::mutate(file = path)
    }) |>
      dplyr::group_by(year, month) |>
      dplyr::summarise(
        files = list(file),
        .groups = "drop"
      )
  }

  # Summarize MODIS and Sentinel
  modis_lookup <- summarise_file(modis_ndvi_transformed) |>
    dplyr::rename(modis_files = files)

  sentinel_lookup <- summarise_file(sentinel_ndvi_transformed) |>
    dplyr::rename(sentinel_files = files)

  # Full join so we keep rows present in either source
  combined_lookup <- dplyr::full_join(modis_lookup, sentinel_lookup, by = c("year", "month"))

  # Create month column in YYYY-MM format and join with months_to_process
  combined_lookup <- combined_lookup |>
    dplyr::mutate(month = sprintf("%04d-%02d", year, month)) |>
    dplyr::select(month, modis_files, sentinel_files)

  # Filter to only requested months
  tibble::tibble(month = months_to_process) |>
    dplyr::left_join(combined_lookup, by = "month")
}
