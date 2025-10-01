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

  # Build MODIS lookup table with date ranges
  modis_lookup <- purrr::map_dfr(modis_ndvi_transformed, ~{
    ds <- arrow::open_dataset(.x)
    dates <- ds |>
      dplyr::select(date) |>
      dplyr::summarise(
        start_date = min(date),
        end_date = max(date)
      ) |>
      dplyr::collect()

    tibble::tibble(
      modis_file = .x,
      start_date = dates$start_date,
      end_date = dates$end_date
    )
  })

  # Build Sentinel lookup table with date ranges
  sentinel_lookup <- purrr::map_dfr(sentinel_ndvi_transformed, ~{
    ds <- arrow::open_dataset(.x)
    dates <- ds |>
      dplyr::select(date) |>
      dplyr::summarise(
        start_date = min(date),
        end_date = max(date)
      ) |>
      dplyr::collect()

    tibble::tibble(
      sentinel_file = .x,
      start_date = dates$start_date,
      end_date = dates$end_date
    )
  })

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
