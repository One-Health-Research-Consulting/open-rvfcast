#' Create NDVI Transformed Sources Lookup
#'
#' Creates a tibble pairing each month with its relevant MODIS and Sentinel NDVI files.
#' Files are matched to months using the date span encoded in each filename.
#'
#' @author Assistant and Nathan Layman
#'
#' @param modis_ndvi_transformed Character vector of MODIS NDVI file paths
#' @param sentinel_ndvi_transformed Character vector of Sentinel NDVI file paths
#' @param months_to_process Character vector of months in "YYYY-MM" format
#' @param modis_ndvi_transformed_directory Directory holding all MODIS NDVI parquets
#' @param sentinel_ndvi_transformed_directory Directory holding all Sentinel NDVI parquets
#'
#' @return A tibble with columns: month, modis_files (list), sentinel_files (list)
#'
#' @note The upstream MODIS and Sentinel targets only return the files produced by
#'   their branches in the current run (or a single placeholder file when nothing new
#'   was needed), so relying on them alone leaves months with no source files. All
#'   parquets present in the source directories are therefore considered, and the
#'   target values are kept as inputs only to enforce the dependency.
#'
#' @export
create_ndvi_transformed_sources <- function(modis_ndvi_transformed,
                                            sentinel_ndvi_transformed,
                                            months_to_process,
                                            modis_ndvi_transformed_directory,
                                            sentinel_ndvi_transformed_directory) {

  # Every parquet on disk (synced from S3 or freshly built) plus anything the targets returned
  modis_files <- unique(c(
    list.files(modis_ndvi_transformed_directory, pattern = "^transformed_modis_NDVI_.*\\.parquet$", full.names = TRUE)
  , modis_ndvi_transformed[!is.na(modis_ndvi_transformed)]
  ))
  sentinel_files <- unique(c(
    list.files(sentinel_ndvi_transformed_directory, pattern = "^transformed_sentinel_NDVI_.*\\.parquet$", full.names = TRUE)
  , sentinel_ndvi_transformed[!is.na(sentinel_ndvi_transformed)]
  ))

  # Date span covered by each file, parsed from its name
  modis_spans    <- get_ndvi_file_spans(modis_files, source = "modis")
  sentinel_spans <- get_ndvi_file_spans(sentinel_files, source = "sentinel")

  # For each requested month keep the files whose span overlaps that month
  tibble::tibble(month = months_to_process) |>
    dplyr::mutate(
      month_start    = as.Date(paste0(month, "-01"))
    , month_end      = lubridate::ceiling_date(month_start, "month") - 1
    , modis_files    = purrr::map2(month_start, month_end, files_overlapping_span, spans = modis_spans)
    , sentinel_files = purrr::map2(month_start, month_end, files_overlapping_span, spans = sentinel_spans)
    ) |>
    dplyr::select(month, modis_files, sentinel_files)
}

#' Parse the date span covered by MODIS or Sentinel NDVI parquet files from their names
#'
#' @param files Character vector of file paths
#' @param source Either "modis" (transformed_modis_NDVI_{start}.parquet, 16-day
#'   composites) or "sentinel" (transformed_sentinel_NDVI_{start}_to_{end}.parquet)
#'
#' @return A tibble with columns file, start, end
#' @noRd
get_ndvi_file_spans <- function(files, source = c("modis", "sentinel")) {

  source <- match.arg(source)

  # Pull every ISO date out of each basename
  file_dates <- regmatches(basename(files), gregexpr("\\d{4}-\\d{2}-\\d{2}", basename(files)))

  spans <- tibble::tibble(
    file  = files
  , start = as.Date(purrr::map_chr(file_dates, ~ if (length(.x) >= 1) .x[1] else NA_character_))
  , end   = if (source == "sentinel") {
      as.Date(purrr::map_chr(file_dates, ~ if (length(.x) >= 2) .x[2] else NA_character_))
    } else {
      # MODIS composites span 16 days from their start date (same rule as get_needed_s3_filenames)
      as.Date(purrr::map_chr(file_dates, ~ if (length(.x) >= 1) .x[1] else NA_character_)) + 15L
    }
  )

  # Drop anything whose name could not be parsed
  dplyr::filter(spans, !is.na(start), !is.na(end))
}

#' Select files whose date span overlaps a given window
#'
#' @param start Window start date
#' @param end Window end date
#' @param spans Tibble from get_ndvi_file_spans()
#'
#' @return Character vector of file paths, or NULL when none overlap
#' @noRd
files_overlapping_span <- function(start, end, spans) {
  hits <- spans$file[spans$start <= end & spans$end >= start]
  # NULL keeps the "no files" convention transform_ndvi already handles
  if (length(hits) == 0) NULL else hits
}
