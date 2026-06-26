#' Retrieve Sentinel-3 10-day NDVI product metadata from Copernicus Data Space
#'
#' This function retrieves Sentinel-3 SYNERGY SY_2_V10 (10-day NDVI composite)
#' product metadata from the Copernicus Data Space OData API. It filters for
#' Africa-covering S3A Near-Real-Time products and returns date ranges and
#' product identifiers needed to drive the download pipeline.
#'
#' @author Emma Mendelsohn
#'
#' @param
#'
#' @return A tibble with columns: id (product UUID), name (product filename),
#'   start_date (Date), end_date (Date). Each row represents one 10-day
#'   composite period with non-overlapping date ranges.
#'
#' @note Previously used the Copernicus RESTO API, which was deprecated and
#'   returns 403 Forbidden as of 2026. Now uses the OData v1 API.
#'
#' @examples
#' #This function doesn't require any arguments.
#' get_sentinel_ndvi_api_parameters()
#'
#' @export
get_sentinel_ndvi_api_parameters <- function(sentinel_ndvi_transformed_directory = NULL,
                                              basename_template = "transformed_sentinel_NDVI_{start_date}_to_{end_date}.parquet",
                                              ...) {

  # Query Africa S3A Near-Real-Time 10-day NDVI composites via OData API
  # The RESTO API previously used (catalogue.dataspace.copernicus.eu/resto/api/)
  # was deprecated and now returns 403; OData v1 is the current replacement
  resp <- httr::GET(
    "https://catalogue.dataspace.copernicus.eu/odata/v1/Products",
    query = list(
      `$filter`  = paste0(
        "Collection/Name eq 'SENTINEL-3'",
        " and Attributes/OData.CSC.StringAttribute/any(att:att/Name eq 'productType'",
        " and att/OData.CSC.StringAttribute/Value eq 'SY_2_V10___')",
        " and contains(Name, 'AFRICA')",
        " and startswith(Name, 'S3A')",
        " and contains(Name, '_NT_')"
      ),
      `$top`     = 1000,
      `$orderby` = "ContentDate/Start asc"
    )
  )

  raw <- jsonlite::fromJSON(rawToChar(resp$content))$value

  # Compute non-overlapping date ranges: each period ends one day before the
  # next starts. Sentinel data uses inclusive start and end dates that share
  # a boundary, so we shift the end back by one day to avoid overlap.
  start_dates <- lubridate::floor_date(lubridate::as_date(raw$ContentDate$Start), unit = "day")
  end_dates <- c(
    start_dates[-1] - 1,
    lubridate::floor_date(lubridate::as_date(tail(raw$ContentDate$End, 1)), unit = "day")
  )

  out <- tibble::tibble(
    id         = raw$Id,
    name       = raw$Name,
    start_date = start_dates,
    end_date   = end_dates
  )

  # Filter to only products where the output parquet does not yet exist. Sentinel
  # products are historical observations that do not change once published, so
  # there is no need to re-run a branch whose parquet is already present. This
  # prevents sentinel_ndvi_transformed from dispatching hundreds of branches on
  # every pipeline run when only a few new products have appeared since last time.
  if (!is.null(sentinel_ndvi_transformed_directory)) {
    out <- out |>
      mutate(output_file = file.path(sentinel_ndvi_transformed_directory,
                                     glue::glue_data(pick(everything()), basename_template))) |>
      filter(!file.exists(output_file)) |>
      select(-output_file)
  }

  # targets cannot branch over a 0-row tibble. When all sentinel parquets already
  # exist the filter above returns nothing; return one NA sentinel row so the
  # pattern = map() branch dispatches exactly once and transform_sentinel_ndvi
  # can short-circuit immediately without downloading anything.
  if (nrow(out) == 0) {
    return(tibble::tibble(
      id         = NA_character_,
      name       = NA_character_,
      start_date = as.Date(NA),
      end_date   = as.Date(NA)
    ))
  }

  return(out)

}
