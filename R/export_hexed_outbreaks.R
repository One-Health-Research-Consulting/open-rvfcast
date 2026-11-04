#' Place outbreaks in hexes exactly as the response labels do, and save them
#'
#' Run in the rvf_data_processing_targets pipeline. Saves the indexed outbreaks with the
#' hex each one is labelled in, so the model_framework_targets pipeline can link
#' predictions to outbreaks (link_predictions_to_outbreaks()) without reading the rvf store.
#'
#' Hex placement reproduces the label construction:
#'  1) get_rvf_response() snaps each outbreak to the centre of its wahis raster cell
#'  2) lag_join_aggregate() moves that point to the nearest raster cell that has covariate
#'     data (cells along coasts and data gaps can be missing) and uses that cell's hex
#'  3) a hex-window is labelled an outbreak only when its summed cases are > 0, so
#'     outbreaks reported with 0 or missing cases never create a positive label
#'
#' @title export_hexed_outbreaks
#' @param wahis_outbreaks_indexed Outbreaks with chain and index-case flags; the
#'   rvf_outbreaks_indexed target
#' @param wahis_raster_template The wahis_raster_template target (packed or unpacked)
#' @param cell_hex_lookup Raster cells that have covariate data and the hex each belongs
#'   to: a tibble with x, y and shapeName, or the path to any one of the per-date
#'   region_data parquet files (every file holds the full cell-to-hex mapping)
#' @param save_filename Where to save the hexed outbreaks (.Rds)
#' @return save_filename. The saved tibble has one row per outbreak with shapeName, outbreak_id,
#'   start_date, cases, counts_toward_label (cases > 0), country_iso3c, when present
#'   outbreak_chain_id, index_case and country_index_case, and the raw longitude and latitude
#' @author Morgan Kain
#' @export
export_hexed_outbreaks <- function(wahis_outbreaks_indexed, wahis_raster_template
                                   , cell_hex_lookup, save_filename) {

  ## Targets store the template packed; unpack it if needed
  if (inherits(wahis_raster_template, "PackedSpatRaster")) {
    wahis_raster_template <- terra::unwrap(wahis_raster_template)
  }

  ## Covariate cells and their hexes, rounded as in lag_join_aggregate()
  cell_hex_lookup <- read_cell_hex_lookup(cell_hex_lookup)

  outbreaks <- wahis_outbreaks_indexed |>
    dplyr::filter(!is.na(longitude), !is.na(latitude), !is.na(start_date)) |>
    ## Step 1 (get_rvf_response): snap to the centre of the wahis raster cell
    dplyr::mutate(
      cell   = terra::cellFromXY(wahis_raster_template, cbind(longitude, latitude))
    , x_cell = round(terra::xFromCell(wahis_raster_template, cell), 4)
    , y_cell = round(terra::yFromCell(wahis_raster_template, cell), 4)
    )

  ## Step 2 (lag_join_aggregate): move to the nearest cell that has covariate data, in
   ## the same planar x/y space (no CRS) that lag_join_aggregate() uses
  nearest_cell <- sf::st_nearest_feature(
    sf::st_as_sf(outbreaks, coords = c("x_cell", "y_cell"))
  , sf::st_as_sf(cell_hex_lookup, coords = c("x", "y"))
  )

  hexed_outbreaks <- outbreaks |>
    dplyr::mutate(
      shapeName           = cell_hex_lookup$shapeName[nearest_cell]
      ## Step 3 (lag_join_aggregate): only outbreaks with cases > 0 can create a label
    , counts_toward_label = as.numeric(dplyr::coalesce(as.numeric(cases), 0) > 0)
    ) |>
    dplyr::select(
      shapeName, outbreak_id, start_date, cases, counts_toward_label, country_iso3c
    , dplyr::any_of(c("outbreak_chain_id", "index_case", "country_index_case"))
      ## Raw (unsnapped) coordinates, for placing outbreaks in areas other than hexes
    , longitude, latitude
    ) |>
    dplyr::arrange(shapeName, start_date)

  saveRDS(hexed_outbreaks, save_filename)

  save_filename

}


## Read the covariate cell-to-hex mapping (tibble or one region_data parquet file)
read_cell_hex_lookup <- function(cell_hex_lookup) {

  ## Any one region_data file holds every covariate cell with its hex
  if (is.character(cell_hex_lookup)) {
    cell_hex_lookup <- arrow::read_parquet(cell_hex_lookup, col_select = c("x", "y", "shapeName"))
  }

  cell_hex_lookup |>
    dplyr::transmute(x = round(x, 4), y = round(y, 4), shapeName) |>
    dplyr::distinct()

}
