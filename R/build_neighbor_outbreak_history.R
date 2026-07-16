#' Build the neighbor observed-outbreak history term
#'
#' For every hex and every model date calculate a set of lagged covariates measuring recent 
#' observed outbreak activity in neighboring hexes. For each backward window (matching the 
#' near-lag windows used elsewhere in the pipeline) the term is the sum over neighboring hexes of 
#' a per-outbreak weight (log10(cases + 1) by default, so that larger outbreaks exert more spread 
#' pressure). 
#'
#' @author Morgan Kain
#'
#' @param wahis_outbreaks Cleaned outbreak point data (the rvf_outbreaks target)
#' @param region_map The region map list whose first element is the hex sf object (with shapeName)
#' @param path_to_region_neighbors Filepath to the saved spdep-style neighbor list (nb) built from region_map
#' @param dates_to_process Model dates for which to compute the term (joined_dates$dates).
#' @param lags Integer vector of window ceilings in days. Defaults to c(30, 60, 90)
#' @param case_weight Logical; if TRUE weight each neighbor outbreak by log10(cases + 1), else 1.
#' @param local_folder Directory in which to save the output. Created if it does not exist.
#' @param save_filename Base filename for the output Parquet.
#' @param overwrite Logical; if TRUE recompute even when a covering file already exists.
#'
#' @return A string containing the filepath to the saved Parquet file.
#'
#' @export
build_neighbor_outbreak_history <- function(
    wahis_outbreaks
  , region_map
  , path_to_region_neighbors
  , dates_to_process
  , lags          = c(30, 60, 90)
  , case_weight   = TRUE
  , local_folder  = "data/neighbor_outbreak_history"
  , save_filename = "neighbor_outbreak_history"
  , overwrite     = FALSE) {

  ## Create the output directory and assemble the full save path
  dir.create(local_folder, recursive = TRUE, showWarnings = FALSE)
  save_filename <- paste0(local_folder, "/", save_filename, ".parquet")

  ## Sorted and de-duplicate dates to make the coverage check below exact
  dates_to_process <- sort(unique(as.Date(dates_to_process)))

  ## Reuse an existing file only when it already covers every requested date
   ## Important because dates are appended incrementally with each monthly forecast run
  error_safe_read_parquet <- purrr::possibly(arrow::read_parquet, NULL)
  existing_dataset        <- error_safe_read_parquet(save_filename)

  if (!is.null(existing_dataset) && !overwrite) {
    if (all(dates_to_process %in% as.Date(existing_dataset$date))) return(save_filename)
  }

  ## Pull the hex sf (with shapeName) out of the region_map list
  hexes <- region_map[[1]]

  ## Neighbor adjacency
  adjacency <- build_hex_adjacency(hexes, path_to_region_neighbors)

  ## Assign each observed outbreak to the hex it falls in, keeping its start_date and a weight
  outbreaks_hex <- assign_outbreaks_to_hexes(wahis_outbreaks, hexes, case_weight = case_weight)

  ## Attribute every source-hex outbreak to each focal hex that neighbors it, then collapse to a
   ## per (focal hex, date) daily spread-pressure weight; the focal hex is never its own neighbor
  focal_activity <- adjacency |>
    dplyr::inner_join(outbreaks_hex, by = c("neighbor" = "source")) |>
    dplyr::group_by(focal, start_date) |>
    dplyr::summarize(w = sum(weight, na.rm = TRUE), .groups = "drop")

  ## Column ceilings and floors for each non-overlapping backward window
  lag_lo <- c(0, head(lags, -1))

  ## For every model date, sum the neighbor activity falling in each backward window per focal hex
  neighbor_history <- purrr::map_dfr(dates_to_process, function(model_date) {

    ## One tibble of per-hex sums for each lag window, later joined together on the focal hex
    per_lag <- purrr::map(seq_along(lags), function(k) {

      window_start <- model_date - lags[k]
      window_end   <- model_date - lag_lo[k]

      focal_activity |>
        dplyr::filter(start_date > window_start, start_date <= window_end) |>
        dplyr::group_by(focal) |>
        dplyr::summarize(!!paste0("neigh_outbreak_wt_", lags[k]) := sum(w, na.rm = TRUE), .groups = "drop")

    })

    ## Combine the windows; a hex missing from a window simply had no neighbor activity there (0)
    combined <- purrr::reduce(per_lag, dplyr::full_join, by = "focal")

    if (nrow(combined) == 0) return(NULL)

    combined |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("neigh_outbreak_wt_"), ~ tidyr::replace_na(.x, 0))) |>
      dplyr::mutate(date = model_date) |>
      dplyr::rename(shapeName = focal)

  })

  arrow::write_parquet(neighbor_history, save_filename, compression = "gzip", compression_level = 5)

  save_filename

}


## Assign outbreak points to hexes and attach a per-outbreak spread-pressure weight
assign_outbreaks_to_hexes <- function(wahis_outbreaks, hexes, case_weight = TRUE) {

  ## Drop outbreaks without coordinates before the spatial join
  pts <- wahis_outbreaks |>
    dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(hexes), remove = FALSE)

  ## Spatial join to recover the containing hex; left = FALSE drops points outside the hex grid
  joined <- sf::st_join(pts, hexes |> dplyr::select(shapeName), left = FALSE) |>
    sf::st_drop_geometry()

  ## Median observed weight, used to impute outbreaks with unknown case counts (~1% of records).
   ## Imputing on the log scale gives a "typical" outbreak and avoids the heavy right tail of raw
   ## counts inflating the value
  med_weight <- median(log10(joined$cases + 1), na.rm = TRUE)

  ## One row per outbreak: its source hex, start date, and weight. Known counts use log10(cases + 1)
   ## so larger outbreaks exert more spread pressure; NA counts fall back to the median weight
  joined |>
    dplyr::transmute(
      source     = shapeName
    , start_date = as.Date(start_date)
    , weight     = if (case_weight) dplyr::coalesce(log10(cases + 1), med_weight) else 1
    ) |>
    dplyr::filter(!is.na(source), !is.na(start_date))

}


## Build a tidy (focal shapeName, neighbor shapeName) adjacency from a saved neighbor list, or
## rebuild it from the hex geometry when the saved list is absent or does not line up with the map
build_hex_adjacency <- function(hexes, path_to_region_neighbors) {

  nb <- NULL

  ## Prefer the saved spdep-style neighbor list; it is row-indexed to the map used to build it,
   ## so it is only valid when its length matches the current number of hexes
  if (!is.null(path_to_region_neighbors) && file.exists(path_to_region_neighbors)) {
    nb_info <- readRDS(path_to_region_neighbors)
    if (length(nb_info$nb) == nrow(hexes)) nb <- nb_info$nb
  }

  ## Fall back to rebuilding neighbors (shared edges plus a small tolerance for micro-gaps) in an
   ## equal-area projection so the within-distance test is in meters; this guarantees alignment
  if (is.null(nb)) {
    cea <- "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
    nb  <- build_neighbors_with_tolerance(hexes |> sf::st_transform(cea), tol_m = 500)$nb
  }

  ## Expand the neighbor list into focal/neighbor shapeName pairs using row-index -> shapeName
  dplyr::tibble(
    focal    = rep(hexes$shapeName, lengths(nb))
  , neighbor = hexes$shapeName[unlist(nb)]
  ) |>
    dplyr::filter(focal != neighbor)

}
