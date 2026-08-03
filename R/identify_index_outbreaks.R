#' Identify index-case / outbreak chains
#'
#' Two WAHIS "outbreaks"are treated as part of the same underlying "outbreak chain" 
#' (an epidemiologically linked string of cases) when they fall within both a spatial 
#' and a temporal threshold of one another. Within each chain the earliest start_date 
#' is flagged as the index case (the most important case that a forecast can predict)
#'
#' @author Morgan Kain
#'
#' @param wahis_outbreaks Outbreak-level data; rvf_outbreaks target
#' @param space_threshold_km Two outbreaks in different countries farther apart than
#'   this are never in the same chain, regardless of timing.
#' @param time_threshold_days Two outbreaks in different countries farther apart in
#'   time than this are never in the same chain, regardless of distance. 
#' @param within_country_space_threshold_km Spatial threshold applied when two 
#'   outbreaks share a country. If NULL defaults to space_threshold_km. 
#' @param within_country_time_threshold_days Temporal threshold applied when two 
#'   outbreaks share a country. If NULL defaults to time_threshold_days. 
#' @param country_polygons Country-level sf polygons; the continent_polygon target 
#' @param crs_cea Equal-area CRS used to compute the spatial thresholds in meters,
#'   matching the projection used for hex neighbor adjacency elsewhere in the pipeline
#' @details Given the importance of capturing the first outbreak within a given country,
#'   two parameters are used for space and time to allow for a few more cases being
#'   labeled as index for economic reasons. Parameters chosen so that they link local
#'   spread but conservative enough to allow big jumps in space to be a labeled as a
#'   "new" outbreak even if those could reasonably be the same outbreak with local spread.
#' @return wahis_outbreaks with columns added: outbreak_chain_id (integer
#'   chain membership; NA for rows missing coordinates or a start date), index_case and
#'   country_index_case (1 for the earliest case in its chain, else 0, conditional on
#'   country or not)
#' @export
identify_index_outbreaks <- function(
    wahis_outbreaks
  , space_threshold_km                 = 250
  , time_threshold_days                = 60
  , within_country_space_threshold_km  = NULL
  , within_country_time_threshold_days = NULL
  , country_polygons                   = NULL
  , crs_cea                            = "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
) {

  stopifnot(all(c("start_date", "latitude", "longitude") %in% names(wahis_outbreaks)))

  ## NULL means "no special treatment": fall back to the cross-border threshold
  within_country_space_threshold_km  <- within_country_space_threshold_km  %||% space_threshold_km
  within_country_time_threshold_days <- within_country_time_threshold_days %||% time_threshold_days

  uses_country_specific_thresholds <- !identical(within_country_space_threshold_km, space_threshold_km) ||
    !identical(within_country_time_threshold_days, time_threshold_days)

  if (uses_country_specific_thresholds && is.null(country_polygons)) {
    stop("country_polygons must be supplied when within_country_* thresholds differ from the cross-border ones")
  }

  ## Keep only rows placeable in space and time; carry the original row order through
   ## so the chain id / index flag can be left-joined back onto the untouched input
  outbreaks <- wahis_outbreaks |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::filter(!is.na(longitude), !is.na(latitude), !is.na(start_date))

  ## Country membership has to be known *before* chains are built whenever it gates which
   ## threshold applies (as opposed to only being used afterwards for country_index_case)
  if (!is.null(country_polygons)) {
    outbreaks <- outbreaks |>
      dplyr::mutate(country_iso3c = assign_outbreak_countries(outbreaks, country_polygons))
  }

  ## Project to an equal-area CRS (matches the hex-neighbor adjacency built in
   ## build_neighbor_outbreak_history.R) so the spatial thresholds are true distances in meters
  pts_cea <- outbreaks |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    sf::st_transform(crs_cea)

  ## Space-and-time-filtered adjacency: an edge exists when two outbreaks are within
   ## both thresholds of one another, using the wider within-country thresholds when
   ## the pair shares a country and the cross-border ones otherwise
  adjacency <- build_chain_adjacency(
    outbreaks
  , pts_cea
  , space_threshold_km                 = space_threshold_km
  , time_threshold_days                = time_threshold_days
  , within_country_space_threshold_km  = within_country_space_threshold_km
  , within_country_time_threshold_days = within_country_time_threshold_days
  , has_country                        = !is.null(country_polygons))

  ## Connected components of the adjacency graph = outbreak chains
  chain_id <- igraph::components(igraph::graph_from_adj_list(adjacency, mode = "all"))$membership

  outbreaks <- outbreaks |>
    dplyr::mutate(outbreak_chain_id = chain_id) |>
    ## Deterministic tie-break: if two outbreaks in the same chain share a start_date,
     ## the one appearing first in the input wins the index-case flag
    dplyr::arrange(outbreak_chain_id, start_date, .row_id) |>
    dplyr::group_by(outbreak_chain_id) |>
    dplyr::mutate(index_case = as.integer(dplyr::row_number() == 1)) |>
    dplyr::ungroup()

  ## Country-level ("economic") index case: within an already-arranged chain, the first
   ## case to land in each country resets the clock for that country's own jurisdiction,
   ## independent of how far along the underlying epidemiological chain already is
  join_cols <- c(".row_id", "outbreak_chain_id", "index_case")

  if (!is.null(country_polygons)) {

    outbreaks <- outbreaks |>
      dplyr::group_by(outbreak_chain_id, country_iso3c) |>
      dplyr::mutate(country_index_case = as.integer(dplyr::row_number() == 1)) |>
      dplyr::ungroup()

    join_cols <- c(join_cols, "country_iso3c", "country_index_case")

  }

  result <- wahis_outbreaks |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::left_join(
      outbreaks |> dplyr::select(dplyr::all_of(join_cols))
    , by = ".row_id"
    ) |>
    dplyr::mutate(index_case = tidyr::replace_na(index_case, 0))

  if (!is.null(country_polygons)) {
    result <- result |> dplyr::mutate(country_index_case = tidyr::replace_na(country_index_case, 0))
  }

  result |> dplyr::select(-.row_id)

}


## Spatially join outbreak points to the country polygon their coordinates fall inside
assign_outbreak_countries <- function(outbreaks, country_polygons) {

  pts <- outbreaks |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  joined <- sf::st_join(pts, country_polygons |> dplyr::select(country_iso3c), left = TRUE)

  missing <- which(is.na(joined$country_iso3c))

  if (length(missing) > 0) {
    nearest <- sf::st_nearest_feature(pts[missing, ], country_polygons)
    joined$country_iso3c[missing] <- country_polygons$country_iso3c[nearest]
  }

  joined |> sf::st_drop_geometry() |> dplyr::pull(country_iso3c)

}

## Build a per-outbreak neighbor list restricted to pairs within the applicable
## space/time thresholds
build_chain_adjacency <- function(
    outbreaks
  , pts_cea
  , space_threshold_km
  , time_threshold_days
  , within_country_space_threshold_km
  , within_country_time_threshold_days
  , has_country
) {

  ## Cast the widest net first via the spatial index, then narrow with the exact
   ## per-pair distance and the applicable threshold below
  search_radius_km <- max(space_threshold_km, within_country_space_threshold_km)
  space_nb         <- sf::st_is_within_distance(pts_cea, dist = search_radius_km * 1000)

  purrr::imap(space_nb, function(neighbors, i) {

    neighbors <- neighbors[neighbors != i]
    if (length(neighbors) == 0) return(integer(0))

    day_gap <- abs(as.numeric(outbreaks$start_date[i] - outbreaks$start_date[neighbors]))
    dist_km <- as.numeric(sf::st_distance(pts_cea[i, ], pts_cea[neighbors, ])) / 1000

    same_country <- if (has_country) {
      outbreaks$country_iso3c[i] == outbreaks$country_iso3c[neighbors]
    } else {
      rep(FALSE, length(neighbors))
    }

    space_cutoff <- ifelse(same_country, within_country_space_threshold_km, space_threshold_km)
    time_cutoff  <- ifelse(same_country, within_country_time_threshold_days, time_threshold_days)

    neighbors[dist_km <= space_cutoff & day_gap <= time_cutoff]

  })

}
