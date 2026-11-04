#' Overlap between H3 hexes and larger regions (e.g., aggregated ADM2 regions)
#'
#' Computes, for every hex-region pair that overlaps, the share of the hex inside the region
#' and the share of the region covered by the hex. Overlaps are computed with planar GEOS in
#' the regions' equal-area CRS (reduced_pan_regions*.Rds files are stored in +proj=cea). 
#' A lon/lat copy of the regions is also returned for mapping: projecting the regions 
#' to lon/lat leaves small back-tracking spikes (left from boundary simplification) that 
#' s2 reads as crossing edges, so the lon/lat copy is rebuilt in s2 with crossing edges split.
#'
#' @title build_hex_region_overlap
#' @param regions Regions as an sf object in an equal-area CRS, or a path to an .Rds of one
#'   (e.g., data/upscaling_regions/reduced_pan_regions_target60000km2.Rds). Columns `country` and `region` are
#'   kept when present. An sf wrapped in a list and / or in lon/lat (e.g.,
#'   data/upscaling_regions/region_hexes_for_evaluation.Rds) is unwrapped and moved to +proj=cea
#' @param region_hexes Hex grid as an sf object with a shapeName column, or a list whose first
#'   element is one (as in the region_hexes target)
#' @param map_simplify_m Tolerance (metres, in the regions' equal-area CRS) for simplifying the
#'   lon/lat display copy (regions_ll); overlaps always use the full-detail regions
#' @return List with
#'   - regions_ea: regions in their equal-area CRS with area_index (row number), adm_km2,
#'     n_hexes and hex_equiv (region size in hex equivalents)
#'   - regions_ll: the same regions in lon/lat (s2-valid), with simplified boundaries, for maps
#'   - overlap: one row per hex-region piece with area_index, shapeName, overlap_km2,
#'     hex_frac (share of the hex inside the region), adm_frac (share of the region covered
#'     by the hex) and majority_region (the region holding the largest share of the hex)
#' @author Morgan Kain
#' @export
build_hex_region_overlap <- function(regions, region_hexes, map_simplify_m = 2000) {

  ## Accept a path to the saved regions or the sf object itself
  if (is.character(regions)) regions <- readRDS(regions)

  ## Regions saved like the hex targets (sf wrapped in a list, in lon/lat; e.g., the larger
   ## evaluation hexes) are unwrapped and moved to an equal-area CRS
  if (!inherits(regions, "sf")) regions <- regions[[1]]
  if (isTRUE(sf::st_is_longlat(regions))) regions <- sf::st_transform(regions, "+proj=cea +datum=WGS84 +units=m")

  ## region_hexes / region_map targets wrap the sf object in a list
  if (!inherits(region_hexes, "sf")) region_hexes <- region_hexes[[1]]

  ## Stable region id (row number) and area, in the regions' equal-area CRS
  regions_ea         <- regions |> dplyr::mutate(area_index = dplyr::row_number(), .before = 1)
  regions_ea$adm_km2 <- as.numeric(sf::st_area(regions_ea)) / 1e6

  ## Hexes moved into the same CRS, with their areas
  hexes_ea <- region_hexes |>
    dplyr::select(shapeName) |>
    sf::st_transform(sf::st_crs(regions_ea))
  
  hexes_ea$hex_km2 <- as.numeric(sf::st_area(hexes_ea)) / 1e6

  ## Planar intersection pieces and their areas
  pieces <- sf::st_intersection(
    hexes_ea   |> sf::st_set_agr("constant")
  , regions_ea |> dplyr::select(area_index, adm_km2) |> sf::st_set_agr("constant")
  )
  
  pieces$overlap_km2 <- as.numeric(sf::st_area(pieces)) / 1e6

  ## Determine overlap in the aggregated ADM2 regions and the raw H3 hexes
  overlap <- pieces |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::filter(overlap_km2 > 0) |>
    dplyr::mutate(
      hex_frac = overlap_km2 / hex_km2
    , adm_frac = overlap_km2 / adm_km2
    ) |>
    dplyr::mutate(majority_region = hex_frac == max(hex_frac), .by = shapeName) |>
    dplyr::select(area_index, shapeName, overlap_km2, hex_frac, adm_frac, majority_region) |>
    dplyr::arrange(area_index, dplyr::desc(hex_frac))

  ## Region size in hexes touched and in hex equivalents
  regions_ea <- regions_ea |>
    dplyr::left_join(
      overlap |> dplyr::summarize(n_hexes = dplyr::n(), hex_equiv = sum(hex_frac), .by = area_index)
    , by = "area_index")

  list(
    regions_ea = regions_ea
    ## Display copy for maps: boundaries simplified before the
     ## lon/lat rebuild; overlaps above use the full-detail regions
  , regions_ll = regions_ea |>
      sf::st_simplify(dTolerance = map_simplify_m, preserveTopology = TRUE) |>
      s2_safe_lonlat()
  , overlap    = overlap
  )

}

## Lon/lat copy of polygons rebuilt in s2 with crossing edges split
s2_safe_lonlat <- function(x) {

  x_ll <- sf::st_transform(x, 4326)

  sf::st_set_geometry(
    x_ll
  , sf::st_geometry(x_ll) |>
      sf::st_as_binary() |>
      s2::s2_geog_from_wkb(check = FALSE) |>
      s2::s2_rebuild(options = s2::s2_options(split_crossing_edges = TRUE, validate = FALSE)) |>
      sf::st_as_sfc() |>
      sf::st_set_crs(4326)
  )

}
