#' Build a hexagonal grid over the whole study area using either H3 or a simple custom function that
#' simply splits up land into hexagon shapes. There are lots of benefits of H3 (robustness, code)
#' efficiency, established regions where results could be shared with other projects that also use H3.
#' However, the spatial extent options are rather restrictive with H3 and may not be suitable. If this
#' is the case, the custom function is fine, but is specific to this project and doesn't have all of the
#' code architecture support of H3
#'
#'
#' @title make_hex_grid_h3

#' @param template_rast template over study area to cover
#' @param target_area_km2 hopefully area of hexagons. H3 has set sizes, so this is rather restrictive
#' @param h3_res h3jsr numeric size value that if used will supersede target_area_km2
#' @return sf "MULTIPOLYGON" with a hex_id column
#' @author Morgan Kain
#' @export

make_hex_grid_h3     <- function(template_rast, target_area_km2 = 12000, h3_res = NULL) {

  ## Get land extent from template raster
  r <- if (nlyr(template_rast) > 1) template_rast[[1]] else template_rast

  ## Logical mask of non-NA cells
  land_mask <- !is.na(r)

  ## Extract the coordinates to determine hexagons we need when this is all said and done
  xy_coords <- crds(template_rast) |> as.data.frame()

  ## Get an approximate data footprint / dissolve mask to a single land polygon
  land_polys <- as.polygons(land_mask, dissolve = TRUE)

  ## Convert to sf and ensure lon/lat for building H3 hexagons
  land_sf <- st_as_sf(land_polys) |> st_make_valid() |> st_transform(., 4326)

  ## terra to sf conversion can create some junk, just need geometry and ID column
  land_sf <- land_sf |> mutate(land_id = 1L) |> select(land_id, geometry)

  ## Choose H3 resolution; H3 takes in specific numeric values. From the H3
   ## documentation here are the areas for these values from 0 to 10
  h3_area_table <- tibble(
    res = 0:10
  , area_km2 = c(
      4250546.847, 607220.978, 86745.854, 12392.264, 1770.323,
      252.903, 36.129, 5.161, 0.737, 0.105, 0.015
    )
  )

  ## Figure out the resolution that is closest to what is desired
  if (!is.null(h3_res)) {
    res_chosen <- h3_res
  } else {
    res_chosen <- h3_area_table |>
      mutate(diff = abs(area_km2 - target_area_km2)) |>
      arrange(diff) |>
      slice(1) |>
      pull(res)
  }

  ## Print to the user how good / bad the available resolution is
  message("Using H3 resolution ", res_chosen, " (approx area ",
          round(h3_area_table$area_km2[h3_area_table$res == res_chosen], 2),
          " km^2 per hex)")

  ## Use H3 polyfill to get hex IDs covering land_sf
   ## h3jsr::polyfill returns a vector of hex indexes that intersect the polygon
  hex_ids <- h3jsr::polygon_to_cells(st_geometry(land_sf), res = res_chosen)

  ## Convert hex IDs to polygons
  hex_sfc <- h3jsr::cell_to_polygon(hex_ids, simple = TRUE)

  ## Create sf object with hex_id and geometry and ensure MULTIPOLYGON geometry type
  hex_sf <- st_sf(
    hex_id   = hex_ids |> unlist()
  , geometry = hex_sfc
  , crs      = 4326
  ) |>
  st_cast("MULTIPOLYGON")

  ## Filter out the hexagons that we dont need
  points_in_hexes <- st_join(
    xy_coords |> st_as_sf(coords = c("x", "y"), crs = 4326)
  , hex_sf
  , join = st_within) |>
    as.data.frame() |>
    bind_cols(xy_coords)

  needed_hex <- points_in_hexes |> pull(hex_id) |> unique()

  clipped_reduced <- hex_sf |>
  filter(hex_id %in% needed_hex) |>
    rename(shapeName = hex_id) |>
    list()

  saveRDS(clipped_reduced, "data/region_hexes.Rds")

  clipped_reduced

}
make_hex_grid_custom <- function(template_rast, target_area_km2) {

  ## Extract & validate raster CRS
  r_crs <- terra::crs(template_rast, proj = TRUE)

  ## Convert raster extent → sf boundary
  boundary_sf <- template_rast |>
    terra::ext() |>
    as.polygons() |>
    st_as_sf() |>
    st_set_crs(r_crs) |>
    st_make_valid()

  ## Reproject boundary to equal-area for hex construction
  boundary_aea <- st_transform(boundary_sf, "ESRI:102022")

  ## Extract the coordinates to determine hexagons we need when this is all said and done
  xy_coords <- crds(template_rast) |> as.data.frame()

  ## hexagon geometry formulas
  side_len <- sqrt((2 * target_area_km2 * 1e6) / (3 * sqrt(3)))
  width    <- 2 * side_len

  ## Show user what they are creating
  message("Hexagon width (m): ", round(width))

  ## Build grid ----
  hex_grid <- st_make_grid(
    boundary_aea
  , cellsize = width
  , what     = "polygons"
    ## Not squares
  , square   = FALSE
  )

  hex_sf <- st_sf(
    hex_id   = seq_along(hex_grid)
  , geometry = hex_grid
  )

  ## Clip to Africa boundary and convert to MULTIPOLYGON
  clipped <- st_intersection(hex_sf, boundary_aea) |> st_cast("MULTIPOLYGON")

  ## Filter out the hexagons that we dont need
  points_in_hexes <- st_join(
    xy_coords |>
    st_as_sf(coords = c("x", "y"), crs = 4326) |>
      st_transform(st_crs(clipped))
    , clipped
    , join = st_within) |>
    as.data.frame() |>
    bind_cols(xy_coords)

  needed_hex <- points_in_hexes |> pull(hex_id) |> unique()

  clipped_reduced <- clipped |>
    filter(hex_id %in% needed_hex) |>
    rename(shapeName = hex_id) |>
    list()

  saveRDS(clipped_reduced, "data/region_hexes.Rds")

  clipped_reduced

}
