#' Two functions to mask data to a sub-region of Africa (e.g., country) and then summarize covariates
#' into sub-sub regions of the sub-region (e.g., district). Because all dates use the same x, y coordinates
#' can just figure out ADM regions for one date and join this with the other dates
#'
#'
#' @title mask_and_cluster_build_template

#' @param cov_files A single covariate parquet file
#' @param hexes_sf sf / dataframe object of sub-sub regions
#' @param countries_sf sf / dataframe object of sub-sub regions
#' @param district_id_col name of the identifier column for the sub-sub region
#' @param out_dir directory to save output
#' @return Single tibble of the ADM identifiers for all x, y coordinates
#' @author Morgan Kain
#' @export

mask_and_cluster_build_template <- function(
    cov_files
  , hex_sf
  , countries_sf
  , district_id_col = "shapeName"
  , out_dir
) {

  ## If is.na(cov_files) it means there is no new data so propagate NA
  
  
  ## Check that we're only working on one date at a time
  stopifnot(length(cov_files) == 1)

  ## Read one parquet file
  df <- read_parquet(cov_files) |>
    filter(forecast_interval == 30) |>
    dplyr::select(-forecast_interval) |>
    mutate(index = seq_len(n()), .before = 1)

  ## NOTE: Quite possibly a better way to do this, but this is what we are doing for now...

  ## ** First for country and ADM2 region ------------------------------------------------

  ## Ensure districts are in projected CRS (ensure valid polygons)
  countries_sf <- lapply(countries_sf, FUN = function(x) {
    sf::st_make_valid(x) |> st_collection_extract("POLYGON")
  })

  crs_target   <- lapply(countries_sf, FUN = function(x) sf::st_crs(x))

  ## Extract all of the identifier names for the sub-sub regions, retaining just the name column
  all_identifiers <- lapply(countries_sf, FUN = function(x) {
    district_identifiers <- names(x)
    district_name_col   <- district_identifiers[grepl("Name", district_identifiers)]
    district_identifiers <- district_identifiers[!grepl("Name", district_identifiers)]
    list(district_name_col, district_identifiers)
  })

  names(all_identifiers) <- lapply(countries_sf, FUN = function(x) x$shapeGroup[1])

  df_sorted <- purrr::pmap(list(crs_target, countries_sf, all_identifiers, names(all_identifiers)), .f = function(x, y, z, q) {
    ## Convert to sf
    pts_sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = x)

    old_s2 <- sf_use_s2(TRUE)

    ## Spatial join: keep only points in the full set of sub-sub regions
    joined <- try({
        sf::st_join(pts_sf, y, left = FALSE)
    }, silent = TRUE)

    if (class(joined)[1] == "try-error") {
      old_s2 <- sf_use_s2(FALSE)
      joined <- try({
        sf::st_join(pts_sf, y, left = FALSE)
      }, silent = TRUE)
    }

    ## Extract coordinates back into x and y columns
    ## (mostly for plotting and debugging purposes)
    coords <- sf::st_coordinates(joined)
    joined <- joined |>
      mutate(x = coords[, 1], y = coords[, 2], .before = 1) |>
      as_tibble() |>
      dplyr::select(-all_of(z[[2]])) |>
      relocate(!!z[[1]], .after = "y") |>
      mutate(Country = q, .before = shapeName) |>
      relocate(index, .before = 1)

  }) |>
  bind_rows()

  ## Country boundary layers are digitized independently per-country and often overlap slightly
   ## at shared borders (and, e.g., Western Sahara vs Morocco, overlap substantially where two
   ## layers both claim the same disputed territory). st_join above therefore sometimes matches a
   ## single point to more than one country/ADM2, which would otherwise duplicate that point
   ## downstream every time the template is joined back onto covariate data. Keep a single,
   ## deterministic match per point: countries_sf/hex_sf are processed in a fixed order, so the
   ## first match for a given index is reproducible across runs.
  df_sorted <- df_sorted |> distinct(index, .keep_all = TRUE)

  ## ** Then for hex ------------------------------------------------
  hex_sf <- lapply(hex_sf, FUN = function(x) {
    sf::st_make_valid(x) |> st_collection_extract("POLYGON")
  })

  crs_target   <- lapply(hex_sf, FUN = function(x) sf::st_crs(x))

  ## Extract all of the identifier names for the sub-sub regions, retaining just the name column
  all_identifiers <- lapply(hex_sf, FUN = function(x) {
    district_identifiers <- names(x)
    district_name_col   <- district_identifiers[grepl("Name", district_identifiers)]
    district_identifiers <- district_identifiers[!grepl("Name", district_identifiers)]
    list(district_name_col, district_identifiers)
  })

  names(all_identifiers) <- lapply(hex_sf, FUN = function(x) x$shapeGroup[1])

  df_sorted2 <- purrr::pmap(list(crs_target, hex_sf, all_identifiers, names(all_identifiers)), .f = function(x, y, z, q) {
    ## Convert to sf
    pts_sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = x)

    old_s2 <- sf_use_s2(TRUE)

    ## Spatial join: keep only points in the full set of sub-sub regions
    joined <- try({
        sf::st_join(pts_sf, y, left = FALSE)
    }, silent = TRUE)

    if (class(joined)[1] == "try-error") {
      old_s2 <- sf_use_s2(FALSE)
      joined <- try({
        sf::st_join(pts_sf, y, left = FALSE)
      }, silent = TRUE)
    }

    ## Extract coordinates back into x and y columns
    ## (mostly for plotting and debugging purposes)
    coords <- sf::st_coordinates(joined)
    joined <- joined |>
      mutate(x = coords[, 1], y = coords[, 2], .before = 1) |>
      as_tibble() |>
      dplyr::select(-all_of(z[[2]])) |>
      relocate(!!z[[1]], .after = "y") |>
      mutate(Country = q, .before = shapeName)

  }) |>
  bind_rows()

  ## H3 hexes tile edge-to-edge with no gaps or overlaps, but the default st_join match
   ## (st_intersects) can still match a point lying exactly on a shared hex edge to both
   ## neighboring hexes; keep a single deterministic match per point as above
  df_sorted2 <- df_sorted2 |> distinct(index, .keep_all = TRUE)

  df_sorted2 <- df_sorted2 |> dplyr::select(index, shapeName)

  df_sorted <- df_sorted |>
    rename(ADM2 = shapeName) |>
    left_join(df_sorted2) |>
    relocate(shapeName, .after = ADM2)


  ## Round to 7 decimal places so coordinates match across the pipeline
  df_sorted |>
    dplyr::select(x, y, Country, ADM2, shapeName) |>
    mutate(x = round(x, 7), y = round(y, 7))

}


#' @title mask_and_cluster_from_template

#' @param template ADM regions for one date built from mask_and_cluster_build_template
#' @param cov_files all covariate parquet files
#' @param out_dir directory to save output
#' @param overwrite boolean to recalculate and save over a previously saved file or not
#' @return character vector path to parquet files
#' @author Morgan Kain
#' @export

mask_and_cluster_from_template <- function(template, cov_files, out_dir, overwrite = FALSE) {

  ## Check that we're only working on one date at a time
  stopifnot(length(cov_files) == 1)

  ## Set filename
  save_filename <- paste(
    out_dir
    , "/"
    , out_dir |> strsplit("data/") |> unlist() |> tail(1)
    , "_"
    , cov_files |> strsplit("data_") |> unlist() |> tail(1)
    , sep = ""
  )
  message(paste0("Processing ", cov_files))

  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  existing_dataset        <- error_safe_read_parquet(save_filename)

  if (!is.null(existing_dataset) && !overwrite) {
    # Check if file has data - if zero rows, overwrite anyway
    row_count <- existing_dataset |> count() |> collect() |> pull(n)
    if (row_count > 0) {
      message("file already exists and can be loaded, skipping processing")
      return(save_filename)
    } else {
      message("file exists but has zero rows, overwriting")
    }
  }

  ## Read one parquet file; round coordinates to match template precision
  df <- read_parquet(cov_files) |>
    mutate(x = round(x, 7), y = round(y, 7))

  ## Join with the template to get the ADM region info
  df_with_adm <- df |>
    left_join(template |>
                mutate(x = round(x, 7), y = round(y, 7))) |>
    relocate(Country, ADM2, shapeName, .after = y) |>
    filter(!is.na(Country))

  ## Write output to a parquet file
  arrow::write_parquet(df_with_adm, save_filename, compression = "gzip", compression_level = 5)

  save_filename

}
