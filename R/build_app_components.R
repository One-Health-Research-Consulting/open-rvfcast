#' Data files for the forecast explorer apps (hexes and up-scaled regions)
#'
#' Builds one file per app (www/forecast_explorer_hex.R and www/forecast_explorer_regions.R),
#' in the same layout so both apps share their code (www/forecast_explorer_common.R):
#'  - geometry: the mapped units in lon/lat with unit_id, unit_name and country
#'  - forecasts: forward forecasts by unit, base date and window (prob; regions also carry the
#'    perfect-dependence and independence bounds and the share of the region covered by hexes)
#'  - context: per-unit forecast quality and outbreak history saved by the most recent
#'    PURPOSE = train run (save_forecast_context())
#'  - outbreaks: every labelled outbreak (cases > 0) placed in its unit (hexes: the labelled hex;
#'    regions: by coordinates, as for the regional labels)
#'  - meta: unit labels, forecast windows, colour limits (the test-period limits saved with the
#'    context, so app colours match the static maps), history_end and the context run date
#' Regional forecasts are the hex forecasts up-scaled with the saved copula fit
#' (predict_regional_copula()).
#'
#' @title build_forecast_app_data
#' @param forecast_files Files written by examine_fits_within() in a PURPOSE = forecast run (the
#'   examined_fits_within_pan target); every forecasts_for_Africa_<date>.json among them is used
#' @param region_hexes The region_hexes target
#' @param hexed_outbreaks_path Outbreaks placed in their labelled hexes; saved by
#'   export_hexed_outbreaks() (data/rvf_outbreaks_hexed.Rds)
#' @param context_dir Folder of saved training-run context (the most recent file is used)
#' @param forecast_horizon Upper bounds (days) of the consecutive forecast windows
#' @param countries_path Country outlines with ADM0_A3 and NAME columns, for country names
#' @param outpath Folder the two app data files are written to
#' @return Paths to the two saved files (forecast_app_hex.rds and forecast_app_regions.rds)
#' @author Morgan Kain
#' @export
build_forecast_app_data <- function(
    forecast_files
  , region_hexes
  , hexed_outbreaks_path
  , context_dir      = "outputs/forecast_context"
  , forecast_horizon = c(30, 60, 90, 120, 150)
  , countries_path   = "www/countries.geojson"
  , outpath          = "www"
) {

  ## Context saved by the most recent training run, and this run's forward forecasts
  context_path <- get_latest_forecast_context(context_dir)
  context      <- qs::qread(context_path)
  hex_preds    <- read_forecast_hex_predictions(forecast_files)

  if (!inherits(region_hexes, "sf")) region_hexes <- region_hexes[[1]]

  ## ISO3 -> country name lookup for labels
  country_names <- sf::read_sf(countries_path) |>
    sf::st_drop_geometry() |>
    dplyr::distinct(country = ADM0_A3, country_name = NAME)

  hexed_outbreaks <- readRDS(hexed_outbreaks_path) |> dplyr::filter(counts_toward_label == 1)

  ## Shared meta data; window labels follow the "days lower-(upper-1)" convention of the maps
  meta <- list(
    forecast_windows = make_forecast_windows(forecast_horizon) |>
      dplyr::mutate(window_label = paste0("Days ", window_lower, "-", forecast_interval - 1))
  , history_end      = context$history_end
  , context_run_date = context$run_date
  , context_file     = basename(context_path)
  , forecast_dates   = sort(unique(hex_preds$date)))

  
  #### Hexes -------------------------------------------------------------------------------

  ## Hex country = the country most of its forecast rows are labelled with
  hex_country <- purrr::map_dfr(grep("forecasts_for_Africa_.*\\.json$", unlist(forecast_files), value = TRUE)
                                , ~ jsonlite::read_json(.x, simplifyVector = TRUE) |> dplyr::distinct(shapeName = H3, country = Country)) |>
    dplyr::count(shapeName, country) |>
    dplyr::slice_max(n, n = 1, by = shapeName, with_ties = FALSE) |>
    dplyr::select(-n)

  hex_app <- list(
    geometry  = region_hexes |>
      dplyr::select(shapeName) |>
      sf::st_transform(4326) |>
      dplyr::left_join(hex_country, by = "shapeName") |>
      dplyr::left_join(country_names, by = "country") |>
      dplyr::transmute(unit_id = shapeName, unit_name = paste0("Hex ", shapeName), country, country_name)
  , forecasts = hex_preds |> dplyr::transmute(unit_id = shapeName, date, forecast_interval, prob = prob_pred)
  , context   = context$hex_context |> dplyr::rename(unit_id = shapeName)
  , outbreaks = hexed_outbreaks |> dplyr::transmute(unit_id = shapeName, outbreak_id, start_date, cases, country = country_iso3c)
  , meta      = c(meta, list(unit = "hex", unit_label = "hex", units_label = "hexes", fill_limits = context$fill_limits$hex)))

  
  #### Regions -----------------------------------------------------------------------------

  region_overlap <- build_hex_region_overlap(context$regions_path, region_hexes)
  region_rows    <- predict_regional_copula(hex_preds, region_overlap, context$params_full, context$copula_settings)

  region_app <- list(
    geometry  = region_overlap$regions_ll |>
      dplyr::left_join(country_names, by = "country") |>
      dplyr::transmute(unit_id = as.character(area_index), unit_name = short_region_name(region, country_name), country, country_name
                       , districts = region, adm_km2) |>
      sf::st_as_sf()
  , forecasts = region_rows |>
      dplyr::transmute(unit_id = as.character(area_index), date, forecast_interval, prob = p_copula
                       , prob_lower = p_max, prob_upper = p_indep, coverage)
  , context   = context$region_context |> dplyr::mutate(unit_id = as.character(area_index), .before = 1)
  , outbreaks = place_outbreaks_in_regions(hexed_outbreaks, region_overlap$regions_ea) |>
      dplyr::left_join(hexed_outbreaks |> dplyr::select(outbreak_id, country = country_iso3c), by = "outbreak_id") |>
      dplyr::transmute(unit_id = as.character(area_index), outbreak_id, start_date, cases, country)
  , meta      = c(meta, list(unit = "region", unit_label = "region", units_label = "regions", fill_limits = context$fill_limits$regional)))

  paths <- file.path(outpath, c("forecast_app_hex.rds", "forecast_app_regions.rds"))
  saveRDS(hex_app, paths[1])
  saveRDS(region_app, paths[2])
  paths

}


#' Create the needed data files for the visualization app
#'
#' Builds the data for the original prediction explorer (www/prediction_explorer.R), from the
#' per-aggregation files the former examine_fits_within() saved. Kept for reference; no longer
#' run by the pipeline.
#'
#' @title build_app_components

#' @param predictions target examined_fits_within_pan
#' @param shapvals shap values
#' @param outpath path to save file for app
#' @return path to saved file
#' @author Morgan Kain
#' @export

build_app_components_predictions <- function(predictions, shapvals, outpath) {

  predictions.map <- predictions[grepl("predictions", predictions)]
  predictions.cal <- predictions[grepl("calcurves", predictions)]

  files_tibble.map <- tibble(
    no_agg       = predictions.map[grepl("No_aggregation", predictions.map)]
  , double_agg   = predictions.map[grepl("Double", predictions.map)]
  , temporal_agg = predictions.map[grepl("Temporal", predictions.map)]
  , spatial_agg  = predictions.map[grepl("Spatial", predictions.map)]
  )

  files_tibble.cal <- tibble(
    no_agg       = predictions.cal[grepl("No_aggregation", predictions.cal)]
  , double_agg   = predictions.cal[grepl("Double", predictions.cal)]
  , temporal_agg = predictions.cal[grepl("Temporal", predictions.cal)]
  , spatial_agg  = predictions.cal[grepl("Spatial", predictions.cal)]
  )

  no_agg.map       <- purrr::map(files_tibble.map$no_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  double_agg.map   <- purrr::map(files_tibble.map$double_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  temporal_agg.map <- purrr::map(files_tibble.map$temporal_agg, .f = function(i) {
   qread(i)
  }) |> bind_rows()
  spatial_agg.map  <- purrr::map(files_tibble.map$spatial_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()

  no_agg.cal       <- purrr::map(files_tibble.cal$no_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  double_agg.cal   <- purrr::map(files_tibble.cal$double_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  temporal_agg.cal <- purrr::map(files_tibble.cal$temporal_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  spatial_agg.cal  <- purrr::map(files_tibble.cal$spatial_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()

  map.tib <- tibble(
    no_agg       = no_agg.map |> list()
  , double_agg   = double_agg.map |> list()
  , temporal_agg = temporal_agg.map |> list()
  , spatial_agg  = spatial_agg.map |> list()
  )

  cal.tib <- tibble(
    no_agg       = no_agg.cal |> list()
  , double_agg   = double_agg.cal |> list()
  , temporal_agg = temporal_agg.cal |> list()
  , spatial_agg  = spatial_agg.cal |> list()
  )

  shap.tib <- shapvals |>
      dplyr::select(shap_summary) |>
      rename(shap.tib = shap_summary)

  map_dat <- tibble(
    map.tib  = map.tib |> list()
  , cal.tib  = cal.tib |> list()
  ) |>
  bind_cols(shap.tib)

## Do teh rest of the cleanup and prepwork for hosting the predictions
cal.dat <- map_dat$cal.tib[[1]]
shap.dat <- map_dat$shap.tib[[1]]
map.dat <- map_dat$map.tib[[1]]
dat.no_agg <- map.dat$no_agg[[1]] |> as.data.frame() |> dplyr::select(-geometry)
dat.double_agg <- map.dat$double_agg[[1]]
dat.temporal_agg <- map.dat$temporal_agg[[1]]
dat.spatial_agg <- map.dat$spatial_agg[[1]]
cal.double_agg <- cal.dat$double_agg[[1]]

rm(cal.dat)

## Build lookup (runs once at startup)
parent_lookup <- make_parent_lookup(
    small_ids = unique(dat.temporal_agg$shapeName),
    large_ids = unique(dat.double_agg$region_norm)
)

## Ensure date columns are Date class
dat.no_agg$date <- as.Date(dat.no_agg$date)
dat.double_agg$date <- as.Date(dat.double_agg$date)
dat.temporal_agg$date <- as.Date(dat.temporal_agg$date)
dat.spatial_agg$date <- as.Date(dat.spatial_agg$date)
cal.double_agg$min_date <- as.Date(cal.double_agg$min_date)
cal.double_agg$max_date <- as.Date(cal.double_agg$max_date)

####
## Setup for second tab
####

## Fully aggregated hexes across the full time series
avg_map_data <- dat.double_agg |>
    group_by(region_norm) |>
    summarise(
        prob_pred = mean(prob_pred, na.rm = TRUE),
        an_outbreak = max(true_out),
        geometry = dplyr::first(geometry),
        .groups = "drop"
    ) |>
    st_as_sf()

rm(dat.double_agg)
gc()

no_agg_with_big_hexes <- dat.spatial_agg |>
    as.data.frame() |>
    dplyr::select(-geometry) |>
    rename(small_ids = region_norm)

saveRDS(
    list(
        parent_lookup = parent_lookup,
        no_agg_with_big_hexes = no_agg_with_big_hexes,
        avg_map_data = avg_map_data,
        cal.double_agg = cal.double_agg,
        dat.no_agg = dat.no_agg,
        parent_lookup = parent_lookup,
        dat.temporal_agg = dat.temporal_agg,
        dat.spatial_agg = dat.spatial_agg,
        shap.dat = shap.dat
    ),
    outpath
)

  outpath

}

build_app_components_data <- function(df_raw, outpath) {

## poorly non-dynamic
all_cols        <- names(df_raw)[-c(1:10)]
static_covars   <- all_cols[c(1:3, 4, 5, 6, 7, 8:30, 46, 47)]
lagged_covars   <- all_cols[c(34:45)]
forecast_covars <- all_cols[c(31:33)]
temporal_covars <- all_cols[c(48)]

#### Pre-processing ==============================================================

df_raw$date <- as.Date(df_raw$date)

## Build geometry from H3
hex_sf <- df_raw |>
    distinct(shapeName) |>
    mutate(geometry = h3jsr::cell_to_polygon(shapeName, simple = FALSE) |>
        pull(geometry)) |>
    st_as_sf()

## Average across forecast intervals (default version)
df_avg <- df_raw |>
    group_by(shapeName, date) |>
    summarise(
        across(
            all_of(c(static_covars, temporal_covars, lagged_covars, forecast_covars)),
            mean
        ),
        .groups = "drop"
    )

## Hex-level summary (for map + scatter)
hex_summary <- df_avg |>
    group_by(shapeName) |>
    summarise(across(-date, mean), .groups = "drop") |>
    left_join(hex_sf, by = "shapeName") |>
    st_as_sf()

## Long format for density / scatter
df_long <- df_avg |>
    pivot_longer(
        cols = -c(shapeName, date),
        names_to = "variable",
        values_to = "value"
    )

saveRDS(hex_sf, paste0(outpath, "/hex_sf.rds"))
write_parquet(df_avg, paste0(outpath, "/df_avg.parquet"), compression = "gzip", compression_level = 5)
saveRDS(hex_summary, paste0(outpath, "/hex_summary.rds"))

c(paste0(outpath, "/hex_sf.rds"), paste0(outpath, "/df_avg.parquet"), paste0(outpath, "/hex_summary.rds"))

}


### Helper Functions -----------------------------------------------------------

## Short display name for an aggregated ADM2 region: country, then the first districts of its
## " + "-joined district list and how many more there are
short_region_name <- function(region, country_name, n_show = 3) {
  purrr::map2_chr(strsplit(region, " + ", fixed = TRUE), country_name, function(d, cn) {
    shown <- paste(utils::head(d, n_show), collapse = ", ")
    more  <- if (length(d) > n_show) paste0(" and ", length(d) - n_show, " more") else ""
    paste0(cn, ": ", shown, more)
  })
}

## Derive the parent -to- child hex lookup once
make_parent_lookup <- function(small_ids, large_ids) {
    ## Infer the resolution of the large hexes from the first valid id
    large_res <- h3jsr::get_res(large_ids[1])
    ## Map every small hex to its parent at the large resolution
    parents <- h3jsr::get_parent(small_ids, large_res)
    data.frame(shapeName = small_ids, region_norm = parents, stringsAsFactors = FALSE)
}

build_parent_frame <- function(hex_ids, scale_up) {
    child_parent <- data.frame(
        small_ids = hex_ids,
        h3_address = h3jsr::get_parent(hex_ids, scale_up)
    )

    cell_to_polygon(unique(child_parent$h3_address), simple = FALSE) |>
        left_join(child_parent, by = "h3_address") |>
        rename(large_ids = h3_address)
}
