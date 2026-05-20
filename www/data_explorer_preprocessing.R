####
## Code associated with building the correct data structures for the app for
## exploring the raw data
####

#### Build required objects ======================================================

df_raw <- read_parquet("www/raw_data.parquet")

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
      all_of(c(static_covars, temporal_covars, lagged_covars, forecast_covars))
      , mean)
    , .groups = "drop")

## Hex-level summary (for map + scatter)
hex_summary <- df_avg |>
  group_by(shapeName) |>
  summarise(across(-date, mean), .groups = "drop") |>
  left_join(hex_sf, by = "shapeName") |>
  st_as_sf()

## Long format for density / scatter
df_long <- df_avg |>
  pivot_longer(cols = -c(shapeName, date),
               names_to = "variable",
               values_to = "value")

saveRDS(hex_sf, "www/hex_sf.rds")
write_parquet(df_avg, "www/df_avg.parquet", compression = "gzip", compression_level = 5)
saveRDS(hex_summary, "www/hex_summary.rds")

map_dat <- qread("www/data_for_app.qs")

cal.dat  <- map_dat$cal.tib[[1]]
shap.dat <- map_dat$shap.tib[[1]]
map.dat  <- map_dat$map.tib[[1]]
dat.no_agg <- map.dat$no_agg[[1]] |>
    as.data.frame() |>
    dplyr::select(-geometry)
dat.double_agg <- map.dat$double_agg[[1]]
dat.temporal_agg <- map.dat$temporal_agg[[1]]
dat.spatial_agg <- map.dat$spatial_agg[[1]]
cal.double_agg <- cal.dat$double_agg[[1]]

rm(cal.dat)

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
  parent_lookup = parent_lookup
, no_agg_with_big_hexes = no_agg_with_big_hexes
, avg_map_data = avg_map_data
, cal.double_agg = cal.double_agg
, dat.no_agg = dat.no_agg
, parent_lookup = parent_lookup
, dat.temporal_agg = dat.temporal_agg
, dat.spatial_agg = dat.spatial_agg
, shap.dat        = shap.dat
  )
, "www/app_data_processed.Rds"
)

install.packages("shinylive")

shinylive::export(
    appdir  = "shinylive/prediction_explorer",
    destdir = "docs"
)

shinylive::app_start("docs")
