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
hex_sf <- df_raw %>%
  distinct(shapeName) %>%
  mutate(geometry = h3jsr::cell_to_polygon(shapeName, simple = FALSE) %>% pull(geometry)) %>%
  st_as_sf()

## Average across forecast intervals (default version)
df_avg <- df_raw %>%
  group_by(shapeName, date) %>%
  summarise(
    across(
      all_of(c(static_covars, temporal_covars, lagged_covars, forecast_covars))
      , mean)
    , .groups = "drop")

## Hex-level summary (for map + scatter)
hex_summary <- df_avg %>%
  group_by(shapeName) %>%
  summarise(across(-date, mean), .groups = "drop") %>%
  left_join(hex_sf, by = "shapeName") %>%
  st_as_sf()

## Long format for density / scatter
df_long <- df_avg %>%
  pivot_longer(cols = -c(shapeName, date),
               names_to = "variable",
               values_to = "value")

saveRDS(hex_sf, "www/hex_sf.rds")
write_parquet(df_avg, "www/df_avg.parquet", compression = "gzip", compression_level = 5)
saveRDS(hex_summary, "www/hex_summary.rds")

