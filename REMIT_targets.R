# This repository uses targets projects.
# To switch to the REMIT pipeline run:
# `Sys.setenv(TAR_PROJECT = "remit")`

# Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true")) {
  capsule::capshot(c(
    "packages.R",
    list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE),
    list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)
  ))
}

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source(f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Targets options
source("_targets_settings.R")

# Convenience function to format .env flags properly for overwrite parameter and target cues
# For AWS targets if the overwrite flag is 'TRUE' we don't want to download data from AWS
# otherwise we always want to check.
parse_flag <- function(flags, cue = NULL) {
  stopifnot(cue %in% c(NULL, "never", "always"))
  flag <- any(as.logical(Sys.getenv(flags, unset = "FALSE")))
  if (!is.null(cue)) flag <- targets::tar_cue(ifelse(flag, cue, ifelse(cue == "never", "always", "thorough")))
  flag
}

# REMIT Targets ---------------------
REMIT_targets <- tar_plan(
  tar_target(REMIT_weather_vars,
             c(
               ## Theoretically can obtain these as well, but have to use nasapower::get_power and for that
               ## have to provide small regions of the map at a time. So if we want these two variables would
               ## have to jump through quite a few hoops which may not be worth it as these covariates have similar
               ## analogs elsewhere in the dataset
               #   "solar_rad"        = "ALLSKY_SFC_SW_DWN"
               # , "clouds"           = "CLOUD_AMT"
               "evapotrans"       = "EVPTRNS"
               , "soil_moisture"    = "GWETPROF"
               , "precipitation"    = "PRECTOTCORR"
               , "spec_humid_2m"    = "QV2M"
               , "rel_humid_2m"     = "RH2M"
               , "air_temp_avg"     = "T2M"
               , "air_temp_max"     = "T2M_MAX"
               , "air_temp_min"     = "T2M_MIN"
               , "air_temp_range"   = "T2M_RANGE"
               , "dewpoint"         = "T2MDEW"
               , "surface_temp_avg" = "TS"
               , "wind_speed_10m"   = "WS10M"
               , "wind_speed_2m"    = "WS2M"))
  , tar_target(nasa_weather_transformed_REMIT
               , fetch_and_transform_nasa_weather(
                 months_to_process
                 , REMIT_weather_vars
                 , continent_raster_template
                 , local_folder = "data/nasa_weather_transformed_REMIT"
                 , basename_template = "nasa_weather_transformed_{months_to_process}.parquet"
                 , endpoint = "https://power-datastore.s3.amazonaws.com/v10/daily/{year}/{month}/power_10_daily_{yyyymmdd}_merra2_lst.nc"
                 , overwrite = FALSE
                 , NULL
                 , dates_to_process)
               , pattern = map(months_to_process)
               , error = "null"
               , format = "file")
  , tar_target(nasa_weather_summarized_REMIT
               , summarize_REMIT_weather_data(
                 dat          = nasa_weather_transformed_REMIT
                 , weather_vars = REMIT_weather_vars
                 , yrs          = seq(2005, 2025)
                 , path_to_out  = "data/nasa_weather_summarized_REMIT")
               , pattern = map(REMIT_weather_vars)
               , error   = "null"
               , format  = "file")
  , tar_target(nasa_weather_REMIT_combined
               , combine_REMIT_weather_data(
                 nasa_weather_summarized_REMIT
                 , "data/nasa_weather_combined_REMIT"))
  , tar_target(nasa_weather_REMIT_cleaned
               , impute_REMIT_weather_data(
                 nasa_weather_REMIT_combined
                 , "data/nasa_weather_combined_REMIT"))
  , tar_target(africa_full_predictor_data_sources_REMIT, list(
    nasa_weather_REMIT_cleaned = nasa_weather_REMIT_cleaned,
    bioclim_preprocessed = bioclim_preprocessed,
    soil_preprocessed = soil_preprocessed,
    aspect_preprocessed = aspect_preprocessed,
    slope_preprocessed = slope_preprocessed,
    glw_preprocessed = glw_preprocessed,
    elevation_preprocessed = elevation_preprocessed,
    landcover_preprocessed = landcover_preprocessed))
  , tar_target(africa_full_predictor_data_REMIT, {
    joined_df <- map(africa_full_predictor_data_sources_REMIT %>% unlist()
                     , .f = function(this_file) { arrow::read_parquet(this_file) }) %>%
      reduce(., left_join, by = c("x", "y"))
    joined_df <- joined_df[complete.cases(joined_df), ]
    joined_df %>% arrow::write_parquet(
      "data/africa_full_predictor_data_REMIT/REMIT_data.parquet"
      , compression = "gzip", compression_level = 5)
    return("data/africa_full_predictor_data_REMIT/REMIT_data.parquet")
  })
)


# List targets -----------------------------------------------------------------
list(
  REMIT_targets
)