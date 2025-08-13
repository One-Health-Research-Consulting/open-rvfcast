# This repository uses targets projects.
# To switch to the data acquisition and cleaning pipeline run:
# `Sys.setenv(TAR_PROJECT = "rvf")`

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

data_import_targets <- tar_plan(
  
  ## Polygon of Africa 
  tar_target(continent_polygon, create_africa_polygon())
  
  ## Africa shape object for masking
  , tar_target(wahis_raster_template, terra::rasterize(
    terra::vect(continent_polygon)
    ## Mask against a raster filled with 1's
    , terra::rast(
      continent_polygon
      ## Set Resolution
      , resolution = 0.1
      , vals = 1
    )
    ## Wrap to avoid problems with targets
  ) |> terra::wrap()) 
  
  ## Import base predictors from the predictor processing project
  , tar_target(base_predictors_directory,
               create_data_directory(directory_path = "data/africa_full_predictor_data")
  )
  
  ## Download predictor files from AWS if they don't already exist
  , tar_target(base_predictors_AWS,
               AWS_get_folder(base_predictors_directory,
                              skip_fetch = TRUE,
                              sync_with_remote = FALSE
               ),
               error = "continue",
               cue = tar_cue("always")
  )
  
  ## Read all parquet files in the directory using Arrow
  , tar_target(base_predictors,
               list.files(base_predictors_directory, pattern = "\\.parquet$", full.names = TRUE)
  )
  
  ## Import RVF outbreak data
  , tar_target(rvf_outbreaks, get_wahis_rvf_outbreaks() |>
                 mutate(
                   start_date = coalesce(outbreak_start_date, outbreak_end_date)
                   , end_date   = coalesce(outbreak_end_date, outbreak_start_date)
                 ) |>
                 select(cases, start_date, end_date, latitude, longitude) |>
                 distinct() |>
                 arrange(end_date) |>
                 mutate(outbreak_id = seq_len(n())))
  
  ## Set up directory for cleaned case data
  , tar_target(
    rvf_response_directory,
    create_data_directory(directory_path = "data/rvf_response")
  )
  
  ## Rebuild dates used to generate predictors (also used in previous pipeline)
  , tar_target(dates_in_predictors, set_model_dates(
    start_year = 2005,
    end_year = lubridate::year(Sys.time()),
    n_per_month = 2,
    seed = 212
  ),
  cue = tar_cue("always"))
  
  ## Conceivably there could be a situation where we would want to make predictions for 
  ## dates that do not perfectly align with the same dates that we used to generate our
  ## predictions, so writing the downstream functions to allow for that.
  ## *However* for now proceeding with these two dates being the same
  , tar_target(dates_for_predictions, dates_in_predictors),
  
  ## dates_for_predictions --> rvf_response --> rvf_model_data
  
  ## Creates a tibble that contains, for each given dates_to_process
  ## and forecast interval, the outbreaks in the forecast interval duration
  ## after the given dates_to_process
  tar_target(rvf_response, get_rvf_response(
    rvf_outbreaks
    , wahis_raster_template
    , forecast_intervals = c(1, 30, 60, 90, 120, 150)
    , dates_to_process = dates_in_predictors
    , local_folder = rvf_response_directory
  )
  , format = "file"
  , repository = "local"
  )
  
  ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  , tar_target(region_name, "RSF")
  , tar_target(region_data_directory, create_data_directory(
    directory_path = paste("data/", region_name, "_full_response_data", sep = "")
  ))
  , tar_target(region_districts, rgeoboundaries::geoboundaries("South Africa", "adm2"))
  
)

## Build final master dataset for model fitting by:
## A) Masking to the Sub-Region of interest
## B) Setting up lagged variables
## C) Joining in cases
## D) Summarizing covariates and cases to the Sub-Sub-Region of interest
rvf_processing_targets <- tar_plan(
  
  ## Build smaller more manageable .parquet files composed of the same dates but
  ## with data masked to the Sub-Region and with Sub-Sub Regions identified
  tar_target(region_data, mask_and_cluster(
    cov_files       = base_predictors
    , districts_sf    = region_districts
    , district_id_col = "shapeName"
    , out_dir         = region_data_directory
    , overwrite       = FALSE
  )
  , pattern = map(base_predictors)
  , error   = "null"
  , format  = "file"
  )
  
  ## Set up folder for the cleaned data
  , tar_target(region_cleaned_data_directory, create_data_directory(
    directory_path = paste("data/", region_name, "_cleaned_response_data", sep = "")
  ))
  
  ## Calculate lags, join cases, summarize and build master dataset
  , tar_target(cleaned_region_data, lag_join_aggregate(
    cov_files       = region_data[-length(region_data)]
    , rvf_response    = rvf_response
    , district_id_col = "shapeName"
    , out_dir         = region_cleaned_data_directory
    , overwrite       = FALSE
  ))
  
)

list(
  data_import_targets,
  rvf_processing_targets
)
