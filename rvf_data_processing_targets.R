# This repository uses targets projects.
# To switch to the data acquisition and cleaning pipeline run:
# `Sys.setenv(TAR_PROJECT = "rvf")`

## NOTES / ToDo ----------------------------------------------------------------

## Setup / Preamble ------------------------------------------------------------

## Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true")) {
  capsule::capshot(c(
    "packages.R",
    list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE),
    list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)
  ))
}

## Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source(f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

## Targets options
source("_targets_settings.R")

## Some settings that change much about the pipeline ---------------------------
using_hexes <- TRUE

## Targets for loading needed data ---------------------------------------------
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
      ## Wrap to avoid problems with targets
    )) |>
    terra::wrap())

  ## Import base predictors from the predictor processing project
  , tar_target(base_predictors_directory,
               create_data_directory(directory_path = "data/africa_full_predictor_data"))

  ## Download predictor files from AWS if they don't already exist
  , tar_target(base_predictors_AWS,
               AWS_get_folder(
                 base_predictors_directory
               , skip_fetch = TRUE
               , sync_with_remote = FALSE)
               , error = "continue"
               , cue   = tar_cue("always"))

  ## Read all parquet files in the directory using Arrow
  , tar_target(base_predictors,
               list.files(base_predictors_directory, pattern = "\\.parquet$", full.names = TRUE))

  ## Import RVF outbreak data
  , tar_target(rvf_outbreaks, get_wahis_rvf_outbreaks() |>
                 mutate(
                   start_date = coalesce(outbreak_start_date, outbreak_end_date)
                 , end_date   = coalesce(outbreak_end_date, outbreak_start_date)
                 ) |>
                 filter(grepl("sheep|cattle|camelidae|goat", species)) |>
                 select(cases, start_date, end_date, latitude, longitude) |>
                 distinct() |>
                 arrange(end_date) |>
                 mutate(outbreak_id = seq_len(n())))

  ## Import RVF seroprevalence data
  , tar_target(rvf_seroprevalence, readRDS("data/cases_sero.Rds")$sero_data |> mutate(index = seq_len(n())))

  ## Set up directory for cleaned case data
  , tar_target(rvf_response_directory
    , create_data_directory(directory_path = "data/rvf_response"))

  ## Rebuild dates used to generate predictors (also used in previous pipeline)
  , tar_target(dates_in_predictors, set_model_dates(
      start_year = 2005
    , end_year = lubridate::year(Sys.time())
    , n_per_month = 2
    , seed = 212)
  , cue = tar_cue("always"))

  ## Conceivably there could be a situation where we would want to make predictions for
  ## dates that do not perfectly align with the same dates that we used to generate our
  ## predictions, so writing the downstream functions to allow for that.
  ## *However* for now proceeding with these two dates being the same
  , tar_target(dates_for_predictions, dates_in_predictors)

  ## dates_for_predictions --> rvf_response --> rvf_model_data

  ## Creates a tibble that contains, for each given dates_to_process
  ## and forecast interval, the outbreaks in the forecast interval duration
  ## after the given dates_to_process
  , tar_target(rvf_response, get_rvf_response(
      rvf_outbreaks
    , wahis_raster_template
    , forecast_intervals = c(1, 30, 60, 90, 120, 150)
    , dates_to_process   = dates_in_predictors
    , local_folder       = rvf_response_directory)
  , format     = "file"
  , repository = "local")

  ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  , tar_target(region_name, ifelse(using_hexes, "pan_hex", "pan"))

  , tar_target(region_data_directory, create_data_directory(
      directory_path = paste("data/", region_name, "_full_response_data", sep = "")))

  ## Pulls all African countries. Alternatively can just provide a single country
   ## directly to get_region_districts below
  , tar_target(all_african_countries, {
    ne_countries(continent = "Africa", returnclass = "sf") |>
      st_drop_geometry() |>
      pull(admin) |>
      sort()})

  ## Sub-regions of region of interest
   ## Takes a character vector of any length
  , tar_target(region_districts, get_region_districts(c(all_african_countries, "MYT", "COM")))

  ## Build the alternative hexagon-based spatial aggregation
  , tar_target(region_hexes, make_hex_grid_h3(
      template_rast   = unwrap(wahis_raster_template)
    , target_area_km2 = 12000
    , h3_res          = NULL))

  , tar_target(region_map, if (using_hexes) {
      region_hexes
    } else {
      region_districts
    })

  ## Prep the seroprevalence-cases dataset
  , tar_target(cases_sero, prep_cases_sero_dataset(
      sero_dat  = rvf_seroprevalence
    , cases_dat = rvf_outbreaks
    , map_dat   = region_map))

)

## Targets to build and process the sero layer ---------------------------------
modeling_targets <- tar_plan(

  ## Fit the spatio-temporal kernel model
  tar_target(sero_stan_model,
    fit_sero_cases_stan(
      stan_dat  = cases_sero$stan_data
    , outpath   = "data/sero_kernel_icar_base_model_samples.Rds"
    , overwrite = FALSE)
    , error     = "null"
    , format    = "file")

  ## Make predictions from the fitted stan model over a series of targets in order to faciliate parallelization
   ## for this computationally expensive step
, tar_target(prepped_pairs, prep_all_pairs(
    sero_cases_dat     = cases_sero
  , cov_dat            = joined_region_data
  , map_dat            = region_map[[1]]))

  ## adjusted extracted samples from the fitted model
, tar_target(prepped_samps, prep_samps(fitted_stan_model = sero_stan_model, time_adjustment = TRUE))

  ## all linkages between forecasted dates and neighboring outbreaks
, tar_target(prepped_all_dates, prep_all_dates(cov_dat = joined_region_data))

  ## estimated seroprevalence for prepped_all_dates
, tar_target(built_sero_for_outbreaks, build_sero_for_outbreaks(
    the_pairs = prepped_pairs
  , the_samps = prepped_samps)
  , pattern   = map(prepped_pairs))

  ## Pull together the final layer
, tar_target(finished_sero_layer, finish_sero_layer(
    sero_cases_dat = cases_sero
  , samps          = prepped_samps
  , with_outbreaks = built_sero_for_outbreaks
  , all_dates      = prepped_all_dates
  , outpath        = "data/sero_layer.parquet"
  , overwrite      = FALSE)
  , error          = "null"
  , format         = "file")

)

## Build final master dataset for model fitting --------------------------------
 ## A) Masking to the Sub-Region of interest
 ## B) Setting up lagged variables
 ## C) Joining in cases
 ## D) Summarizing covariates and cases to the Sub-Sub-Region of interest
rvf_processing_targets <- tar_plan(

## Determine the Country and ADM2 (or 1) region and H3 hex for all of the x, y coordinates
 tar_target(region_data_template, mask_and_cluster_build_template(
    cov_files        = base_predictors[1]
  , hex_sf          = region_hexes
  , countries_sf    = region_districts
  , district_id_col = "shapeName"
  , out_dir         = region_data_directory))

## Use the template to categorize all x, y, coordinates for all of the data (by date)
 ## Mask to the Sub-Region of interest (drop nothing if pan-African is desired) and
 ## with Sub-Sub Regions identified
 ## Build smaller more manageable .parquet files composed of the same dates but
, tar_target(region_data, mask_and_cluster_from_template(
      template        = region_data_template
    , cov_files        = base_predictors
    , out_dir         = region_data_directory
    , overwrite       = FALSE)
  , pattern = map(base_predictors)
  , error   = "null"
  , format  = "file")

  ## Set up folders for the cleaned data
  , tar_target(region_cleaned_data_directory, create_data_directory(
     directory_path = paste("data/", region_name, "_cleaned_response_data", sep = "")))

  , tar_target(region_joined_data_directory, create_data_directory(
     directory_path = paste("data/", region_name, "_joined_response_data", sep = "")))

  ## First step in building the lagged variables of figuring out what files are needed for each
   ## of the lags. Save these details for each individual date in a list of tibbles
  , tar_target(prepped_dates, prep_dates(
      cov_files     = region_data[-length(region_data)]
    , rvf_response = rvf_response))

  ## some finicky bs to get arrow to work. For whatever reason will not work unless I pull these out
   ## of the prepped_dates object
  , tar_target(file_path_per_date, lapply(prepped_dates, FUN = function(x) x$filename[1]) |> unlist())

  ## Calculate lags, join cases, summarize and build master dataset. Save the output in individual
   ## parquet files by date
, tar_target(cleaned_region_data, lag_join_aggregate(
    file_list        = file_path_per_date
  , processed_dates = prepped_dates
  , cov_files        = region_data[-length(region_data)]
  , rvf_response    = rvf_response
  , out_dir         = region_cleaned_data_directory
  , overwrite       = FALSE)
  , pattern = map(file_path_per_date)
  , error   = "null"
  , format  = "file")

  ## Build a single master file
, tar_target(joined_region_data, combine_lja(
    in_dir    = cleaned_region_data
  , out_dir   = region_joined_data_directory
  , overwrite = FALSE)
  , error     = "null"
  , format    = "file")

  ## And finally add the sero layer
, tar_target(final_region_data, join_in_sero_layer(
    region_dat = joined_region_data
  , sero_layer = finished_sero_layer
  , overwrite  = TRUE)
  , error   = "null"
  , format  = "file")

)

list(
  data_import_targets
, modeling_targets
, rvf_processing_targets
)
