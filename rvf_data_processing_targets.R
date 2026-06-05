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

## Get the "purpose" of the current run (full model 'train' or 'forecast')
purpose <- Sys.getenv("PURPOSE")

## Targets options
source("_targets_settings.R")

## Some settings that change much about the pipeline ---------------------------
using_hexes <- TRUE

## Targets for loading needed data ---------------------------------------------
data_import_targets <- tar_plan(

  ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  tar_target(region_name, ifelse(using_hexes, "pan_hex", "pan"))

  ## Same exact function to get the needed dates as generated in the previous pipeline step
, tar_target(dates_to_process_all, {

    narrow_dates <- set_model_dates(
      start_year  = 2005
    , end_year    = lubridate::year(Sys.time())
    , n_per_month = 2
    , seed        = 212)

    if (purpose == "forecast") {
      narrow_dates <- c(narrow_dates, rollbackward(as_date(floor_date(Sys.Date(), "month") - 2)))

      ## If the last date happens to be the same as the randomly chosen date that month, drop it
      narrow_dates <- unique(narrow_dates)
    }

    narrow_dates

  }, cue         = tar_cue("always"))


  ## Path to the saved full africa data from the previous pipeline step and
  ## set up folders for the "raw" hex combined data and cleaned data
, tar_target(base_predictors_directory, create_data_directory(directory_path = "data/africa_full_predictor_data"))
, tar_target(region_joined_data_directory, create_data_directory(
    directory_path = paste("data/", region_name, "_joined_response_data", sep = "")))
, tar_target(region_data_directory, create_data_directory(
   directory_path = paste("data/", region_name, "_full_response_data", sep = "")))
, tar_target(region_cleaned_data_directory, create_data_directory(
   directory_path = paste("data/", region_name, "_cleaned_response_data", sep = "")))

  ## get all of the file paths to all of the full africa data data in the S3 bucket. Determined
  ## later in the pipeline which are needed
, tar_target(base_predictor_paths_AWS, {
    all_files    <- AWS_get_filenames(base_predictors_directory)
    needed_files <- purrr::map(dates_to_process_all, .f = function(x) {
      all_files[grepl(x, all_files)]
    }) |> unlist()
  }, cue = tar_cue("always"))

   ## Load the already-processed response parquet to determine what dates have been built
 , tar_target(region_data_dates, {

     loaded_region_data <- read_parquet(
       paste0(region_joined_data_directory, "/pan_hex_joined_response_data_final_with_sero.parquet"))
     max_date           <- max(loaded_region_data$date)

     tibble(all_dates = unique(loaded_region_data$date))

   }, cue = tar_cue("always"))

  ## Check the last date available in the files created from the previous pipeline
, tar_target(africa_data_dates, {

    full_parquet_dates <- purrr::map(base_predictor_paths_AWS, .f = function(i) {
       (strsplit(i, "data_")[[1]][2] |> strsplit(".parquet"))[[1]][1]
    }) |>
    unlist() |>
    as.Date() |>
    sort()

    ## Because of how the lags work, need to drop all files in the first three months,
     ## which is the first 6 files
   full_parquet_dates <- full_parquet_dates[-c(1:6)]

    tibble(
      all_dates = full_parquet_dates
    , file_paths = base_predictor_paths_AWS[-c(1:6)]
    )

  }, cue = tar_cue("always"))

  ## To be extra carefiul, join the dates to make sure they are in the same order
, tar_target(joined_dates, {
    left_join(
      africa_data_dates |> rename(dates = all_dates)
    , region_data_dates |> rename(dates = all_dates) |> mutate(processed = 1)
    ) |>
    mutate(processed = ifelse(is.na(processed), 0, 1))
  })

  ## Determine if / what dates need to run
, tar_target(dates_in_predictors, {
    ## New predictor dates exist when africa_data max is ahead of what's already processed
    new_dates      <- joined_dates |> filter(processed == 0) |> pull(dates)
    needs_updating <- ifelse(length(new_dates) > 0, TRUE, FALSE)
    if (needs_updating) {
      needed_dates <- new_dates
    } else {
      needed_dates <- NA
    }

    needed_dates

  }, cue = tar_cue("always"))

  ## Subset predictor paths to only dates not yet in the response data;
  ## empty vector when dates_in_predictors is NA so base_predictors has 0 branches
, tar_target(new_predictor_paths, {

    ## New predictor dates exist when africa_data max is ahead of what's already processed
    file_paths <- joined_dates |>
        filter(processed == 0) |>
        pull(file_paths)
    needs_updating <- ifelse(length(file_paths) > 0, TRUE, FALSE)

    if (needs_updating) {
        needed_paths <- file_paths
    } else {
        file_paths <- character(0)
    }

   file_paths

  }, cue = tar_cue("always"))

  ## Make sure AT LEAST these few files are downloaded for a few of the steps below,
   ## then recheck for the full suite for all needed calculations further down
, tar_target(minimal_date_needs, AWS_get_files(new_predictor_paths))

  ## Polygon of Africa
, tar_target(continent_polygon, create_africa_polygon())

  ## Africa shape object for masking
, tar_target(wahis_raster_template, terra::rasterize(
    terra::vect(continent_polygon)
    ## Mask against a raster filled with 1's
    , terra::rast(
        continent_polygon
        ## Set Resolution
      , resolution = 0.1
      , vals       = 1
      ## Wrap to avoid problems with targets
    )) |>
    terra::wrap())

  ## One branch per new file only; format = "file" tracks content changes
, tar_target(base_predictors, new_predictor_paths, format = "file", pattern = map(new_predictor_paths))

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
, tar_target(rvf_response_directory, create_data_directory(directory_path = "data/rvf_response"))

  ## Creates a tibble that contains, for each given dates_to_process
  ## and forecast interval, the outbreaks in the forecast interval duration
  ## after the given dates_to_process
, tar_target(rvf_response, {
    ## Guard: if no new predictor dates, cancel so the existing parquet is preserved and
    ## downstream targets see no change. get_rvf_response has no overwrite check, so without
    ## this guard it would overwrite the parquet with empty data when dates_to_process = NA.
    tar_cancel(all(is.na(dates_in_predictors)))
    get_rvf_response(
      wahis_outbreaks       = rvf_outbreaks
    , wahis_raster_template = wahis_raster_template
    , forecast_intervals    = c(1, 30, 60, 90, 120, 150)
    , dates_to_process      = joined_dates$dates
    , local_folder          = rvf_response_directory
    , overwrite             = ifelse(all(is.na(dates_in_predictors)), FALSE, TRUE))
  }
  , format                  = "file"
  , repository              = "local")

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
   ## *NOTE: the target joined_region_data is built near the end of this targets script*
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
  , overwrite      = TRUE)
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
    ## Arbitrary which we use as a template, so just grab one that we know we have
     ## from earlier in the pipeline
    cov_files        = minimal_date_needs[1]
  , hex_sf          = region_hexes
  , countries_sf    = region_districts
  , district_id_col = "shapeName"
  , out_dir         = region_data_directory))

## Use the template to categorize all x, y, coordinates for all of the data (by date)
 ## Mask to the Sub-Region of interest (drop nothing if pan-African is desired) and
 ## with Sub-Sub Regions identified
 ## Build smaller more manageable .parquet files composed of the same dates but
, tar_target(region_data_minimal_date, mask_and_cluster_from_template(
    template  = region_data_template
    ## First processing step for the new date[s]
  , cov_files  = minimal_date_needs
  , out_dir   = region_data_directory
  , overwrite = FALSE)
  , pattern   = map(minimal_date_needs)
  , error     = "null"
  , format    = "file")

  ## First step in building the lagged variables of figuring out what files are needed for each
   ## of the lags. Save these details for each individual date in a list of tibbles
, tar_target(prepped_dates, prep_dates(
    cov_files     = region_data_minimal_date
  , rvf_response = rvf_response
  , dates_all    = joined_dates$dates |> as.Date()))

  ## some finicky bs to get arrow to work. For whatever reason will not work unless I pull these out
   ## of the prepped_dates object
   ## Returns NULL if there isn't outbreak data for the given date
, tar_target(file_path_per_date, lapply(prepped_dates, FUN = function(x) x$filename[1]) |> unlist())

  ## Make sure these needed files for the lag dates are downloaded
, tar_target(all_needed_full_africa_files, {
    all_needed_files      <- bind_rows(prepped_dates) |> pull(file_nums) |> unlist() |> unique()
    download_needed_files <- joined_dates[all_needed_files, ]$file_paths
    download_needed_files
  })

  ## process the remaining files needed for cleaning region data given lagged variables
, tar_target(region_data, mask_and_cluster_from_template(
    template  = region_data_template
    ## First processing step for the new date[s]
  , cov_files  = all_needed_full_africa_files
  , out_dir   = region_data_directory
  , overwrite = FALSE)
  , pattern   = map(all_needed_full_africa_files)
  , error     = "null"
  , format    = "file")

  ## Calculate lags, join cases, summarize and build master dataset. Save the output in individual
   ## parquet files by date
, tar_target(cleaned_region_data, lag_join_aggregate(
    file_list       = file_path_per_date
  , processed_dates = prepped_dates
  , cov_files       = region_data
  , rvf_response    = rvf_response
  , out_dir         = region_cleaned_data_directory
  , all_dates       = joined_dates
  , overwrite       = FALSE)
  , pattern         = map(file_path_per_date)
  , error           = "null"
  , format          = "file")

  ## Upload to bucket
, tar_target(cleaned_region_data_AWS_upload, AWS_put_files(
    transformed_file_list = cleaned_region_data
  , local_folder          = region_cleaned_data_directory
  , overwrite             = parse_flag("OVERWRITE_CLEANED_REGION_DATA"))
  , error                 = "null")

  ## Build a single master file
, tar_target(joined_region_data, combine_lja(
    in_dir    = cleaned_region_data
  , out_dir   = region_joined_data_directory
  , overwrite = ifelse(all(is.na(dates_in_predictors)), FALSE, TRUE))
  , error     = "null"
  , format    = "file")

  ## Append new data to existing joined parquet, join sero, write single _final_with_sero.parquet,
  ## and delete the intermediate _final.parquet left by combine_lja
, tar_target(final_region_data, append_with_sero(
    new_files    = cleaned_region_data
  , existing_dat = joined_region_data
  , sero_layer   = finished_sero_layer
  , out_dir      = region_joined_data_directory)
  , error        = "null"
  , format       = "file")

  ## Upload to bucket
, tar_target(final_region_data_AWS_upload, AWS_put_files(
    transformed_file_list = final_region_data
  , local_folder          = region_joined_data_directory
  , overwrite             = parse_flag("OVERWRITE_FINAL_REGION_DATA"))
  , error                 = "null")

)

list(
  data_import_targets
, modeling_targets
, rvf_processing_targets
)
