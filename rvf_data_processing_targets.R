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


## Some settings -----------------------------------------------------------------

## If true uses H3 hexes, if false uses ADM2
using_hexes <- TRUE

## If true, rebuilds all dates regardless of whether or not they exist
rebuild <- TRUE

## reduce outbreaks by looking at resolution of nearby start dates?
 ## NOTE: not supported yet, because no decision has been made on how to reduce
 ## In fact, it may be the case that these are never reduced, instead "index"
 ## cases will be determined and weighted
reduce_outbreaks_by_end_date <- FALSE

## include the background random effect intercept layer (sero unaccounted for by the cases) or not
use_sero_kernel_intercept <- FALSE


## Targets for loading needed data ---------------------------------------------
data_import_targets <- tar_plan(

  ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  tar_target(region_name, ifelse(using_hexes, "pan_hex", "pan"))

  ## Path to the saved full africa data from the previous pipeline step and
  ## set up folders for the "raw" hex combined data and cleaned data
, tar_target(base_predictors_directory, create_data_directory(directory_path = "data/africa_full_predictor_data"))
, tar_target(region_joined_data_directory, create_data_directory(
    directory_path = paste("data/", region_name, "_joined_response_data", sep = "")))
, tar_target(region_data_directory, create_data_directory(
   directory_path = paste("data/", region_name, "_full_response_data", sep = "")))
, tar_target(region_cleaned_data_directory, create_data_directory(
   directory_path = paste("data/", region_name, "_cleaned_response_data", sep = "")))

## Same exact function to get the needed dates as generated in the previous pipeline step
, tar_target(dates_to_process_all, {
  set_model_dates(
    start_year  = 2005
  , end_year    = lubridate::year(Sys.time())
  , n_per_month = 2
  , seed        = 212)
}, cue          = tar_cue("always"))

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
     ## Path to the master parquet that records which dates have already been processed
     final_parquet <- paste0(region_joined_data_directory, "/pan_hex_joined_response_data_final.parquet")
     ## Pull it from S3 if it is not already local so incremental updating is preserved on a
      ## fresh checkout / cleaned data folder; wrapped in try() so a genuine first-ever run (no
      ## AWS creds or no object in the bucket) still falls through to the build-everything path
     try (AWS_get_files(final_parquet), silent = TRUE)
     loaded_region_data <- try(
        read_parquet(final_parquet)
      , silent = TRUE)
      if (class(loaded_region_data)[1] == "try-error") {
        return(tibble(all_dates = character(0) |> as.Date()))
      } else {
        max_date <- max(loaded_region_data$date)
        return(tibble(all_dates = unique(loaded_region_data$date)))
      }
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
      all_dates  = full_parquet_dates
    , file_paths = base_predictor_paths_AWS[-c(1:6)]
    )

  }, cue = tar_cue("always"))

  ## To be extra carefiul, join the dates to make sure they are in the same order
, tar_target(joined_dates, {

   all.d <- left_join(
      africa_data_dates |> rename(dates = all_dates)
    , region_data_dates |> rename(dates = all_dates) |> mutate(processed = 1)
    ) |>
    mutate(processed = ifelse(is.na(processed), 0, 1))

   if (rebuild) all.d <- all.d |> mutate(processed = 0)

   all.d

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

  ## Subset predictor paths to only dates not yet in the response data.
  ## When there are no new files, return the most recently processed path as a non-empty
  ## sentinel so downstream branching targets always receive a non-empty vector; branch
  ## functions use overwrite = FALSE and skip reprocessing of already-completed files.
, tar_target(new_predictor_paths, {

    ## New predictor dates exist when africa_data max is ahead of what's already processed
    file_paths     <- joined_dates |> filter(processed == 0) |> pull(file_paths)
    needs_updating <- length(file_paths) > 0

    if (!needs_updating) {
      ## Sentinel: the last already-processed file; non-empty so branching never errors
      file_paths <- tail(joined_dates$file_paths, 1)
      ## Ensure the sentinel file exists locally so format = "file" targets can hash it;
      ## users may clean the predictor folder to save disk space
      if (!file.exists(file_paths)) AWS_get_files(file_paths)
    }

   file_paths

  }, cue = tar_cue("always"))

  ## Make sure AT LEAST these few files are downloaded for a few of the steps below,
   ## then recheck for the full suite for all needed calculations further down
, tar_target(minimal_date_needs, AWS_get_files(new_predictor_paths))

  ## Polygon of Africa
, tar_target(continent_polygon, create_africa_polygon())

  ## Africa shape object for masking.
  ## Uses ext(continent_polygon) — the same call as continent_raster_template in
  ## predictor_data_processing_targets.R — so both rasters share an identical grid.
, tar_target(wahis_raster_template, terra::rasterize(
    terra::vect(continent_polygon)
  , terra::rast(ext(continent_polygon), resolution = 0.1)) |>
    terra::wrap())

  ## One branch per new file only; format = "file" tracks content changes
, tar_target(base_predictors, new_predictor_paths, format = "file", pattern = map(new_predictor_paths))

  ## WAHIS data outpath
, tar_target(wahis_file_path, "data/WAHIS/outbreak_report_tables.Rds")

  ## Obtain 'raw' outbreak data
, tar_target(wahis_rvf_tables
   , update_wahis_rvf_tables(output_path = wahis_file_path)
   , format = "file"
   , cue    = tar_cue(mode = "always"))

  ## Upload to S3 bucket
, tar_target(wahis_rvf_tables_AWS_upload, AWS_put_files(
    transformed_file_list = wahis_rvf_tables
  , local_folder          = wahis_file_path
  , overwrite             = parse_flag("OVERWRITE_WAHIS"))
  , error                 = "null")

  ## Clean raw outbreak data into form needed for analysis (Step 1)
, tar_target(rvf_outbreaks, clean_rvf_outbreaks(
    output_path = wahis_file_path
  , map_dat     = region_map
  , reduced     = reduce_outbreaks_by_end_date))

  ## Import RVF seroprevalence data
, tar_target(rvf_seroprevalence, readRDS("data/cases_sero.Rds")$sero_data |> mutate(index = seq_len(n())))

  ## Set up directory for cleaned case data
, tar_target(rvf_response_directory, create_data_directory(directory_path = "data/rvf_response"))

  ## Clean raw outbreak data into form needed for analysis (Step 2)
  ## Creates a tibble that contains, for each given dates_to_process
  ## and forecast interval, the outbreaks in the forecast interval duration
  ## after the given dates_to_process
, tar_target(rvf_response,
    get_rvf_response(
      wahis_outbreaks       = rvf_outbreaks
    , wahis_raster_template = wahis_raster_template
    , forecast_intervals    = c(1, 30, 60, 90, 120, 150)
    , dates_to_process      = joined_dates$dates
    , local_folder          = rvf_response_directory
    , reduced               = reduce_outbreaks_by_end_date
    , overwrite             = FALSE)
    , format                = "file"
    , repository            = "local")

  ## Build a neighbor observed-outbreak history term: per hex, per model date, case-weighted
   ## (as a measure of nearby spread pressure) recent outbreak activity in the neighboring hexes
   ## (focal hex excluded) over the same three near-lag windows (1-30, 31-60, 61-90). The aim here
   ## is a `spread` term -- the kernel-smoothed sero layer also encodes neighbor outbreaks but
   ## blends that spread signal with the (opposite-signed) immunity signal. The hope is that this
   ## term helps to deconfound the short term introduction pressure
, tar_target(neighbor_outbreak_history,
    build_neighbor_outbreak_history(
      wahis_outbreaks          = rvf_outbreaks
    , region_map               = region_map
    , path_to_region_neighbors = paste0("data/", region_name, "_region_neighbors.Rds")
    , dates_to_process         = joined_dates$dates
    , lags                     = c(30, 60, 90)
    , case_weight              = TRUE
    , overwrite                = FALSE)
    , format                   = "file"
    , repository               = "local")

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

  ## Build a template for the sero layer
, tar_target(sero_template, {
    template  <- read_parquet(minimal_date_needs[1])
    cross_join(
      region_map[[1]] |> as.data.frame() |> dplyr::select(shapeName)
    , africa_data_dates |> dplyr::select(all_dates) |> rename(date = all_dates)
    )
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

  ## path to where to save some intermediate files for building the sero layer
, tar_target(sero_helper_path, "data/sero_layer_prep")
  
  ## Make predictions from the fitted stan model over a series of targets in order to facilitate parallelization
  ## for this computationally expensive step
  ## *NOTE: the target joined_region_data is built near the end of this targets script*
, tar_target(prepped_pairs, prep_all_pairs(
    sero_cases_dat = cases_sero
  , cov_dat        = sero_template
  , map_dat        = region_map[[1]]
  , outpath        = sero_helper_path
  , overwrite      = FALSE))

  ## adjusted extracted samples from the fitted model
, tar_target(prepped_samps, prep_samps(fitted_stan_model = sero_stan_model, time_adjustment = TRUE))

  ## estimated seroprevalence for prepped_all_dates
, tar_target(built_sero_for_outbreaks, build_sero_for_outbreaks(
    the_pairs     = prepped_pairs
  , the_samps     = prepped_samps
  , use_intercept = use_sero_kernel_intercept)
  , pattern       = map(prepped_pairs))

  ## Sero layer location
, tar_target(sero_path, "data/sero_layer_prep/sero_layer_int")

  ## Pull together the final layer
, tar_target(finished_sero_layer, finish_sero_layer(
    sero_cases_dat = cases_sero
  , samps          = prepped_samps
  , with_outbreaks = built_sero_for_outbreaks
  , use_intercept  = use_sero_kernel_intercept
  , all_dates      = sero_template
  , outpath        = paste0(sero_path, "_", use_sero_kernel_intercept, ".parquet")
  , overwrite      = FALSE)
  , error          = "null"
  , format         = "file")

  ## Upload the layer to the S3 bucket
, tar_target(sero_layer_AWS_upload, AWS_put_files(
    transformed_file_list = finished_sero_layer
  , local_folder          = sero_path
  , overwrite             = parse_flag("OVERWRITE_SERO_LAYER"))
  , error                 = "null")

)

## Build final master dataset for model fitting --------------------------------
 ## A) Masking to the Sub-Region of interest
 ## B) Setting up lagged variables
 ## C) Joining in cases
 ## D) Summarizing covariates and cases to the Sub-Sub-Region of interest
rvf_processing_targets <- tar_plan(

  ## Determine the Country and ADM2 (or 1) region and H3 hex for all of the x, y coordinates
  tar_target(region_data_template, {
    ## No new files to process; preserve the previously-built template in the targets store
    tar_cancel(length(minimal_date_needs) == 0)
    mask_and_cluster_build_template(
      ## Arbitrary which we use as a template, so just grab one that we know we have
       ## from earlier in the pipeline
      cov_files       = minimal_date_needs[1]
    , hex_sf          = region_hexes
    , countries_sf    = region_districts
    , district_id_col = "shapeName"
    , out_dir         = region_data_directory)
  })

## Use the template to categorize all x, y, coordinates for all of the data (by date)
 ## Mask to the Sub-Region of interest (drop nothing if pan-African is desired) and
 ## with Sub-Sub Regions identified
 ## Build smaller more manageable .parquet files composed of the same dates
, tar_target(region_data_minimal_date, mask_and_cluster_from_template(
    template  = region_data_template
    ## First processing step for the new date[s]
  , cov_files = minimal_date_needs
  , out_dir   = region_data_directory
  , overwrite = FALSE)
  , pattern   = map(minimal_date_needs)
  , error     = "null"
  , format    = "file")

  ## First step in building the lagged variables of figuring out what files are needed for each
   ## of the lags. Save these details for each individual date in a list of tibbles
, tar_target(prepped_dates, prep_dates(
    cov_files = region_data_minimal_date
  , dates_all = joined_dates$dates |> as.Date()))

  ## some finicky bs to get arrow to work. For whatever reason will not work unless I pull these out
   ## of the prepped_dates object
   ## Returns NULL if there isn't outbreak data for the given date
  ## targets cannot dynamically branch over a zero-length target ("cannot branch over empty
   ## target"), so when prepped_dates is empty (nothing new to process this cycle) fall back to
   ## a placeholder branch; lag_join_aggregate() detects the corresponding empty processed_dates
   ## and no-ops via error = "null" on the downstream target
, tar_target(file_path_per_date, {
    fp <- lapply(prepped_dates, FUN = function(x) x$filename[1]) |> unlist() |> as.character()
    if (length(fp) == 0) fp <- NA_character_
    fp
  })

  ## Make sure these needed files for the lag dates are downloaded
  ## When prepped_dates is empty (nothing new to process this cycle) bind_rows(prepped_dates)
   ## has no file_nums column to pull(), so short-circuit to the same NA placeholder branch
   ## used by file_path_per_date; mask_and_cluster_from_template() will fail to read path NA and
   ## get dropped from region_data via error = "null" on that downstream target
, tar_target(all_needed_full_africa_files, {
    if (length(prepped_dates) == 0) {
      NA_character_
    } else {
      all_needed_files      <- bind_rows(prepped_dates) |> pull(file_nums) |> unlist() |> unique()
      download_needed_files <- joined_dates[all_needed_files, ]$file_paths
      download_needed_files
    }
  })


  ## Of the files needed for lags, those NOT already processed above via minimal_date_needs
   ## On a full rebuild, minimal_date_needs already spans every date, so this is empty --
   ## collapse to the same NA_character_ sentinel used elsewhere 
, tar_target(remaining_files, {
    rem <- all_needed_full_africa_files[all_needed_full_africa_files %notin% minimal_date_needs]
    if (length(rem) == 0) NA_character_ else rem
  })

  ## Process only the lag-only files region_data_minimal_date did not already cover; 
   ## when remaining_files is the NA sentinel, read_parquet(NA) errors and the 
   ## branch is dropped via error = "null"
, tar_target(region_data_remaining, mask_and_cluster_from_template(
    template  = region_data_template
  , cov_files = remaining_files
  , out_dir   = region_data_directory
  , overwrite = FALSE)
  , pattern   = map(remaining_files)
  , error     = "null"
  , format    = "file")

  ## Full set of processed files needed downstream for lag calculations: the union of this
   ## cycle's newly processed dates and any additional older lag-only files. 
   ## On a full rebuild region_data_remaining is empty and region_data reduces to region_data_minimal_date
, tar_target(region_data, unique(c(region_data_minimal_date, region_data_remaining)), format = "file")

  ## Calculate lags, join cases, summarize and build master dataset. Save the output in individual
   ## parquet files by date
, tar_target(cleaned_region_data, lag_join_aggregate(
    file_list          = file_path_per_date
  , processed_dates    = prepped_dates
  , cov_files          = region_data
  , rvf_response       = rvf_response
  , sero_layer         = finished_sero_layer
  , neighbor_outbreaks = neighbor_outbreak_history
  , out_dir            = region_cleaned_data_directory
  , all_dates          = joined_dates
  , overwrite          = FALSE)
  , pattern            = map(file_path_per_date)
  , error              = "null"
  , format             = "file")

  ## Upload to bucket
, tar_target(cleaned_region_data_AWS_upload, AWS_put_files(
    transformed_file_list = cleaned_region_data
  , local_folder          = region_cleaned_data_directory
  , overwrite             = parse_flag("OVERWRITE_CLEANED_REGION_DATA"))
  , error                 = "null")

  ## Store the name of the existing model data file
, tar_target(model_data_file_name, {
    paste0(
      region_joined_data_directory
    , "/pan_hex_joined_response_data_final_with_sero_int_"
    , use_sero_kernel_intercept
    , ".parquet")
  })

  ## Append new data to existing joined parquet, join sero,
   ## write single _final_with_sero.parquet
, tar_target(final_region_data, combine_lja_and_append(
    new_files     = cleaned_region_data
  , save_filename = model_data_file_name
  , rebuild       = rebuild
  , out_dir       = region_joined_data_directory
  , overwrite     = ifelse(all(is.na(dates_in_predictors)), FALSE, TRUE))
  , error         = "null"
  , format        = "file")

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
