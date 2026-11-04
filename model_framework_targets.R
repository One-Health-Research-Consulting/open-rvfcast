# This repository uses targets projects
# To switch to the modeling pipeline run:
# Sys.setenv(TAR_PROJECT = "model")

################################################################################
## NOTES / ToDo ----------------------------------------------------------------
################################################################################

## 1) See long github issue tracking progress and To Do list
## 2) Naming convention is to name targets as nouns (even if it is a funny nonsense word)
 ## and the function called for that targets as a verb (the verb creates the noun)

################################################################################
## Setup / Preamble ------------------------------------------------------------
################################################################################

## Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true")) {
  capsule::capshot(c(
    "packages.R"
  , list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE)
  , list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)))
}

## Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source(f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

## Get the "purpose" of the current run (full model 'train' or 'forecast')
purpose <- Sys.getenv("PURPOSE")

if (purpose %notin% c("train", "forecast")) {
  stop("Choose 'train' or 'forecast' for PURPOSE in .env")
}

## Targets options
source("_targets_settings.R")

## Convenience function to format .env flags properly for overwrite parameter and target cues
parse_flag <- function(flags, cue = FALSE) {
  flags <- any(as.logical(Sys.getenv(flags, unset = "FALSE")))
  if (cue) flags <- targets::tar_cue(ifelse(flags, "always", "thorough"))
  flags
}

################################################################################
## Some settings that change much about the pipeline ---------------------------
################################################################################

## Whether to use H3 hex-based spatial aggregation (TRUE) or ADM2 regions (FALSE)
 ## NOTE 1: Most development is with using_hexes == TRUE in mind, so that choice is strongly advised
 ## NOTE 2: Chocie has to match what was used in the previous phase of the pipeline
 ## (rvf_data_processing_targets.R) because the data output from that phase will have
 ## region names that must be matched here. For example, if TRUE in the previous pipeline
 ## which_countries -> region_districts will be an empty list (as there are no country
 ## names in the hex ids)
using_hexes     <- TRUE

## Column name that refers to the subregions. Default if not manually adjusted in
 ## the previous pipeline (rvf_data_processing_targets.R) is "shapeName"
 ## NOTE 1: Early code development was built for using_hexes == FALSE (hex option added later)
 ## so some naming conventions are a bit of a hold-over from that development.
 ## NOTE 2: The short of it is shapeName is fine for now regardless of the above choice,
 ## however leaving this here for flexibility in case some input layers change. Easier to
 ## adjust here than everywhere in the code
district_id_col <- "shapeName"

## include the background random effect intercept layer (sero unaccounted for by the cases) or not
## NOTE 1: More development time spent with FALSE, thus FALSE suggested as default choice
use_sero_kernel_intercept <- FALSE

################################################################################
## Targets for loading needed data ---------------------------------------------
################################################################################
model_data_targets <- tar_plan(

  ## Eventually will want to download the data from the S3 bucket, but for now load from local
   ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  tar_target(region_name, ifelse(using_hexes, "pan_hex", "pan"))

  ## Load cleaned data built from the previous pipeline step
, tar_target(region_data_path
             , paste("data/", region_name, "_joined_response_data/"
             , region_name, "_joined_response_data_final_with_sero_int_"
             , use_sero_kernel_intercept
             , ".parquet"
             , sep = ""), format  = "file")

  ## Load and mask forecast data so that forecasts further out than the summarized
   ## outbreak data are NA
, tar_target(region_data_raw, read_parquet(region_data_path) |>
               ungroup() |>
               mutate(index = seq_len(n()), .before = 1) |>
               mutate(
                 soil_texture = as.numeric(as.factor(soil_texture))
               , soil_drainage = as.numeric(as.factor(soil_drainage))))

  ## Reduce down to already scaled covariates (happens in prior pipeline steps) 
   ## and scale the other unbounded covariates so that covariates are roughly on the same scale
, tar_target(region_data, clean_region_data(
    dat     = region_data_raw
  , map_dat = region_hexes[[1]]))

  ## Get date from which we are forecasting if we are forecasting
, tar_target(forecast_date, max(region_data$date))

  ## Other paths to intermediate products to save computation time.
   ## Most used only for using_hexes == FALSE
, tar_target(path_to_joined_regions, paste("data/joined_", region_name, "_regions.Rds", sep = ""))
, tar_target(path_to_collapsed_regions, paste("data/reduced_", region_name, "_regions.Rds", sep = ""))
, tar_target(path_to_region_neighbors, paste("data/", region_name, "_region_neighbors.Rds", sep = ""))
, tar_target(path_to_clustered_regions, paste("data/clustered_", region_name, "_regions.Rds", sep = ""))

  ## The main pipeline predicts for all African countries.
   ## Can provide a single country here to also save output for a single country
   ## (the model will still run for all of Africa, but predictions will also be summarized into
   ## ADM2 regions for the chosen country here)
, tar_target(which_countries, "South Africa")

  ## Sub-regions of region[s] of interest (here ADM2 regions are returned)
, tar_target(region_districts, get_region_districts(which_countries))

  ## Load the previously saved spatial hexes.
   ## Saved as part of the rvf_data_processing_targets.R pipeline phase
, tar_target(region_hexes, readRDS("data/region_hexes_small.Rds"))

  ## set up which map object to use given choice for using_hexes
, tar_target(region_map, if (using_hexes) region_hexes else region_districts)

  ## Last date of the training data set (all data beyond this date will be set aside for final model evaluation)
   ## NOTE: No 100% principled way to choose this date. This date was chosen to make both training and test
   ## data sets large enough and to have some outbreaks in the test set (fewer recorded outrbeaks very near
   ## the present)
, tar_target(end_date, as.Date("2020-12-19"))

  ## Forecast windows into the future as established in the previous steps of the pipeline
, tar_target(forecast_horizon, c(30, 60, 90, 120, 150))

  ## Establishes a small gap between training windows to have no overlap between forecasted
   ## covariates which only go out to 90 days (120 and 150 forecast horizons also use
   ## the 90 day forecast covariates)
, tar_target(max_lag_period, 90)

)

################################################################################
## Targets for preparing for model tuning --------------------------------------
################################################################################
cross_validation_targets <- tar_plan(

  ## Split the data first then fold on the training data.
  tar_target(splitted_data, split_data(
     dat      = region_data
    ## Option available to leave gaps between the end of one test set time window and the start of the
     ## next if desired. If end_date = end_date the next window will start directly after the previous ends
   , end_date = end_date
     ## Minor reduction in the data set for model fitting speed. Drops map pixels with outlines in terms
      ## of the joint covariate stack where outbreaks have never been recorded
      ## See further details in function
   , reduce   = TRUE))

  ## And split data for making predictions on full map once model is tuned
, tar_target(splitted_data_fitting, split_data(
    dat      = region_data
  , end_date = end_date
  , reduce   = FALSE))

  ## Extract training and test data as standalone targets so workers load only the
   ## slice they need rather than the full splitted_data object
, tar_target(train_data, splitted_data$train_data[[1]])
, tar_target(test_data,  splitted_data$test_data[[1]])

  ## Same extractions for the fitting-phase data (reduce = FALSE, larger than tuning data)
, tar_target(train_data_fitting, splitted_data_fitting$train_data[[1]])
, tar_target(test_data_fitting,  splitted_data_fitting$test_data[[1]])

  ## Number of spatial folds (parameter used in multiple functions)
, tar_target(n_spatial_folds, 20)

  ## Generate n_spatial_folds clusters of all Africa regions
   ## NOTE: Most of the helper functions referenced inside this function are
   ## only used if using_hexes == FALSE (generally not advised)
   ## (all functions in spatial_helpers.R)
   ## If using_hexes == TRUE, this is a pretty simple step
, tar_target(clustered_Africa_districts, make_area_clusters(
     sf_list                   = region_map
   , using_hexes               = using_hexes
   , path_to_joined_regions    = path_to_joined_regions
   , path_to_collapsed_regions = path_to_collapsed_regions
   , path_to_region_neighbors  = path_to_region_neighbors
   , path_to_clustered_regions = path_to_clustered_regions
   , k                         = n_spatial_folds
   , tol                       = 1E-9
   , growth_option             = "balanced"
   , seed                      = 10010
   , overwrite                 = FALSE))

  ## Generate CV folds for training data
, tar_target(folded_data_training_raw, fold_data(
     data              = splitted_data
     ## Two options, train_data or test_data.
      ## train_data sets up inner folds for hyperparameter tuning
      ## test_data just splits testing period into chunks for assessing forecasting accuracy
   , type              = "train_data"
   , sf_districts      = clustered_Africa_districts
     ## Skip through time by the max forecast horizon + max time variables are lagged
   , assess_time_chunk = max(forecast_horizon)
     ## Time gap between the starting date for each temporal fold in the training data.
      ## Setting step_size = max(forecast_horizon) [150] + max_lag_period [90] leads to
      ## no overlap of any data between temporally adjacent folds because the first day of the
      ## 3 months of lagged covariates in fold n+1 start after the last day in the 150 day
      ## forecast window in fold n. Could conceivably let these overlap as it isn't *much*
      ## data overlap, but probably best to keep now overlap
   , step_size         = max(forecast_horizon) + max_lag_period
   , district_id_col   = district_id_col
   , seed              = 10001))

  ## Drop a few folds that don't achieve some minimal data requirements
   ## (too hard to fit and score sensibly when data is too sparse)
, tar_target(folded_data_training, clean_folded_data(
     raw_data                 = splitted_data$train_data
   , folded_data              = folded_data_training_raw
   , epidemic_threshold_total = 10
   , epidemic_threshold_space = 3))

  ## Generate folds for test data for assessing model performance.
   ## NOTE: Splitting into multiple windows to test performance as the amount of data
   ## used to fit the model grows
, tar_target(folded_data_testing, fold_data(
    data              = tibble(test_data = region_data |> list())
  , type              = "test_data"
  , sf_districts      = clustered_Africa_districts
  , assess_time_chunk = max(forecast_horizon)
  , step_size         = max(forecast_horizon)
  , n_spatial_folds   = NULL
  , district_id_col   = district_id_col
  , seed              = 10001
  , holdout_start     = end_date))

  ## Unique folded data for making forecasts from the most recent date.
   ## Separate from the fitting and evaluation pipelines focused just on making predictions
, tar_target(folded_data_forecasting, fold_data(
    data              = tibble(forecasting = region_data |> list())
  , type              = "forecasting"
  , sf_districts      = clustered_Africa_districts
  , assess_time_chunk = max(forecast_horizon)
  , step_size         = max(forecast_horizon)
  , n_spatial_folds   = NULL
  , district_id_col   = district_id_col
  , seed              = 10001
  , current_date      = forecast_date))

)

################################################################################
## Purpose-agnostic targets for conducting model tuning ------------------------
################################################################################
## These targets are needed regardless of PURPOSE; these are referenced downstream
 ## by model_fitting_targets and model_evaluation_targets, so they stay outside the
 ## PURPOSE-conditional block below
model_tuning_targets_common <- tar_plan(

  ## Set up list of a id columns for grouping, summarizing, etc. that are used in a few spots
  tar_target(id_cols, c("shapeName", "Country", "Proportion_Country", "ADM2", "Proportion_ADM2", "date", "index"))

  ## probability value for which an outbreak is considered "likely"
   ## (i.e., what predicted probabilities to pay close attention to)
, tar_target(positive_threshold, seq(0.05, 0.95, by = 0.05))

  ## How much extra weight ones (detected outbreaks) get relative to zeros (no outbreaks)
, tar_target(weightings_on_ones, c(1, 10, 100, 1000))

  ## Extra weight given to country-level index cases (see get_rvf_response/lag_join_aggregate)
   ## on top of the class-imbalance weight when reporting fitted_model's assessment metrics;
   ## 1 means an index case counts double an ordinary positive case (2 is triple, etc.)
, tar_target(country_index_boost, 1)

  ## get the baseline occurrence of outbreaks (in the full data)
, tar_target(start_p, mean(splitted_data_fitting$train_data[[1]]$outbreak == 1))

  ## Interaction constraints used each time a model is fit.
   ## "isolated" is xgboost's default behavior (a listed covariate cant interact with 
   ## *any* other covariate), which is not what we want here, we specifically want to
   ## reduce some largely impactful interactions, which is why we use "free" 
   ## which specifically constrains the covariates in the list to not interact with 
   ## others in the list but allows interactions with all other covariates
, tar_target(interaction_constraints, define_interaction_constraints(
    groups            = list("lat", "lon", "anomaly_not_sero_360", "anomaly_not_sero_720")
  , unlisted_features = "free"))

  ## output directory for saved fits
, tar_target(outer_folds_dir3, create_data_directory(
    directory_path = paste("outputs/", region_name, "_final_model_fits_ws", sep = "")))

)

################################################################################
## Purpose-specific targets for conducting model tuning ------------------------
################################################################################
## PURPOSE == "train" runs the full two-stage tuning pipeline (global grid search 
 ## across all outer/inner folds, then a refined local grid centered on the top 
 ## global results) and ends by writing out finalized_hyperparameters.
## PURPOSE == "forecast" skips all of this, pulling the optimized hyperparameter 
 ## set from the most recent training run to predict cases into the future.
if (purpose == "train") {

model_tuning_targets_purpose <- tar_plan(

  ## Model tuning parameters
  tar_target(tune_pars, data.frame(
    ## natural scale
    tree_min       = 100      
  , tree_max       = 1500
    ## natural scale
  , tree_dep_min   = 4        
  , tree_dep_max   = 9
    ## log10 scale: 0.01 to 0.30
  , learn_rate_min = -2       
  , learn_rate_max = -0.52
    ## natural scale here, but sampled evenly on log10 scale
  , minn_min       = 1        
  , minn_max       = 1000
    ## log10 scale: 0.1 to 1000
  , loss_red_min   = -1       
  , loss_red_max   = 3
    ## natural scale; the maximum is every predictor the model sees
  , mtry_min       = 8        
  , size           = 75))

  ## Distinct seed so this grid is an independent draw, not a copy of tuning_grid
, tar_target(hypergrid_seed, 93822634)

  ## Minimum trees*learn_rate ("boosting capacity") a candidate hyperparameter set 
   ## must have to be kept in the search grid.
, tar_target(min_capacity_for_hypergrid, 2)

  ## Build the "global" hyperparameter tuning grid (first tuning phase)
, tar_target(tuning_grid, build_hyperparameter_grid(
    tune_pars            = tune_pars
  , grid_path            = "data/hypergrid"
  , folded_data_training = folded_data_training
  , splitted_data        = splitted_data
  , overwrite            = FALSE
  , seed                 = hypergrid_seed
  , min_capacity         = min_capacity_for_hypergrid
  , id_cols              = id_cols))

  ## Final prep steps for parallel processing for tuning across all inner folds are to:
   ## 1) Evaluate which of all of the inner folds across all outer folds actually have
   ## ones (outbreaks) in the assessment set
, tar_target(inner_fold_id, prep_fold_ids(
    folded_data = folded_data_training
  , raw_data    = splitted_data) |>
  cross_join(tuning_grid$par_grid[[1]]) |>
  group_by(outer_fold_id) |>
  filter(inner_fold_id %in% unique(inner_fold_id)) |>
  ungroup())

  ## 2) AND which of the outer_fold_ids for ALL of the train_data have at least a single
   ## one in the assess_data. There is no point in wasting computation on inner folds if
   ## the best inner fold hyperparameter set cant be evaluated on the whole training_set
   ## for this outer_fold because there is no outbreak in the assess_data
, tar_target(inner_fold_id_finalized, {
    tfolds <- prep_outer_ids(
      folded_data = folded_data_training
    , raw_data    = splitted_data
    , inner_ids   = inner_fold_id)
  tfolds[sample(nrow(tfolds)), ]})

  ## Path to save all of the individual tuning fits
, tar_target(outer_folds_dir, create_data_directory(
    directory_path = paste("outputs/", region_name, "_model_tuning_inner_ws", sep = "")))

  ## Path to save the hyperparameter set
, tar_target(hyperparam_path, paste0("outputs/hyperparameters/best_hyperparameters", tuning_grid$grid_id, ".csv"))

  ## Do the fitting across all inner -by- outer folds across the "global"
   ## hyperparameter grid
, tar_target(tuned_results_per_outer_fold, tune_results_per_outer_fold(
    prejoined_data = outer_fold_prejoined
  , inner_ids_all  = inner_fold_id_finalized |>
                     dplyr::filter(outer_fold_id == outer_fold_prejoined$outer_fold_id) |>
                     chunk_rows(n_chunks = n_tune_chunks, which = chunk_id)
  , threshold      = positive_threshold
  , weightings     = weightings_on_ones
  , start_p        = start_p
  , id_cols        = id_cols
  , out_dir        = outer_folds_dir
  , tuning_grid_id = tuning_grid$grid_id
  , overwrite      = FALSE
  , DEBUG          = FALSE
  , chunk_id       = chunk_id
  , checktime_path = "outputs/timing"
  , hex_id_col     = district_id_col
  , interaction_constraints = interaction_constraints)
  , pattern        = cross(outer_fold_prejoined, chunk_id)
  , error          = "null"
  , format         = "file")

  ## Hyperparameter selection
   ## Sets are ranked against each other within comparable cells on three components
   ##   pauc: partial ROC AUC over 0-5%; percent of outbreaks in top probability ranks
   ##   within_hex: within-hex AUC, i.e. timing within a hex; a model that only knows which
   ##     hexes are risky scores poorly
   ##   quiet: penalty for false alarms in held-out clusters with no outbreaks, relative to the
   ##     non-outbreak background of outbreak clusters
   ## NOTE: c(0.4, 0.4, 0.2) acts as a reasonable default
, tar_target(selection_weights, c(pauc = 0.4, within_hex = 0.4, quiet = 0.2))

  ## Lower cap on probability for penalizing elevated probabilities in all 0 inner folds
   ## (natural-log ratio of mean predicted probability in quiet clusters to the 
   ## non-outbreak background of outbreak clusters).
, tar_target(selection_quiet_cap, -1)

  ## Two parameters to separate ties; that is, hyperparameter sets that cant really 
   ## be differentiated. See internal code, but in the end, when sets are about equal 
   ## the set that overfits the least is chosen as the best set
, tar_target(selection_near_tie_share, 0.1)
, tar_target(selection_n_boot, 200)

  ## Build a refined local grid centered on the top-k global results, ranked as above
, tar_target(local_tuning_grid, build_local_hyperparameter_grid(
    inner_fold_paths     = tuned_results_per_outer_fold
  , global_grid          = tuning_grid
  , tune_pars            = tune_pars
    ## Separate neighborhoods around the top 2 global sets
  , top_k                = 2
  , size                 = 30
  , selection_weights    = selection_weights
  , quiet_cap            = selection_quiet_cap
    ## Half-widths around each center: trees and mtry as a fraction of the center value,
     ## depth in levels, rest on the log10 scale
  , neighbourhood        = list(
      trees = 0.25, tree_depth = 1, learn_rate_log10 = 0.2
    , min_n_log10 = 0.3, loss_reduction_log10 = 0.5, mtry = 0.25)
  , grid_path            = "data/hypergrid"
  , hyperparam_path      = hyperparam_path
  , folded_data_training = folded_data_training
  , splitted_data        = splitted_data
  , seed                 = hypergrid_seed
  , min_capacity         = min_capacity_for_hypergrid
  , id_cols              = id_cols))

  ## Folds in a digest of the selection settings so each saved set records how it was chosen
, tar_target(local_hyperparam_path, paste0(
    "outputs/hyperparameters/best_hyperparameters_combined_"
  , tuning_grid$grid_id, "--", local_tuning_grid$grid_id
  , "--", digest::digest(list(selection_weights, selection_quiet_cap, selection_near_tie_share, selection_n_boot)), ".csv"))

  ## Structural (pre-calibration) hyperparameter path
, tar_target(structural_hyperparam_path, paste0(
    "outputs/hyperparameters/best_hyperparameters_structural_"
  , tuning_grid$grid_id, "--", local_tuning_grid$grid_id
  , "--", digest::digest(list(selection_weights, selection_quiet_cap, selection_near_tie_share, selection_n_boot)), ".csv"))

  ## (outer x inner x local-index) combinations, shuffled for load balancing.
   ## Mirrors inner_fold_id_finalized but cross-joined with the local grid.
, tar_target(local_inner_fold_id_finalized, {
    base   <- prep_fold_ids(folded_data = folded_data_training, raw_data = splitted_data) |>
      cross_join(local_tuning_grid$par_grid[[1]])
    tfolds <- prep_outer_ids(
      folded_data = folded_data_training
    , raw_data    = splitted_data
    , inner_ids   = base)
    tfolds[sample(nrow(tfolds)), ]
  })

  ## Pre-join inner fold indices with training covariates, one branch per outer fold.
   ## Workers for tuned_results_per_outer_fold load this small per-fold slice rather than
   ## the full train_data, and the join is computed once per fold instead of once per
   ## (inner_fold x tune_grid) branch.
, tar_target(outer_fold_prejoined, tibble(
    outer_fold_id = folded_data_training$outer_fold_id
  , data          = list(
     folded_data_training$inner_folds[[1]] |>
      left_join(train_data, by = "index")))
  , pattern = map(folded_data_training))

  ## Number of pieces to split each outer fold's (inner_fold x tune_grid).
   ## Use more workers: total tuning branches = nrow(folded_data_training) * n_tune_chunks
, tar_target(n_tune_chunks, max(1L, ceiling(as.integer(30) / nrow(folded_data_training))))

  ## Chunk index branching dimension, crossed against outer_fold_prejoined below
, tar_target(chunk_id, seq_len(n_tune_chunks))

  ## Reuse the already-computed outer_fold_prejoined branches; only the per-fold
   ## ID slice changes (pointing to local grid indices instead of global ones).
   ## See the matching comment on tuned_results_per_outer_fold above for why the filter/chunk
   ## is done inline rather than via local_inner_fold_ids_per_outer_chunked.
, tar_target(local_tuned_results, tune_results_per_outer_fold(
    prejoined_data = outer_fold_prejoined
  , inner_ids_all  = local_inner_fold_id_finalized |>
                       dplyr::filter(outer_fold_id == outer_fold_prejoined$outer_fold_id) |>
                       chunk_rows(n_chunks = n_tune_chunks, which = chunk_id)
  , threshold      = positive_threshold
  , weightings     = weightings_on_ones
  , start_p        = start_p
  , id_cols        = id_cols
  , out_dir        = outer_folds_dir
  , tuning_grid_id = local_tuning_grid$grid_id
  , overwrite      = FALSE
  , DEBUG          = FALSE
  , chunk_id       = chunk_id
  , checktime_path = "outputs/timing"
  , hex_id_col     = district_id_col
  , interaction_constraints = interaction_constraints)
  , pattern        = cross(outer_fold_prejoined, chunk_id)
  , error          = "null"
  , format         = "file")

  ## Get the finalized hyperparameter set prior to any re-calibration, ranking the pooled
   ## global + local candidates (the full ranking is saved beside it as *_selection_table.csv)
, tar_target(finalized_hyperparameters_structural, finalize_hyperparameters_from_inner(
    inner_folds       = c(tuned_results_per_outer_fold, local_tuned_results)
  , selection_weights = selection_weights
  , quiet_cap         = selection_quiet_cap
  , near_tie_share    = selection_near_tie_share
  , n_boot            = selection_n_boot
  , seed              = hypergrid_seed
  , tuning_grid_id    = paste(tuning_grid$grid_id, local_tuning_grid$grid_id, sep = "--")
  , outpath           = structural_hyperparam_path))

  ## Probability calibration ---------------------------------------------------
   ## Step 1: refit the winning set once per outer fold on that fold's full training
   ## window (same interaction constraints as every other fit) and save ("harvest") 
   ## its held-out predictions. One branch per outer fold so the fits run in parallel.
, tar_target(calibration_harvest, harvest_outer_fold_predictions(
    winning_hyperparam_path = finalized_hyperparameters_structural
  , outer_fold_row          = folded_data_training
  , full_data               = train_data
  , start_p                 = start_p
  , id_cols                 = id_cols
  , interaction_constraints = interaction_constraints
  , hex_id_col              = district_id_col
  , out_dir                 = "outputs/calibration_harvest"
  , overwrite               = FALSE)
  , pattern                 = map(folded_data_training)
  , format                  = "file")

  ## Step 2: fit the calibration on the pooled harvest by maximum likelihood (one 
   ## intercept per forecast window, one shared slope on the log-odds. Also 
   ## cross-fit across outer folds and save diagnostics (observed vs expected by window,
   ## fold and year) to out_dir
, tar_target(probability_calibration_result, fit_probability_calibration_on_outer_folds(
    harvest_files = calibration_harvest
  , out_dir       = "outputs/calibration_harvest"))

  ## Step 3: Build the final calibrated hyperparameter set (calibration coefficients 
   ## added as calib_* columns)
, tar_target(finalized_hyperparameters, write_calibrated_hyperparameters(
    probability_calibration_result = probability_calibration_result
  , structural_hyperparam_path     = finalized_hyperparameters_structural
  , outpath                        = local_hyperparam_path)
  , format                         = "file")

)

## PURPOSE == forecast
} else {

  ## PURPOSE == "forecast": skip tuning entirely and point finalized_hyperparameters at
   ## whatever hyperparameter set was most recently finalized by a PURPOSE = train run
   ## (the calib_* calibration coefficients live as columns on that same file, so nothing further to look up)
model_tuning_targets_purpose <- tar_plan(
  tar_target(finalized_hyperparameters, get_latest_finalized_hyperparameters(hyperparam_dir = "outputs/hyperparameters"))
)

}

## Pull together the two tar_plans for pipeline run tracking/success
model_tuning_targets <- c(model_tuning_targets_common, model_tuning_targets_purpose)

################################################################################
## Fitting of model on holdout data --------------------------------------------
################################################################################
model_fitting_targets <- tar_plan(

  ## Set up what data is referenced depending on the purpose of this run
  tar_target(folded_data_for_fitting, if (purpose == "train") folded_data_testing else folded_data_forecasting)
      
  ## Use the finalized hyperparameters to fit the model for all of the chunks of time that
   ## make up the testing phase
, tar_target(fitted_model, fit_model(
    final_hyper_set = finalized_hyperparameters
  , full_data       = folded_data_for_fitting
  , train_data      = train_data_fitting
  , test_data       = test_data_fitting
  , threshold       = positive_threshold
  , weightings      = weightings_on_ones
  , start_p         = start_p
  , id_cols         = id_cols
  , out_dir         = outer_folds_dir3
  , overwrite       = FALSE
  , DEBUG           = FALSE
  , index_boost     = country_index_boost
  , interaction_constraints = interaction_constraints)
  , pattern         = map(folded_data_for_fitting)
  , error           = "null"
  , format          = "file")

  ## Join fitted_model paths to folded_data_testing for parallel processing for model evaluation
, tar_target(model_out_for_eval, build_model_out_for_eval(
    model_fits = fitted_model
  , full_data  = folded_data_for_fitting))

)

################################################################################
## Asses model performance -----------------------------------------------------
################################################################################
model_evaluation_targets_common <- tar_plan(

  ## Setup function to extract needed pieces for getting variable importance
   ## to be a bit more RAM efficient (so targets doesn't have to load a bunch of
   ## unneeded stuff to run calculate_variable_importance)
  tar_target(variable_importance_prep_a, prep_for_variable_importance_a(
    model_dat  = model_out_for_eval
  , train_data = train_data
  , test_data  = test_data)
  , pattern    = map(model_out_for_eval))

  ## Actually do the variable importance calculation, now loading far fewer targets
   ## given variable_importance_prep_a
, tar_target(variable_importance, calculate_variable_importance(
    model_dat       = variable_importance_prep_a
  , final_hyper_set = finalized_hyperparameters
  , fitted_model    = fitted_model
  , fitdir          = outer_folds_dir3
  , recdir          = outer_folds_dir3
  , num_vars        = 10)
  , pattern         = map(variable_importance_prep_a)
  , error           = "null")

  ## Simple comparison of the shapes of the partial dependence plots among fits
, tar_target(variable_importance_among, compare_vi(variable_importance = variable_importance))

  ## Prep data for SHAP-by-forecast-interval (stratified by shapeName, month, and forecast_interval)
, tar_target(shap_prep_a, prep_for_shap_a(
    model_dat  = model_out_for_eval
  , train_data = train_data
  , test_data  = test_data)
  , pattern    = map(model_out_for_eval))

  ## Compute per-row SHAP values and summarize as mean |SHAP| per feature per forecast_interval
, tar_target(shap_by_forecast_interval, calculate_shap_by_forecast_interval(
    model_dat       = shap_prep_a
  , final_hyper_set = finalized_hyperparameters
  , fitted_model    = fitted_model
  , fitdir          = outer_folds_dir3
  , recdir          = outer_folds_dir3)
  , pattern         = map(shap_prep_a)
  , error           = "null")

  ## Aggregate across outer folds and produce heatmap and line plots
, tar_target(shap_comparison, compare_shap_by_forecast_interval(shap_results = shap_by_forecast_interval))

  ## Folders for the saved hex-level predictions (train) and forecast exports (forecast)
, tar_target(examined_fits_path, "outputs/examined_fits")
, tar_target(forecasts_path, "outputs/forecasts")

  ## Country given its own forecast json in PURPOSE = forecast runs
, tar_target(forecast_country_code, "ZAF")

  ## PURPOSE = train: save each fitted model's hex-level predictions for run_comprehensive_performance()
   ## PURPOSE = forecast: export the forecasts for the most recent date as json (Africa and forecast_country_code)
, tar_target(examined_fits, examine_fits_within(
    model_out        = model_out_for_eval
  , test_data        = test_data
  , regions          = region_map
  , outpath          = examined_fits_path
  , outpath_for_for  = forecasts_path
  , purpose          = purpose
  , country_code     = forecast_country_code
  , overwrite        = TRUE)
  , pattern          = map(model_out_for_eval)
  , error            = "null"
  , format           = "file")

  ## Outbreaks placed in the hexes they are labelled in (saved by the rvf pipeline's
   ## hexed_outbreaks target); links predictions to the outbreaks behind them
, tar_target(hexed_outbreaks_path, "data/rvf_outbreaks_hexed.Rds", format = "file")

)

if (purpose == "train") {

model_evaluation_targets_purpose <- tar_plan(

  ## Extract and save all fitted models' hex-level predictions in one file, which
   ## is needed as the input to run_comprehensive_performance()
  tar_target(ex_fits.all_probs_raw, {
    filepath <- paste0("outputs/fit_evaluation/ex_fits.all_probs_raw_", Sys.Date(), ".qs")
    purrr::map(examined_fits, qread) |>
      bind_rows() |>
      qsave(filepath)
    filepath
  }, error = "null", format = "file")

  ## Larger regions (aggregated ADM2) that hex predictions are up-scaled to; any file in
   ## data/upscaling_regions works (e.g., region_hexes_for_evaluation.Rds for larger hexes)
, tar_target(performance_regions_path, "data/upscaling_regions/reduced_pan_regions_target60000km2.Rds", format = "file")

  ## Comprehensive performance evaluation:
   ## hex-level forecast scores, rare-event verification against climatology and a historical
   ## reference, outbreak-focused (episode) skill, copula up-scaling to regions, and
   ## spatial / ENSO / temporal / model-structure diagnostics. 
   ## Scores the test predictions and the calibration-harvest (outer-fold) predictions
   ## writes tables, figures, data objects and a report to outputs/comprehensive_performance/<date>
   ## and saves the mask context 
   ## used by PURPOSE = forecast runs to outputs/forecast_context
, tar_target(comprehensive_performance, run_comprehensive_performance(
    predictions_path     = ex_fits.all_probs_raw
  , hexed_outbreaks_path = hexed_outbreaks_path
  , train_data           = train_data
  , test_data            = test_data
  , region_hexes         = region_hexes
  , forecast_horizon     = forecast_horizon
  , regions_path         = performance_regions_path
  , harvest_dir          = "outputs/calibration_harvest"
  , fitted_model         = fitted_model
  , variable_importance  = variable_importance
  , chosen_purpose_tag   = "for_test_data"
  , context_dir          = "outputs/forecast_context")
  , error                = "null"
  , format               = "file")

  ## Upload the comprehensive performance folder (report, figures, tables, data) and the
   ## saved forecast context to the S3 bucket
, tar_target(comprehensive_performance_AWS_upload, AWS_put_files(
    transformed_file_list = c(
      list.files(comprehensive_performance, recursive = TRUE, full.names = TRUE)
    , list.files("outputs/forecast_context", full.names = TRUE))
  , local_folder          = comprehensive_performance
  , overwrite             = parse_flag("OVERWRITE_PERFORMANCE"))
  , error                 = "null")

)

## PURPOSE == forecast
} else {

model_evaluation_targets_purpose <- tar_plan(

  ## Hex and regional maps of this run's forward forecasts, masked by the context (and
   ## regional copula fit) saved by the most recent PURPOSE = train run
  tar_target(forecast_context_maps, run_forecast_context_maps(
    forecast_files       = examined_fits
  , region_hexes         = region_hexes
  , hexed_outbreaks_path = hexed_outbreaks_path
  , forecast_horizon     = forecast_horizon
  , context_dir          = "outputs/forecast_context"
  , out_root             = "outputs/forecasts/context_maps")
  , error                = "null")

  ## Data for the forecast explorer apps (www/forecast_explorer_hex.R and _regions.R)
   ## this run's forecasts with the forecast quality and outbreak history saved by 
   ## the latest training run
, tar_target(forecast_app_data, build_forecast_app_data(
    forecast_files       = examined_fits
  , region_hexes         = region_hexes
  , hexed_outbreaks_path = hexed_outbreaks_path
  , context_dir          = "outputs/forecast_context"
  , forecast_horizon     = forecast_horizon
  , outpath              = "www")
  , error                = "null"
  , format               = "file")

  ## Upload the forecast json files, the forecast maps and the app data to the S3 bucket
   ## (the deployed apps read www/forecast_app_*.rds from the bucket)
, tar_target(forecasts_AWS_upload, AWS_put_files(
    transformed_file_list = c(
      examined_fits
    , if (!is.null(forecast_context_maps)) list.files(
        forecast_context_maps
      , recursive = TRUE
      , full.names = TRUE
      ), forecast_app_data)
  , local_folder          = forecasts_path
  , overwrite             = parse_flag("OVERWRITE_FORECASTS"))
  , error                 = "null")

  )

}

## Pull together the two tar_plans for pipeline run tracking/success
model_evaluation_targets <- c(model_evaluation_targets_common, model_evaluation_targets_purpose)

## Targets for verifying a full run of the pipeline has completed --------------
completion_check_targets <- tar_plan(

  ## Top-level target for a run of the pipeline from a bash script
   ## which targets pipeline_complete depends on is determined by purpose
   ## forecast runs stop once the forecasts and forecast maps are uploaded
   ## training runs stop once the comprehensive performance folder is uploaded
  if (purpose == "train") {
    tar_target(pipeline_complete, {
      invisible(comprehensive_performance_AWS_upload)
      TRUE
    })
  } else {
    tar_target(pipeline_complete, {
      invisible(forecasts_AWS_upload)
      TRUE
    })
  }

)

# List targets -----------------------------------------------------------------
list(
  model_data_targets
, cross_validation_targets
, model_tuning_targets
, model_fitting_targets
, model_evaluation_targets
, completion_check_targets)
