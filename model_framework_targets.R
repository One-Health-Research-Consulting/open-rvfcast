# This repository uses targets projects
# To switch to the modeling pipeline run:
# Sys.setenv(TAR_PROJECT = "model")

## NOTES / ToDo ----------------------------------------------------------------

## 1) See github issue

## Setup / Preamble ------------------------------------------------------------

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

## Some settings that change much about the pipeline ---------------------------

## Whether to use H3 hex-based spatial aggregation (TRUE) or ADM2 regions (FALSE)
 ## NOTE: Has to match what was used in the previous phase of the pipeline
 ## (rvf_data_processing_targets.R) because the data output from that phase will have
 ## region names that must be matched here. For example, if TRUE in the previous pipeline
 ## which_countries -> region_districts will be an empty list (as there are no country
 ## names in the hex ids)
using_hexes     <- TRUE

## Column name that refers to the subregions. Default if not manually adjusted in
 ## the previous pipeline (rvf_data_processing_targets.R) is "shapeName"
 ## NOTE: code was originally built for using_hexes == FALSE (hex option added later)
 ## so some code was added here and there to add using_hexes == FALSE language to the
 ## object created with using_hexes == TRUE.
 ## The short of it is shapeName is fine for now regardless of the above choice,
 ## however leaving this here for flexibility in case some input layers change. Easier to
 ## adjust here than everywhere in the code
district_id_col <- "shapeName"

## include the background random effect intercept layer (sero unaccounted for by the cases) or not
use_sero_kernel_intercept <- FALSE

## Targets for loading needed data ---------------------------------------------
model_data_targets <- tar_plan(

  ## Eventually will want to download the data from the S3 bucket, but for now load from local
   ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  tar_target(region_name, ifelse(using_hexes, "pan_hex", "pan"))

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

  ## Reduce down to already scaled covariates and scale the other unbounded covariates so that
   ## covariates are roughly on the same scale
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
, tar_target(path_to_simplifed_regions, paste("data/simplified_", region_name, "_sf.Rds", sep = ""))

  ## The main pipeline predicts for all African countries.
   ## Alternatively can just provide a single country to subset predictions to a single region
   ## (the model will still run for all of Africa, but predictions will also be summarized into
   ## ADM2 regions for the chosen country here)
, tar_target(which_countries, "South Africa")

  ## Sub-regions of region[s] of interest (here ADM2 regions are returned)
, tar_target(region_districts, get_region_districts(which_countries))

  ## Load the previously saved spatial hexes.
   ## Saved as part of the rvf_data_processing_targets.R pipeline phase
, tar_target(region_hexes, readRDS("data/region_hexes_small.Rds"))

  ## Load previous saved larger spatial hexes for summarizing output
, tar_target(performance_hexes, readRDS("data/region_hexes_for_evaluation.Rds"))

  ## set up which map object to use given choice for using_hexes
, tar_target(region_map, if (using_hexes) {
    region_hexes
    } else {
    region_districts
  })

  ## Last date of the training data set (all data beyond this date will be set aside for final model evaluation)
   ## NOTE: No 100% principled way to choose this date. This date was chosen to make both training and test
   ## data sets large enough and to have some outbreaks in the test set (fewer recorded outrbeaks very near
   ## the present)
, tar_target(end_date, as.Date("2020-12-19"))

  ## Forecast windows into the future as established in the previous steps of the pipeline
, tar_target(forecast_horizon, c(30, 60, 90, 120, 150))

  ## Establishes a small gap between training windows to have no overlap
, tar_target(max_lag_period, 90)

)

## Targets for preparing for model tuning --------------------------------------
cross_validation_targets <- tar_plan(

  ## Split the data first then fold on the training data.
   ## General NOTE: Name of target as nouns (even if it is a funny nonsense word like it is here)
   ## and the function as the related verb
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
   ## only used if using_hexes == FALSE (all functions in spatial_helpers.R)
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

  ## Quick aside to plot the folded map
, tar_target(plot_spatial_folds, {
     tdat <- clustered_Africa_districts |>
               group_by(cluster) |>
               summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
               sf::st_make_valid()

     ggplot(tdat) +
       geom_sf(aes(fill = factor(cluster)), color = NA) +
       coord_sf(datum = NA) +
       scale_fill_viridis_d(name = "Cluster", option = "C") +
       theme_void() +
       theme(legend.position = "none")
    })

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

## Targets for conducting model tuning -----------------------------------------

## Needed regardless of PURPOSE -- these are also referenced downstream by
 ## model_fitting_targets and model_evaluation_targets, so they stay outside the
 ## PURPOSE-conditional block below
model_tuning_targets_common <- tar_plan(

  ## Set up list of a id columns for grouping, summarizing, etc. that are usde in a few spots
  tar_target(id_cols, c("shapeName", "Proportion_Country", "ADM2", "Proportion_ADM2", "date", "index"))

  ## probability value for which an outbreak is considered "likely"
, tar_target(positive_threshold, seq(0.05, 0.95, by = 0.05))

  ## How much to weight ones (detected outbreaks) relative to zeros (no outbreaks)
, tar_target(weightings_on_ones, c(1, 10, 100, 1000))

  ## Extra weight given to country-level index cases (see get_rvf_response/lag_join_aggregate)
   ## on top of the class-imbalance weight when reporting fitted_model's assessment metrics;
   ## 1 means an index case counts double an ordinary positive case
, tar_target(country_index_boost, 1)

  ## get the baseline occurance of outbreaks (in the full data)
, tar_target(start_p, mean(splitted_data_fitting$train_data[[1]]$outbreak == 1))

, tar_target(outer_folds_dir3, create_data_directory(
  directory_path = paste("outputs/", region_name, "_final_model_fits_ws", sep = "")))

)

## PURPOSE == "train" runs the full two-stage tuning pipeline (global grid search across all
 ## outer/inner folds, then a refined local grid centred on the top global results) and ends by
 ## writing out finalized_hyperparameters. PURPOSE == "forecast" needs none of that -- the branch
 ## below is plain R evaluated once while the plan is being built, so when PURPOSE == "forecast"
 ## none of tuning_grid / tuned_results_per_outer_fold / local_tuning_grid / local_tuned_results
 ## are ever added to the targets graph at all (not merely skipped at run time). Only the target
 ## name finalized_hyperparameters is shared across both branches, since that's the only piece
 ## model_fitting_targets depends on
if (purpose == "train") {

  model_tuning_targets_purpose <- tar_plan(

    ## Model tuning parameters
    tar_target(tune_pars, data.frame(
      tree_min       = 100
    , tree_max       = 1500
    , tree_dep_min   = 4
    , tree_dep_max   = 9
    , learn_rate_min = -2
    , learn_rate_max = -0.52
    , minn_min       = 1
    , minn_max       = 10
    , loss_red_min   = -5
    , loss_red_max   = -0.3
    , mtry_min       = 8
    , size           = 75))

    ## Distinct seed so this grid is an independent draw, not a copy of tuning_grid
  , tar_target(hypergrid_seed, 71982634)

    ## Minimum trees*learn_rate ("boosting capacity") a candidate hyperparameter set must have
     ## to be kept in the search grid. Confirmed empirically that below ~12 on this dataset,
     ## the ensemble never accumulates enough boosting rounds to escape a constant,
     ## input-independent prediction regardless of the other hyperparameters
  , tar_target(min_capacity_for_hypergrid, 20)

    ## Build the "global" (first step) hyperparameter tuning grid
  , tar_target(tuning_grid, build_hyperparameter_grid(
      tune_pars            = tune_pars
    , grid_path            = "data/hypergrid"
    , folded_data_training = folded_data_training
    , splitted_data        = splitted_data
    , overwrite            = FALSE
    , seed                 = hypergrid_seed
    , min_capacity         = min_capacity_for_hypergrid))

    ## Final prep steps for parallel processing for tuning across all inner folds are to
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

  , tar_target(outer_folds_dir, create_data_directory(
      directory_path = paste("outputs/", region_name, "_model_tuning_inner_ws", sep = "")))

    ## DIAGNOSTIC ONLY -- nothing downstream depends on this target. It exists purely so the
     ## (outer_fold_id x chunk_id) partition of inner_fold_id_finalized can be sanity-checked
     ## cheaply. See how pattern = cross(outer_fold_prejoined, chunk_id) in tuned_results_per_outer_fold
     ## below
  , tar_target(inner_fold_ids_per_outer_chunked
    , inner_fold_id_finalized |>
       dplyr::filter(outer_fold_id == folded_data_training$outer_fold_id) |>
       chunk_rows(n_chunks = n_tune_chunks, which = chunk_id) |>
       dplyr::mutate(chunk_id = chunk_id, .after = "outer_fold_id")
     , pattern = cross(folded_data_training, chunk_id))

  , tar_target(hyperparam_path, paste0("outputs/hyperparameters/best_hyperparameters", tuning_grid$grid_id, ".csv"))

  , tar_target(tuned_results_per_outer_fold, tune_results_per_outer_fold(
      prejoined_data  = outer_fold_prejoined
    , inner_ids_all   = inner_fold_id_finalized |>
                          dplyr::filter(outer_fold_id == outer_fold_prejoined$outer_fold_id) |>
                          chunk_rows(n_chunks = n_tune_chunks, which = chunk_id)
    , threshold       = positive_threshold
    , weightings      = weightings_on_ones
    , start_p         = start_p
    , id_cols         = id_cols
    , out_dir         = outer_folds_dir
    , tuning_grid_id  = tuning_grid$grid_id
    , overwrite       = FALSE
    , DEBUG           = FALSE
    , chunk_id        = chunk_id
    , checktime_path  = "outputs/timing"
    , hex_id_col      = district_id_col)
    , pattern         = cross(outer_fold_prejoined, chunk_id)
    , error           = "null"
    , format          = "file")

   ## Set up a target to explore how the hyperparameter set changes as a function of the various dials.
    ## NOTE: see commenting for each individual weighting dial target for details on that parameter
  , tar_target(dial_hyperspace, sobol::sobol_design(
      lower = c(weightval_raw_for_scoring = 10, weightval_hex_for_scoring = 1,
                gamma_for_combined_score = 0, delta_for_index_score = 0)
    , upper = c(weightval_raw_for_scoring = 5000, weightval_hex_for_scoring = 100,
                gamma_for_combined_score = 5, delta_for_index_score = 10)
    , nseq  = 500))

    ## Determine the hyperparameter sets that appear across this full weighting parameter space.
     ## NOTE: used below to determine the dial weights that will be used for the rest of tuning
     ## AND to be used to explore the implications of different choices on actual predicted results
  , tar_target(dial_best_sets, calc_dial_best_set(
      fits            = "outputs/pan_hex_model_tuning_inner_ws" #tuned_results_per_outer_fold
    , dial_hyperspace = dial_hyperspace
    , tuning_grid_id  = tuning_grid$grid_id))

    ## Figure out the single set of weighting dial values to use based on the objective
  , tar_target(chosen_weight_set, chose_weight_set(
      full_set        = dial_best_sets$all_sets
    , summarized_sets = dial_best_sets$summarized_indices
    , objective       = "balanced"))

   ## Penalty weight on S_neg_penalty (raw/global, non-hex), used for the final_score component that
    ## gets folded into final_score_combined via gamma
  , tar_target(weightval_raw_for_scoring, chosen_weight_set$weightval_raw)

  ## Penalty weight on S_neg_penalty_hex. Within hex weight for hexes that have
  ## never experienced an outbreak (~92% of hexes have never had an event)
  , tar_target(weightval_hex_for_scoring, chosen_weight_set$weightval_hex)

  ## Weight on the raw (non-hex) final_score when blending it into final_score_combined =
  ## final_score_hex + gamma * final_score. This exists so a hyperparameter set that
  ## gets the within-hex timing right but is systematically miscalibrated overall
  ## (too high/low everywhere in a given hex) can still be penalized.
  ## Larger gamma puts more weight on the entire raw final_score. That is, a larger gamma pulls
  ## the blended score towards “global” calibration: both better absolute positive-day
  ## confidence and better absolute false-alarm control together
  , tar_target(gamma_for_combined_score, chosen_weight_set$gamma)

  ## Weight on final_score_index (country-level index-case performance, see
  ## get_rvf_response/lag_join_aggregate) when blending it into final_score_combined = final_score_hex +
  ## gamma * final_score + delta * final_score_index. An index case is already counted once as
  ## an ordinary positive in final_score_hex/final_score; delta > 0 makes it count again, so
  ## hyperparameter selection rewards catching those cases
  , tar_target(delta_for_index_score, chosen_weight_set$delta)

    ## Build a refined local grid centered on the top-k global results, ranked by final_score_combined
  , tar_target(local_tuning_grid, build_local_hyperparameter_grid(
      inner_fold_paths     = tuned_results_per_outer_fold
    , global_grid          = tuning_grid
    , tune_pars            = tune_pars
    , top_k                = 8
    , size                 = 75
    , weightval_raw        = weightval_raw_for_scoring
    , weightval_hex        = weightval_hex_for_scoring
    , gamma                = gamma_for_combined_score
    , delta                = delta_for_index_score
    , expansion            = 0.2
    , grid_path            = "data/hypergrid"
    , hyperparam_path      = hyperparam_path
    , folded_data_training = folded_data_training
    , splitted_data        = splitted_data
    , seed                 = hypergrid_seed
    , min_capacity         = min_capacity_for_hypergrid))

  , tar_target(local_hyperparam_path, paste0(
    "outputs/hyperparameters/best_hyperparameters_combined_"
    , tuning_grid$grid_id, "--", local_tuning_grid$grid_id, ".csv"))

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
  , tar_target(outer_fold_prejoined
               , tibble(
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

    ## DIAGNOSTIC ONLY, mirroring inner_fold_ids_per_outer_chunked above -- nothing downstream
     ## depends on this; it's just a cheap way to sanity-check the local grid's partition.
  , tar_target(local_inner_fold_ids_per_outer_chunked
     , local_inner_fold_id_finalized |>
        dplyr::filter(outer_fold_id == folded_data_training$outer_fold_id) |>
        chunk_rows(n_chunks = n_tune_chunks, which = chunk_id) |>
        dplyr::mutate(chunk_id = chunk_id, .after = "outer_fold_id")
     , pattern = cross(folded_data_training, chunk_id))

    ## Reuse the already-computed outer_fold_prejoined branches; only the per-fold
     ## ID slice changes (pointing to local grid indices instead of global ones).
     ## See the matching comment on tuned_results_per_outer_fold above for why the filter/chunk
     ## is done inline rather than via local_inner_fold_ids_per_outer_chunked.
  , tar_target(local_tuned_results, tune_results_per_outer_fold(
       prejoined_data  = outer_fold_prejoined
     , inner_ids_all   = local_inner_fold_id_finalized |>
                            dplyr::filter(outer_fold_id == outer_fold_prejoined$outer_fold_id) |>
                            chunk_rows(n_chunks = n_tune_chunks, which = chunk_id)
     , threshold       = positive_threshold
     , weightings      = weightings_on_ones
     , start_p         = start_p
     , id_cols         = id_cols
     , out_dir         = outer_folds_dir
     , tuning_grid_id  = local_tuning_grid$grid_id
     , overwrite       = FALSE
     , DEBUG           = FALSE
     , chunk_id        = chunk_id
     , checktime_path  = "outputs/timing"
     , hex_id_col      = district_id_col)
     , pattern         = cross(outer_fold_prejoined, chunk_id)
     , error           = "null"
     , format          = "file")

    ## weightval_raw/weightval_hex/gamma/delta are NOT passed here separately -- they are read directly
     ## off local_tuning_grid inside finalize_hyperparameters_from_inner, so
     ## this stage can never silently drift out of sync with whatever scoring parameters actually
     ## built that grid (see the note on build_local_hyperparameter_grid)
  , tar_target(finalized_hyperparameters, finalize_hyperparameters_from_inner(
      inner_folds       = c(tuned_results_per_outer_fold, local_tuned_results)
    , local_tuning_grid = local_tuning_grid
    , tuning_grid_id    = paste(tuning_grid$grid_id, local_tuning_grid$grid_id, sep = "--")
    , outpath           = local_hyperparam_path))

  )

} else {

  ## PURPOSE == "forecast": skip tuning entirely and point finalized_hyperparameters at
   ## whatever hyperparameter set was most recently finalized by a PURPOSE = train run
  model_tuning_targets_purpose <- tar_plan(

    tar_target(finalized_hyperparameters, get_latest_finalized_hyperparameters(
      hyperparam_dir = "outputs/hyperparameters"))

  )

}

model_tuning_targets <- c(model_tuning_targets_common, model_tuning_targets_purpose)

## Fitting of model on holdout data --------------------------------------------
model_fitting_targets <- tar_plan(

  ## Set up what data is referenced depending on the purpose of this run
  tar_target(folded_data_for_fitting, {
    if (purpose == "train") {
      folded_data_testing
    } else {
      folded_data_forecasting
    }
  })

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
  , index_boost     = country_index_boost)
  , pattern         = map(folded_data_for_fitting)
  , error           = "null"
  , format          = "file")

  ## Join fitted_model paths to folded_data_testing for parallel processing for model evaluation
, tar_target(model_out_for_eval, build_model_out_for_eval(
    model_fits = fitted_model
  , full_data  = folded_data_for_fitting))

)

## Asses model performance -----------------------------------------------------
model_evaluation_targets <- tar_plan(

  ## Setup function to extract needed pieces for getting variable importance
   ## to be a bit more RAM efficient (so targets doesn't have to load a bunch of
   ## not needed stuff to run calculate_variable_importance)
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
  , pattern         = map(variable_importance_prep_a))

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
, tar_target(shap_comparison, compare_shap_by_forecast_interval(
    shap_results = shap_by_forecast_interval))

  ## Set up path for app files and examined fits and forecasts
, tar_target(app_file_path, "outputs/for_app")
, tar_target(examined_fits_path, "outputs/examined_fits")
, tar_target(forecasts_path, "outputs/forecasts")

  ## Evaluate fit in a few other ways apart from calibration curves:
   ## A) comparing prob to truth across space and time
   ## B) distributions of predicted probabilities for true ones (places where outbreaks occurred)
   ## C) confusion matrix as a function of different probability cutoffs
  ## NOTE: Just returns predictions for the most recent date if purpose == "forecast"
, tar_target(examined_fits_within_pan, examine_fits_within(
    model_out        = model_out_for_eval
  , test_data        = test_data
  , regions          = region_map
  , larger_districts = performance_hexes
  , africa_sf        = path_to_simplifed_regions
  , region_to_sum    = NULL
  , p_thresh         = positive_threshold
  , using_hexes      = using_hexes
  , outpath          = examined_fits_path
  , outpath_for_for  = forecasts_path
  , outpath_for_app  = app_file_path
  , purpose          = purpose
  , overwrite        = TRUE
    ## Make predictions for a given country
  , country_code     = "ZAF")
  , pattern          = map(model_out_for_eval)
  , error            = "null"
  , format           = "file")

  ## Upload the new fits and the new forecasts to the S3 bucket
, tar_target(model_fits_AWS_upload, AWS_put_files(
    transformed_file_list = fitted_model
  , local_folder          = outer_folds_dir3
  , overwrite             = parse_flag("OVERWRITE_FITTED_MODEL"))
  , error                 = "null")

, tar_target(examined_fits_AWS_upload, {
   if (purpose == "train") {
     AWS_put_files(
       transformed_file_list = examined_fits_within_pan
     , local_folder          = examined_fits_path
     , overwrite             = parse_flag("OVERWRITE_EXAMINED_FITS"))
   } else {
     AWS_put_files(
       transformed_file_list = examined_fits_within_pan
     , local_folder          = forecasts_path
     , overwrite             = parse_flag("OVERWRITE_FORECASTS"))
   }}, error                 = "null")

  ## Similar steps as above but for the chosen country of interest (see target which_countries)
, tar_target(examined_fits_within_country, examine_fits_within(
    model_out        = model_out_for_eval
  , test_data        = test_data
  , regions          = region_map
  , larger_districts = performance_hexes
  , africa_sf        = path_to_simplifed_regions
  , region_to_sum    = region_districts
  , p_thresh         = positive_threshold
  , using_hexes      = using_hexes
  , outpath          = examined_fits_path
  , outpath_for_app  = app_file_path
  , overwrite        = FALSE)
  , pattern          = map(model_out_for_eval)
  , error            = "null"
  , format           = "file")

  ## Export out the pieces needed for the shiny
, tar_target(app_file_needs, {
    all_files <- paste(app_file_path, list.files("outputs/for_app"), sep = "/")
    just_hex_files <- all_files[grepl("FALSE", all_files)]
    just_hex_files
    }
    , format = "file"
  )

  ## Build the data file for the app
, tar_target(built_app_components_predictions, build_app_components_predictions(
    predictions = app_file_needs
  , shapvals    = shap_comparison
  , outpath     = "www/app_data_processed.Rds"
  ), format     = "file")

  ## NOTE: poorly non-dynamic, be careful with this (see inside function)
, tar_target(built_app_components_data, build_app_components_data(
    df_raw  = region_data
  , outpath = "www"
  ), format = "file")

  ## For speed and RAM considerations, extract out pieces for individual exploration as
   ## targets, and can the more easily plot / explore from these extracted pieces
, tar_target(ex_fits.all_probs_raw, {

  filepath <- paste0("outputs/fit_evaluation/ex_fits.all_probs_raw_", Sys.Date(), ".qs")
  if (file.exists(filepath)) {
    filepath
  }

  tf <- purrr::map(examined_fits_within_pan, .f = function(x) {
    qread(x) |> dplyr::select(outer_fold_id, aggregation, all_preds) |> unnest(all_preds)
  }) |>
    bind_rows()

  qsave(tf, filepath)
  filepath
}, error   = "null", format  = "file")
, tar_target(ex_fits.summary_probs_raw, {

  filepath <- paste0("outputs/fit_evaluation/ex_fits.summary_probs_raw_", Sys.Date(), ".qs")
  if (file.exists(filepath)) {
    filepath
  }

    tf <- purrr::map(examined_fits_within_pan, .f = function(x) {
        qread(x) |> dplyr::select(outer_fold_id, aggregation, summary_probs) |> unnest(summary_probs)
      }) |>
      bind_rows()

    qsave(tf, filepath)
    filepath
  }, error   = "null", format  = "file")
, tar_target(ex_fits.summary_probs, {

  filepath <- paste0("outputs/fit_evaluation/ex_fits.summary_probs_", Sys.Date(), ".qs")
  if (file.exists(filepath)) {
    filepath
  }

    tf <- plot.summary_probs(ex_fits.summary_probs_raw)
    qsave(tf, filepath)
    filepath
  }, error   = "null", format  = "file")
, tar_target(ex_fits.plotted_calibration, {

  filepath <- paste0("outputs/fit_evaluation/ex_fits.plotted_calibration_", Sys.Date(), ".qs")

  if (file.exists(filepath)) {
    filepath
  }

    tf <- purrr::map(examined_fits_within_pan, .f = function(x) {
      qread(x) |> dplyr::select(outer_fold_id, aggregation, plotted_calibration) |> unnest(plotted_calibration)
    }) |>
    bind_rows()

    qsave(tf, filepath)
    filepath
  }, error   = "null", format  = "file")
, tar_target(ex_fits.prob_dens_plot, {

  filepath <- paste0("outputs/fit_evaluation/ex_fits.prob_dens_plot_", Sys.Date(), ".qs")

  if (file.exists(filepath)) {
    filepath
  }

  tf <- purrr::map(examined_fits_within_pan, .f = function(x) {
    qread(x) |> dplyr::select(outer_fold_id, aggregation, prob_dens_plot)
  }) |>
  bind_rows()

    qsave(tf, filepath)
    filepath
  }, error   = "null", format  = "file")
, tar_target(ex_fits.map_split, {

  filepath <- paste0("outputs/fit_evaluation/ex_fits.map_split_", Sys.Date(), ".qs")

  if (file.exists(filepath)) {
    filepath
  }

  tf <- purrr::map(examined_fits_within_pan, .f = function(x) {
    qread(x) |> dplyr::select(outer_fold_id, aggregation, date_range, map_split) |> unnest(c(date_range, map_split))
  }) |>
  bind_rows()

    qsave(tf, filepath)
    filepath
  }, error   = "null", format  = "file")

  ## Some targets for saving prediction figures
, tar_target(plotted_calibration.plot_export_opt, save_fig_pieces(
    input     = ex_fits.plotted_calibration
  , outpath   = "reports/figure_pieces/calibration/"
  , evalpath  = "outputs/fit_evaluation/"
  , idinfo    = model_out_for_eval
  , plotname  = "calplot.opt"
  , overwrite = TRUE))
, tar_target(plotted_calibration.plot_export_even, save_fig_pieces(
    input     = ex_fits.plotted_calibration
  , outpath   = "reports/figure_pieces/calibration/"
  , evalpath  = "outputs/fit_evaluation/"
  , idinfo    = model_out_for_eval
  , plotname  = "calplot.even"
  , overwrite = TRUE))
, tar_target(prob_dens.plot_export, save_fig_pieces(
    input     = ex_fits.prob_dens_plot
  , outpath   = "reports/figure_pieces/dens/"
  , evalpath  = "outputs/fit_evaluation/"
  , idinfo    = model_out_for_eval
  , plotname  = "prob_dens_plot"
  , overwrite = TRUE))
, tar_target(map_split.plot_export, save_fig_pieces(
    input     = ex_fits.map_split
  , outpath   = "reports/figure_pieces/map_split/"
  , evalpath  = "outputs/fit_evaluation/"
  , idinfo    = model_out_for_eval
  , plotname  = "map_split"
  , overwrite = TRUE))

)

## Reports ---------------------------------------------------------------------
report_targets <- tar_plan(

  ## Somewhat of a poor, non-dynamic report; need to change path to each and every figure
   ## if code changes which is a bad practice. See figures in report
  tar_quarto(
    openrvfcast_report
  , path  = "reports/openRVFcast_report.qmd"
  , quiet = FALSE)

  ## quick bit of summary info about the model change for the given run
, tar_target(model_note, "Most recent updates: Added index cases, dropped near-term lag sero and
recent outbreak layers")

  ## Timestamped like the other fit_evaluation outputs so each render becomes part of the
   ## performance history rather than overwriting the previous run's report; also lets this
   ## report get swept up by new_performance_files_to_uplaod below (matches on Sys.Date())
, tar_quarto(
    openrvfcast_performance_tracking
  , path        = "outputs/fit_evaluation/openRVFcast_performance_tracking.qmd"
  , output_file = paste0("openRVFcast_performance_tracking_", Sys.Date(), ".html")
  , quiet       = FALSE)

, tar_target(new_performance_files_to_uplaod, {
    outpath   <- "outputs/fit_evaluation"
    filenames <- list.files(outpath)
    filenames <- filenames[grep(Sys.Date(), filenames)]
    paths <- paste0(outpath, "/", filenames)
    paths
  })

, tar_target(performance_files_AWS_upload, AWS_put_files(
    transformed_file_list = new_performance_files_to_uplaod
  , local_folder          = "outputs/fit_evaluation"
  , overwrite             = parse_flag("OVERWRITE_EXAMINED_FITS")))

)

## Targets for verifying a full run of the pipeline has completed --------------
completion_check_targets <- tar_plan(

  ## Top-level target for a run of the pipeline from a bash script
   ## which targets pipeline_complete depends on is determined by purpose
   ## forecast runs stop once the new fits/forecasts are uploaded
   ## training runs require every diagnostic and report and upload of
   ## the performance-tracking outputs
  if (purpose == "train") {
    tar_target(pipeline_complete, {
      invisible(openrvfcast_report)
      invisible(openrvfcast_performance_tracking)
      invisible(performance_files_AWS_upload)
      TRUE
    })
  } else {
    tar_target(pipeline_complete, {
      invisible(examined_fits_AWS_upload)
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
, report_targets
, completion_check_targets
)
