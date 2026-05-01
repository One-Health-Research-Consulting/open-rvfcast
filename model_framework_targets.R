# This repository uses targets projects
# To switch to the modeling pipeline run:
# Sys.setenv(TAR_PROJECT = "model")

## NOTES / ToDo ----------------------------------------------------------------

## 1) Ready to rerun pipeline, but waiting on a machine to run the computation

## Setup / Preamble ------------------------------------------------------------

## Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true"))
  capsule::capshot(c(
    "packages.R"
  , list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE)
  , list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)))

## Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source(f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

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

## Targets for loading needed data ---------------------------------------------
model_data_targets <- tar_plan(

  ## Eventually will want to download the data from the S3 bucket, but for now load from local
   ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  tar_target(region_name, if (using_hexes) {
    "pan_hex"
  } else {
    "pan"
  })
, tar_target(region_data_path
             , paste("data/", region_name, "_joined_response_data/"
             , region_name, "_joined_response_data_final.parquet"
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
, tar_target(region_data, clean_region_data(dat = region_data_raw))

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
   , reduce   = TRUE
   ))

  ## And split data for making predictions on full map once model is tuned
, tar_target(splitted_data_fitting, split_data(
    dat      = region_data
  , end_date = end_date
  , reduce   = FALSE
  ))

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

)

## Targets for conducting model tuning -----------------------------------------
model_tuning_targets <- tar_plan(

  ## Model tuning parameters
  tar_target(tune_pars, data.frame(
    tree_min       = 100
  , tree_max       = 1500
  , tree_dep_min   = 4
  , tree_dep_max   = 10
  , learn_rate_min = 0.01
  , learn_rate_max = 0.5
  , minn_min       = 5
  , minn_max       = 100
  , loss_red_min   = 0
  , loss_red_max   = 0.5
  , mtry_min       = 1
  , mtry_max       = 3
  , size           = 150))

, tar_target(tuning_grid,
    with(tune_pars
      ## Number of alternative grid options available, but space_filling efficient
       ## NOTE: Could possibly do a bit better to save some computation time by
       ## cutting out some of the parameter space where the combination of has some
       ## combination of hyperparameters that don't make a lot of sense
    , grid_space_filling(
        trees(range          = c(tree_min, tree_max))
      , tree_depth(range     = c(tree_dep_min, tree_dep_max))
      , learn_rate(range     = c(learn_rate_min, learn_rate_max), trans = NULL)
      , min_n(range          = c(minn_min, minn_max))
      , loss_reduction(range = c(loss_red_min, loss_red_max))
      ## Arbitrary choice here in which train_inner, doesn't matter which
      , finalize(mtry(), folded_data_training$inner_folds[[10]] |>
                   left_join(
                      splitted_data$train_data[[1]], by = "index") |>
                      filter(cluster != 1))
      ## Total number of combinations of hyperparameters
      , size = size)) |>
      mutate(index = seq_len(n()), .before = 1))

  ## Set up list of a id columns for grouping, summarizing, etc. that are usde in a few spots
, tar_target(id_cols, c("shapeName", "Proportion_Country", "ADM2", "Proportion_ADM2", "date", "index"))

  ## probability value for which an outbreak is considered "likely"
, tar_target(positive_threshold, seq(0.05, 0.95, by = 0.05))

  ## How much to weight ones (detected outbreaks) relative to zeros (no outbreaks)
, tar_target(weightings_on_ones, c(1, 10, 100, 1000))

  ## Set up location for saving intermediate output
, tar_target(outer_folds_dir, create_data_directory(
    directory_path = paste("outputs/", region_name, "_model_tuning_inner_ws", sep = "")))
, tar_target(outer_folds_dir2, create_data_directory(
    directory_path = paste("outputs/", region_name, "_model_tuning_outer_ws", sep = "")))
, tar_target(outer_folds_dir3, create_data_directory(
    directory_path = paste("outputs/", region_name, "_final_model_fits_ws", sep = "")))

  ## Final prep steps for parallel processing for tuning across all inner folds are to
   ## 1) Evaluate which of all of the inner folds across all outer folds actually have
   ## ones (outbreaks) in the assessment set
, tar_target(inner_fold_id, prep_fold_ids(
    folded_data = folded_data_training
  , raw_data    = splitted_data) |>
  cross_join(., tuning_grid) |>
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

  ## Extract a haphazard selection of hyperparameter -by- fold sets for debugging purposes
, tar_target(inner_fold_id_finalized_DEBUG, {
    inner_fold_id_finalized |> filter(
  outer_fold_id == 16 & inner_fold_id == 1  |
  outer_fold_id == 16 & inner_fold_id == 10 |
  outer_fold_id == 18 & inner_fold_id == 5  |
  outer_fold_id == 18 & inner_fold_id == 7  |
  outer_fold_id == 14 & inner_fold_id == 11 |
  outer_fold_id == 5  & inner_fold_id == 13) |>
    filter(index %in% c(15)) |>
    dplyr::select(-assess_inner, -has_outbreak, -nrow)})
, tar_target(folded_data_training_DEBUG, folded_data_training |>
               filter(outer_fold_id %in% inner_fold_id_finalized_DEBUG$outer_fold_id))
, tar_target(folded_data_testing_DEBUG, folded_data_testing |> filter(outer_fold_id %in% c(1, 2, 3)))

  ## Fit across tuning_grid across all inner folds of all outer folds
   ## NOTE: for debugging add _DEBUG after the two instances of inner_fold_id_finalized
, tar_target(tuned_results_per_outer_fold, tune_results_per_outer_fold(
      folded_data = folded_data_training |> dplyr::select(outer_fold_id, inner_folds)
    , inner_ids   = inner_fold_id_finalized_DEBUG
    , raw_data    = splitted_data$train_data[[1]]
    , threshold   = positive_threshold
    , weightings  = weightings_on_ones
    , start_p     = 0.005
    , id_cols     = id_cols
    , out_dir     = outer_folds_dir
    , overwrite   = TRUE
    , DEBUG       = FALSE)
    , pattern     = map(inner_fold_id_finalized_DEBUG)
    , error       = "null"
    , format      = "file")

  ## Join together all tuned inner folds and select the best per outer fold
, tar_target(tuned_results_joined, join_tuned_inner_folds(
    inner_folds  = tuned_results_per_outer_fold
  , training_dat = splitted_data$train_data[[1]]
    ## Choices of how to pick the optimal parameter set include
     ## 'mix' for a balance of pr_auc and logloss or 'binomial'
  , metric       = "mix"
    ## Choices of weighting for 1s vs 0s for either 'mix' or 'binomial'
     ## If for 'mix' can provide anything, if for 'binomial' must be a value
     ## given in the 'weightings' vector given in tune_results_per_outer_fold
    ## In the case of 'mix' the smaller the number (e.g., < 1) causes the hyperparameters
     ## to be more tuned to pr_auc and thus folds with true 1s. The larger the number
     ## the more weight given to logloss (and thus penalizing high probabilities
     ## when there are true 0s). In the case of 'binomial' the larger the number the
     ## more weight given to predicting true 1s with high probability
  , weightval    = 1E-5
    ## Currently both options are to maximize
  , direction    = "max"))

  ## Fit each outer fold with the best inner fold hyperparameter for each of these outer folds
   ## NOTE: for debugging add _DEBUG after the two instances of folded_data_training
, tar_target(tuned_results_across_outer_folds, tune_results_across_outer_folds(
    outer_data     = folded_data_training_DEBUG
  , raw_data       = splitted_data
  , threshold      = positive_threshold
  , hyperparm_sets = tuned_results_joined
  , weightings     = weightings_on_ones
  , start_p        = 0.005
  , id_cols        = id_cols
  , out_dir        = outer_folds_dir2
  , overwrite      = TRUE
  , DEBUG          = FALSE)
  , pattern        = map(folded_data_training_DEBUG)
  , error          = "null"
  , format         = "file")

  ## Extract the best parameter set
, tar_target(finalized_hyperparameters, finalize_hyperparameters(
    outer_folds  = tuned_results_across_outer_folds
  , training_dat = splitted_data$train_data[[1]]
    ## See notes for/in tuned_results_joined
  , metric       = "mix"
  , weightval    = 1E-5
  , direction    = "max"))

)

## Fitting of model on holdout data --------------------------------------------
model_fitting_targets <- tar_plan(

  ## Use the finalized hyperparameters to fit the model for all of the chunks of time that
   ## make up the testing phase
  tar_target(fitted_model, fit_model(
    final_hyper_set = finalized_hyperparameters
  , full_data       = folded_data_testing
  , raw_data        = splitted_data_fitting
  , threshold       = positive_threshold
  , weightings      = weightings_on_ones
  , start_p         = 0.005
  , id_cols         = id_cols
  , out_dir         = outer_folds_dir3
  , overwrite       = TRUE
  , DEBUG           = FALSE)
  , pattern         = map(folded_data_testing)
  , error           = "null"
  , format          = "file")

  ## Join fitted_model paths to folded_data_testing for parallel processing for model evaluation
, tar_target(model_out_for_eval, build_model_out_for_eval(
    model_fits = fitted_model
  , full_data = folded_data_testing))

)

## Asses model performance -----------------------------------------------------
model_evaluation_targets <- tar_plan(

  ## Setup function to extract needed pieces for getting variable importance
   ## to be a bit more RAM efficient (so targets doesn't have to load a bunch of
   ## not needed stuff to run calculate_variable_importance)
  tar_target(variable_importance_prep_a, prep_for_variable_importance_a(
    model_dat     = model_out_for_eval
  , splitted_data = splitted_data)
  , pattern       = map(model_out_for_eval))

  ## Actually do the variable importance calculation, now loading far fewer targets
   ## given variable_importance_prep_a
, tar_target(variable_importance, calculate_variable_importance(
    model_dat       = variable_importance_prep_a
  , final_hyper_set  = finalized_hyperparameters
  , fitdir           = outer_folds_dir3
  , recdir          = outer_folds_dir3
  , num_vars        = 10)
  , pattern         = map(variable_importance_prep_a))

  ## Simple comparison of the shapes of the partial dependence plots among fits
, tar_target(variable_importance_among, compare_vi(variable_importance = variable_importance))

  ## Prep data for SHAP-by-forecast-interval (stratified by shapeName, month, and forecast_interval)
, tar_target(shap_prep_a, prep_for_shap_a(
    model_dat     = model_out_for_eval
  , splitted_data = splitted_data)
  , pattern       = map(model_out_for_eval))

  ## Compute per-row SHAP values and summarize as mean |SHAP| per feature per forecast_interval
, tar_target(shap_by_forecast_interval, calculate_shap_by_forecast_interval(
    model_dat       = shap_prep_a
  , final_hyper_set  = finalized_hyperparameters
  , fitdir           = outer_folds_dir3
  , recdir          = outer_folds_dir3)
  , pattern         = map(shap_prep_a))

  ## Aggregate across outer folds and produce heatmap and line plots
, tar_target(shap_comparison, compare_shap_by_forecast_interval(
    shap_results = shap_by_forecast_interval))

  ## Evaluate fit in a few other ways apart from calibration curves:
   ## A) comparing prob to truth across space and time
   ## B) distributions of predicted probabilities for true ones (places where outbreaks occurred)
   ## C) confusion matrix as a function of different probability cutoffs
, tar_target(examined_fits_within_pan, examine_fits_within(
    model_out        = model_out_for_eval
  , test_data        = splitted_data$test_data[[1]]
  , regions          = region_map
  , larger_districts = performance_hexes
  , africa_sf        = path_to_simplifed_regions
  , region_to_sum    = NULL
  , p_thresh         = positive_threshold
  , using_hexes      = using_hexes
  , outpath          = "outputs/examined_fits"
  , outpath_for_app  = "outputs/for_app"
  , overwrite        = TRUE)
  , pattern          = map(model_out_for_eval)
  , error            = "null"
  , format           = "file")

  ## Similar steps as above but for the chosen country of interest (see target which_countries)
, tar_target(examined_fits_within_country, examine_fits_within(
    model_out        = model_out_for_eval
  , test_data        = splitted_data$test_data[[1]]
  , regions          = region_map
  , larger_districts = performance_hexes
  , africa_sf        = path_to_simplifed_regions
  , region_to_sum    = region_districts
  , p_thresh         = positive_threshold
  , using_hexes      = using_hexes
  , outpath          = "outputs/examined_fits"
  , outpath_for_app  = "outputs/for_app"
  , overwrite        = TRUE)
  , pattern          = map(model_out_for_eval)
  , error            = "null"
  , format           = "file")

  ## Export out the pieces needed for the shiny
, tar_target(app_file_needs, paste("outputs/for_app", list.files("outputs/for_app"), sep = "/"))
, tar_target(built_app_components, build_app_components(
    predictions = app_file_needs
  , shapvals    = shap_comparison
  , outpath     = "www"
  ))

  ## For speed and RAM considerations, extract out pieces for individual exploration as
   ## targets, and can the more easily plot / explore from these extracted pieces
, tar_target(ex_fits.summary_probs_raw, {

  filepath <- "outputs/summarized_fits/ex_fits.summary_probs_raw.qs"
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

  filepath <- "outputs/summarized_fits/ex_fits.summary_probs.qs"
  if (file.exists(filepath)) {
    filepath
  }

    tf <- plot.summary_probs(ex_fits.summary_probs_raw)
    qsave(tf, filepath)
    filepath
  }, error   = "null", format  = "file")
, tar_target(ex_fits.plotted_calibration, {

  filepath <- "outputs/summarized_fits/ex_fits.plotted_calibration.qs"

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

  filepath <- "outputs/summarized_fits/ex_fits.prob_dens_plot.qs"

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

  filepath <- "outputs/summarized_fits/ex_fits.map_split.qs"

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
  , idinfo    = model_out_for_eval
  , plotname  = "calplot.opt"
  , overwrite = TRUE))
, tar_target(plotted_calibration.plot_export_even, save_fig_pieces(
    input     = ex_fits.plotted_calibration
  , outpath   = "reports/figure_pieces/calibration/"
  , idinfo    = model_out_for_eval
  , plotname  = "calplot.even"
  , overwrite = TRUE))
, tar_target(prob_dens.plot_export, save_fig_pieces(
    input     = ex_fits.prob_dens_plot
  , outpath   = "reports/figure_pieces/dens/"
  , idinfo    = model_out_for_eval
  , plotname  = "prob_dens_plot"
  , overwrite = TRUE))
, tar_target(map_split.plot_export, save_fig_pieces(
    input     = ex_fits.map_split
  , outpath   = "reports/figure_pieces/map_split/"
  , idinfo    = model_out_for_eval
  , plotname  = "map_split"
  , overwrite = TRUE))

)

## Reports ---------------------------------------------------------------------
report_targets <- tar_plan(

  ## Somewhat of a poor, non-dynamic report; need to change path to each and every figure
   ## if code changes which is a bad practice. See figures in report
  tar_quarto(
    remit_report
  , path  = "reports/openRVFcast_report.qmd"
  , quiet = FALSE)

)

# List targets -----------------------------------------------------------------
list(
  model_data_targets
, cross_validation_targets
, model_tuning_targets
, model_fitting_targets
, model_evaluation_targets
, report_targets
)
