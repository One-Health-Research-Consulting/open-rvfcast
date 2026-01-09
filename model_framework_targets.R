# This repository uses targets projects
# To switch to the modeling pipeline run:
# Sys.setenv(TAR_PROJECT = "model")

## NOTES / ToDo ----------------------------------------------------------------

## 1) Implement some computation speedup choices
## 2) Finalize covariate selection and model definition
## 3) Get up and running on the server

## Setup / Preamble ------------------------------------------------------------

## Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true"))
  capsule::capshot(c("packages.R",
                     list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE),
                     list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)))

## Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

## Targets options
source("_targets_settings.R")

## Convenience function to format .env flags properly for overwrite parameter and target cues
parse_flag <- function(flags, cue = F) {
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
 ## object created with using_hexes == TRUE. So currently, not fully dynamic. The
 ## short of it is shapeName is fine for now regardless of the above choice
district_id_col <- "shapeName"

## Targets for loading needed data ---------------------------------------------
model_data_targets <- tar_plan(

  ## Eventually will want to download the data from the S3 bucket, but for now load from local
   ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  tar_target(region_name, if(using_hexes){"pan_hex"}else{"pan"})
, tar_target(region_data_path
             , paste("data/", region_name, "_joined_response_data/"
             , region_name, "_joined_response_data_final_with_sero.parquet"
             , sep = ""))

  ## Load and mask forecast data so that forecasts further out than the summarized
   ## outbreak data are NA
, tar_target(region_data_raw, read_parquet(region_data_path) %>% 
               ungroup() %>% mutate(index = seq(n()), .before = 1))

  ## Reduce down to already scaled covariates and scale the other unbounded covariates so that
   ## covariates are roughly on the same scale
, tar_target(region_data, clean_region_data(dat = region_data_raw))

  ## Other paths to intermediate products to save computation time. 
   ## Most used only for using_hexes == FALSE
, tar_target(path_to_joined_regions   , paste("data/joined_"    , region_name, "_regions.Rds", sep = ""))
, tar_target(path_to_collapsed_regions, paste("data/reduced_"   , region_name, "_regions.Rds", sep = ""))
, tar_target(path_to_region_neighbors , paste("data/"           , region_name, "_region_neighbors.Rds", sep = ""))
, tar_target(path_to_clustered_regions, paste("data/clustered_" , region_name, "_regions.Rds", sep = ""))
, tar_target(path_to_simplifed_regions, paste("data/simplified_", region_name, "_sf.Rds", sep = ""))

  ## Pulls all African countries. Alternatively can just provide a single country
   ## directly to get_region_districts below
, tar_target(which_countries, unique(region_data$Country))

  ## Sub-regions of region[s] of interest
, tar_target(region_districts, get_region_districts(which_countries))

  ## Load the previously saved spatial hexes. 
   ## Saved as part of the rvf_data_processing_targets.R pipeline phase
, tar_target(region_hexes, readRDS("data/region_hexes.Rds"))

  ## set up which map object to use given choice for using_hexes
, tar_target(region_map, if(using_hexes){region_hexes}else{region_districts})

  ## Last date of the training data set (all data beyond this date will be set aside for final model evaluation)
, tar_target(end_date, as.Date("2020-12-19"))

  ## As in the comment in the preamble, testing my mental map of the problem and working on code dev for
   ## one forecast horizon for now
, tar_target(forecast_horizon, c(30, 60, 90, 120, 150))
, tar_target(max_lag_period, 90)
  
)

## Targets for preparing for model tuning --------------------------------------
cross_validation_targets <- tar_plan(
  
  ## Best to split the data first then fold on the training data. Can do so on end_date
   ## Going for name of target as a noun (even if it is a funny nonsense word like it is here)
   ## and the function as the related verb
  tar_target(splitted_data, split_data(
     dat      = region_data
    ## Prevent overlap in training and test, so start test after the end of the forecast horizon 
     ## from the last training date
   , end_date = end_date
     ## If we want to reduce the dataset for model fitting speed. Uncertain about this
      ## Details in function (could potentially be pulled out for greater transparency)
   , reduce   = TRUE
   ))
  
  ## And split data for fitting (no reduction)
, tar_target(splitted_data_fitting, split_data(
    dat      = region_data
  , end_date = end_date
  , reduce   = FALSE
  ))
  
  ## Number of spatial folds (parameter used in multiple functions)
, tar_target(n_spatial_folds, 20)

  ## Generate n_spatial_folds clusters of all Africa regions
   ## Note: all functions in spatial_helpers.R
  ## Slightly unwieldy because many of these steps are not needed if using_hexes
  ## but functioning fine, so leaving for now
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
   , overwrite                 = TRUE))

  ## Quick aside to plot the folded map
, tar_target(plot_spatial_folds, clustered_Africa_districts %>% 
               group_by(cluster) %>%
               summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
               sf::st_make_valid() %>% {
                   ggplot(.) +
                   geom_sf(aes(fill = factor(cluster)), color = NA) +
                   coord_sf(datum = NA) +
                   scale_fill_viridis_d(name = "Cluster", option = "C") +
                   theme_void() +
                   theme(legend.position = "none")})
  
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
     ## Time gap between the end of the previous fold and the start of the next fold. For now setting to
      ## the max forecast time so that there isn't overlap
   , step_size         = max(forecast_horizon) + max_lag_period
   , district_id_col   = district_id_col
   , seed              = 10001))
    
  ## Collapse these based on some criteria of "information content" 
, tar_target(folded_data_training, clean_folded_data(
     raw_data                 = splitted_data$train_data
   , folded_data              = folded_data_training_raw
   , epidemic_threshold_total = 10
   , epidemic_threshold_space = 3))

 ## Generate test cases for assessing model performance
, tar_target(folded_data_testing, fold_data(
    data              = tibble(test_data = region_data %>% list())
  , type              = "test_data"
  , sf_districts      = clustered_Africa_districts
  , assess_time_chunk = max(forecast_horizon)
  , step_size         = max(forecast_horizon) + max_lag_period
  , n_spatial_folds   = NULL
  , district_id_col   = district_id_col
  , seed              = 10001
  , holdout_start     = end_date))

)

## Targets for conducting model tuning -----------------------------------------
model_tuning_targets <- tar_plan(
  
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
  , size           = 50))
  
, tar_target(tuning_grid,
    with(tune_pars
      ## Number of alternative grid options available, this seems fine
       ## Possible that some of the parameter space has some combination of 
       ## hyperparameters that don't make a lot of sense
    , grid_space_filling(
        trees(range          = c(tree_min, tree_max))
      , tree_depth(range     = c(tree_dep_min, tree_dep_max))
      , learn_rate(range     = c(learn_rate_min, learn_rate_max), trans = NULL)
      , min_n(range          = c(minn_min, minn_max))
      , loss_reduction(range = c(loss_red_min, loss_red_max))
      ## Arbitrary choice here in which train_inner, shouldn't really matter
      , finalize(mtry()      , folded_data_training$inner_folds[[10]] %>% 
                   left_join(., splitted_data$train_data[[1]], by = "index") %>% filter(cluster != 1))
      ## Total number of combinations of hyperparameters
      , size = size)) %>% mutate(index = seq(n()), .before = 1))

, tar_target(id_cols, c("shapeName", "Country", "date", "index"))

  ## probability value over which a one is assigned
, tar_target(positive_threshold, seq(0.05, 0.95, by = 0.05))
  ## How much to weight 1s relative to 0s in predictions
, tar_target(weightings_on_ones, c(1, 10, 100, 1000))

  ## Set up location for saving intermediate output
, tar_target(outer_folds_dir, create_data_directory(
    directory_path = paste("outputs/", region_name, "_model_tuning_inner", sep = "")))
, tar_target(outer_folds_dir2, create_data_directory(
    directory_path = paste("outputs/", region_name, "_model_tuning_outer", sep = "")))
, tar_target(outer_folds_dir3, create_data_directory(
    directory_path = paste("outputs/", region_name, "_final_model_fits", sep = "")))

## Final prep step for parallel processing for tuning across all inner folds is to
 ## evaluate which of all of the inner folds across all outer folds actually have
 ## 1s in the assessment set
, tar_target(inner_fold_id, prep_fold_ids(
    folded_data = folded_data_training
  , raw_data    = splitted_data
) %>% cross_join(., tuning_grid) %>% 
  group_by(outer_fold_id) %>% 
  filter(inner_fold_id %in% unique(inner_fold_id)) %>% 
  ungroup())

## AND which of the outer_fold_ids for ALL of the train_data have at least a single
## 1 in the assess_data. There is no point in wasting computation on inner folds if
## the best inner fold hyperparameter set cant be evaluated on the whole training_set
## for this outer_fold because there is no outbreak in the assess_data
, tar_target(inner_fold_id_finalized, prep_outer_ids(
    folded_data = folded_data_training
  , raw_data    = splitted_data
  , inner_ids   = inner_fold_id))

  ## NOTE: temp check for debugging purposes
, tar_target(inner_fold_id_finalized_DEBUG, {
    inner_fold_id_finalized %>% filter(
  outer_fold_id == 16 & inner_fold_id == 1 |
  outer_fold_id == 16 & inner_fold_id == 10 |   
  outer_fold_id == 18 & inner_fold_id == 5  |
  outer_fold_id == 18 & inner_fold_id == 7 
    ) %>% filter(index %in% c(15)) %>% 
    dplyr::select(-assess_inner, -has_outbreak, -nrow)})
, tar_target(folded_data_training_DEBUG, folded_data_training %>% filter(outer_fold_id %in% inner_fold_id_finalized_DEBUG$outer_fold_id))
, tar_target(folded_data_testing_DEBUG , folded_data_testing %>% filter(outer_fold_id %in% c(1, 2, 3)))

  ## Fit across tuning_grid across all inner folds of all outer folds
  ## NOTE: temporary minimal for working on downstream pipeline
, tar_target(tuned_results_per_outer_fold, tune_results_per_outer_fold(
      folded_data = folded_data_training_DEBUG %>% dplyr::select(outer_fold_id, inner_folds)
    , inner_ids   = inner_fold_id_finalized_DEBUG
    , raw_data    = splitted_data$train_data[[1]]
    , threshold   = positive_threshold
    , weightings  = weightings_on_ones
    , id_cols     = id_cols
    , out_dir     = outer_folds_dir
    , overwrite   = FALSE
    , DEBUG       = FALSE)
  , pattern = map(inner_fold_id_finalized_DEBUG)
  , error   = "null"
  , format  = "file")

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
  , weightval    = 0.25
    ## Currently both options are to maximize
  , direction    = "max"))

  ## Fit each outer fold with the best inner fold hyperparameter for each of these outer folds
, tar_target(tuned_results_across_outer_folds, tune_results_across_outer_folds(
    outer_data     = folded_data_training_DEBUG
  , raw_data       = splitted_data
  , threshold      = positive_threshold
  , hyperparm_sets = tuned_results_joined
  , weightings     = weightings_on_ones
  , id_cols        = id_cols
  , out_dir        = outer_folds_dir2
  , overwrite      = FALSE
  , DEBUG          = FALSE)
  , pattern = map(folded_data_training_DEBUG)
  , error   = "null"
  , format  = "file")

  ## Extract the best parameter set
, tar_target(finalized_hyperparameters, finalize_hyperparameters(
    outer_folds  = tuned_results_across_outer_folds
  , training_dat = splitted_data$train_data[[1]]
    ## See notes in tuned_results_joined
  , metric       = "mix"
  , weightval    = 0.25
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
  , id_cols         = id_cols
  , out_dir         = outer_folds_dir3
  , overwrite       = FALSE
  , DEBUG           = FALSE)
  , pattern = map(folded_data_testing)
  , error   = "null"
  , format  = "file")
  
  ## Join fitted_model paths to folded_data_testing for parallel processing for model eval
, tar_target(model_out_for_eval, build_model_out_for_eval(
    model_fits = fitted_model
  , full_data  = folded_data_testing))
  
)
  
## Asses model performance -----------------------------------------------------
model_evaluation_targets <- tar_plan(
  
  ## calibration curve groupings
  tar_target(cal_curve_splitgrp, c("forecast_interval"))
  
  ## Build the calibration curves
   ## Depending on what grouping variables are chosen, calibration curves may be made
   ## ACROSS all outer_fold_ids -- which would summarize prediction ability *generally*
   ## However, with a different grouping, plotting can be made *within* fit
, tar_target(calibration_curves, generate_calibration_curve(
    preds      = model_out_for_eval
  , test_data  = splitted_data$test_data[[1]]
  , predname   = ".pred_1"
  , truename   = "outbreak"
  , splitgrp   = cal_curve_splitgrp))
  
, tar_target(plotted_calibration, plot_calibration(
    caltib      = calibration_curves
  , xg          = NULL # "assess_range"
  , yg          = "forecast_interval"
  , forcastvals = c(30, 90, 150)))

## Struggling with RAM useage because targets is deciding to load a huge amount
 ## of stuff it doesn't actually need to run my previous larger function, so
  ## splitting this up
, tar_target(variable_importance_prep_a, prep_for_variable_importance_a(
    model_dat     = model_out_for_eval
  , splitted_data = splitted_data) 
  , pattern = map(model_out_for_eval))

## Actually do the variable importance calculation, now loading far fewer targets
, tar_target(variable_importance, calculate_variable_importance(
    model_dat       = variable_importance_prep_a
  , final_hyper_set = finalized_hyperparameters
  , fitdir          = outer_folds_dir3
  , recdir          = outer_folds_dir3
  , num_vars        = 10)
  , pattern = map(variable_importance_prep_a))

## And a quick comparison of the shapes of the partial dependence plots among fits
, tar_target(variable_importance_among, compare_vi(variable_importance = variable_importance))
  
  ## Evaluate fit in a few other ways apart from calibration curves:
   ## comparing prob to truth across space and time, distributions of probabilities for true ones, confusion matrix 
   ## as a function of different probability cutoffs, etc.
, tar_target(examined_fits_within, examine_fits_within(
    model_out        = model_out_for_eval
  , test_data        = splitted_data$test_data[[1]]
  , region_districts = region_map
  , africa_sf        = path_to_simplifed_regions
  , p_thresh         = positive_threshold
  , using_hexes      = using_hexes)
  , pattern = map(model_out_for_eval)
  , error   = "null")

  ## Then take all of the within-test period summaries and compare model performance broadly
   ## across all of these fitting periods (e.g., specific periods of time, specific countries etc.)
, tar_target(examined_fits_across, examine_fits_across(
    ex_within        = examined_fits_within
  , model_out        = model_out_for_eval
  , test_data        = splitted_data$test_data[[1]]
  , region_districts = region_map
  , africa_sf        = path_to_simplifed_regions
  , using_hexes      = using_hexes))
  
)

## Reports ---------------------------------------------------------------------
report_targets <- tar_plan(
  
  ## Put together a report composed of all of the model evaluation figures 
  tar_target(report, build_report(
    calcurves  = plotted_calibration
  , fitswithin = examined_fits_within
  , fitsacross = examined_fits_across
  , viacross   = variable_importance_among
  , outpath    = "outputs/report.pdf"
  , overwrite  = TRUE)
  , format  = "file")
  
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
