# This repository uses targets projects
# To switch to the modeling pipeline run:
# Sys.setenv(TAR_PROJECT = "model")

## MPK NOTEs on August 13, 2025. 
 ## 1) For cleanliness I am removing the huge amount of commented out code. Very well could be 
  ## that some of this could be useful / important. For this code see commits
  ## prior to August 13, 2025
 ## 2) Working for now with one forecast horizon to get an initial pipeline functioning and make sure
  ## my mental map is in order. To be expanded to all forecast horizons afterward. Aim to make as dynamic
  ## as possible so that this is not a burden

# Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true"))
  capsule::capshot(c("packages.R",
                     list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE),
                     list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)))

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Targets options
source("_targets_settings.R")

# Convenience function to format .env flags properly for overwrite parameter and target cues
parse_flag <- function(flags, cue = F) {
  flags <- any(as.logical(Sys.getenv(flags, unset = "FALSE")))
  if (cue) flags <- targets::tar_cue(ifelse(flags, "always", "thorough"))
  flags
}

## Targets for loading needed data ---------------------------------------------
model_data_targets <- tar_plan(

  ## Eventually will want to download the data from the S3 bucket, but for now load from local
  tar_target(region_data_path, "data/RSF_cleaned_response_data/RSF_cleaned_response_data.parquet")
, tar_target(region_data, read_parquet(region_data_path) %>% ungroup())

  ## Sub-regions of region of interest
, tar_target(region_districts, rgeoboundaries::geoboundaries("South Africa", "adm2"))

  ## Last date of the training data set (all data beyond this date will be set aside for final model evaluation)
, tar_target(end_date, as.Date("2020-12-19"))

  ## As in the comment in the preamble, testing my mental map of the problem and working on code dev for
   ## one forecast horizon for now
, tar_target(forecast_horizon, 90)
, tar_target(max_lag_period, 90)

)

## Targets for preparing for model tuning --------------------------------------
cross_validation_targets <- tar_plan(
  
  ## Best to split the data first then fold on the training data. Can do so on end_date
   ## Going for name of target as a noun (even if it is a funny nonsense word like it is here)
   ## and the function as the related verb
  tar_target(splitted_data, split_data(
    dat              = region_data %>% filter(forecast_interval == forecast_horizon)
    ## Prevent overlap in training and test, so start test after the end of the forecast horizon 
     ## from the last training date
  , end_date         = end_date
  , forecast_horizon = forecast_horizon
  ))
  
  ## Generate CV folds for training data
, tar_target(folded_data_training_raw, fold_data(
      data              = splitted_data
      ## Two options, train_data or test_data. 
       ## train_data sets up inner folds for hyperparameter tuning 
       ## test_data just splits testing period into chunks for assessing forecasting accuracy
    , type              = "train_data"
    , sf_districts      = region_districts
    , assess_time_chunk = forecast_horizon + max_lag_period
    ## Time gap between the end of the previous fold and the start of the next fold. For now setting to
     ## the 3 month lag for the variables for no overlap
    , step_size         = max_lag_period 
    ## 10 Seems sensible to me for a start
    , n_spatial_folds   = 10
    , district_id_col   = "shapeName"
    , seed              = 10001
    ))
    
  ## Collapse these based on some criteria of "information content" 
, tar_target(folded_data_training, clean_folded_data(
     data                     = folded_data_training_raw
   , epidemic_threshold_total = 10
   , epidemic_threshold_space = 3
  ))

 ## Generate test cases for assessing model performance
, tar_target(folded_data_testing, fold_data(
    data              = splitted_data
  , type              = "test_data"
  , sf_districts      = region_districts
  , assess_time_chunk = forecast_horizon + max_lag_period
  , step_size         = max_lag_period 
  , n_spatial_folds   = NULL
  , district_id_col   = "shapeName"
  , seed              = 10001
))

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
  , size           = 20)
  )
  
, tar_target(tuning_grid,
    with(tune_pars
    , grid_space_filling(
        trees(range          = c(tree_min, tree_max))
      , tree_depth(range     = c(tree_dep_min, tree_dep_max))
      , learn_rate(range     = c(learn_rate_min, learn_rate_max), trans = NULL)
      , min_n(range          = c(minn_min, minn_max))
      , loss_reduction(range = c(loss_red_min, loss_red_max))
      ## Arbitrary choice here in which train_inner, shouldn't really matter
      , finalize(mtry()      , folded_data_training$inner_folds$train_inner[[10]])
      ## Total number of combinations of hyperparameters
      , size = 40 
      )
    )
  )  

, tar_target(tuned_results_per_outer_fold, tune_results_per_outer_fold(
    data             = folded_data_training$inner_folds
  , tuning_grid      = tuning_grid
  )
  , pattern = map(folded_data_training$inner_folds)
 )

, tar_target(tuned_results_across_outer_folds, tune_results_across_outer_folds(
    data           = folded_data_training$outer_folds
  , hyperparm_sets = tuned_results_per_outer_fold
  )
  , pattern = map(folded_data_training$outer_folds)
 )

  
)

## Fitting of model on holdout data --------------------------------------------
model_fitting_targets <- tar_plan(
  
)
  
## Asses model performance -----------------------------------------------------
model_evaluation_targets <- tar_plan()

## Reports ---------------------------------------------------------------------
report_targets <- tar_plan()

# List targets -----------------------------------------------------------------
list(
  model_data_targets
, cross_validation_targets
, model_tuning_targets
, model_fitting_targets
, model_evaluation_targets
, report_targets
)
