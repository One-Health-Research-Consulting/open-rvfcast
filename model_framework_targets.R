# This repository uses targets projects
# To switch to the modeling pipeline run:
# Sys.setenv(TAR_PROJECT = "model")

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
   ## Sub Region (e.g., Country) and Sub-Sub Regions (e.g., adm2 -- i.e., district or county) of interest
  tar_target(region_name, "pan")
, tar_target(region_data_path
             , paste("data/", region_name, "_joined_response_data/"
               , region_name, "_joined_response_data_final.parquet"
               , sep = "")
  )
, tar_target(region_data, read_parquet(region_data_path) %>% ungroup() %>%
               mutate(index = seq(n()), .before = 1)
  )
  ## Other paths to intermediate products to save computation time
 , tar_target(path_to_joined_regions   , paste("data/joined_", region_name, "_regions.Rds", sep = ""))
 , tar_target(path_to_collapsed_regions, paste("data/reduced_", region_name, "_regions.Rds", sep = ""))
 , tar_target(path_to_region_neighbors , paste("data/", region_name, "_region_neighbors.Rds", sep = ""))
 , tar_target(path_to_clustered_regions, paste("data/clustered_", region_name, "_regions.Rds", sep = ""))
 , tar_target(path_to_simplifed_regions, paste("data/simplified_", region_name, "_sf.Rds", sep = ""))

  ## Pulls all African countries. Alternatively can just provide a single country
   ## directly to get_region_districts below
, tar_target(which_countries, unique(region_data$Country))

  ## Sub-regions of region[s] of interest
, tar_target(region_districts, get_region_districts(which_countries))

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
  ))
  
  ## Number of spatial folds (parameter used in multiple functions)
, tar_target(n_spatial_folds, 40)

  ## Generate n_spatial_folds clusters of all Africa regions
   ## Note: all funsions in spatial_helpers.R
, tar_target(clustered_Africa_districts, make_area_clusters(
     sf_list                   = region_districts
   , path_to_joined_regions    = path_to_joined_regions
   , path_to_collapsed_regions = path_to_collapsed_regions
   , path_to_region_neighbors  = path_to_region_neighbors
   , path_to_clustered_regions = path_to_clustered_regions
   , k                         = n_spatial_folds
   , tol                       = 0.30
   , seed                      = 10001
   , min_area_km2              = 5
  ))

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
                   theme(legend.position = "none")
               }
               )
  
  ## Generate CV folds for training data
, tar_target(folded_data_training_raw, fold_data(
      data              = splitted_data
      ## Two options, train_data or test_data. 
       ## train_data sets up inner folds for hyperparameter tuning 
       ## test_data just splits testing period into chunks for assessing forecasting accuracy
    , type              = "train_data"
    , sf_districts      = clustered_Africa_districts
    , assess_time_chunk = forecast_horizon + max_lag_period
    ## Time gap between the end of the previous fold and the start of the next fold. For now setting to
     ## the 3 month lag for the variables for no overlap
    , step_size         = max_lag_period 
    , district_id_col   = "shapeName"
    , seed              = 10001
    ))
    
  ## Collapse these based on some criteria of "information content" 
, tar_target(folded_data_training, clean_folded_data(
     raw_data                 = splitted_data$train_data
   , folded_data              = folded_data_training_raw
   , epidemic_threshold_total = 10
   , epidemic_threshold_space = 3
  ))

 ## Generate test cases for assessing model performance
, tar_target(folded_data_testing, fold_data(
    data              = tibble(test_data = region_data %>% 
                                 dplyr::filter(forecast_interval == forecast_horizon) %>% 
                                 list())
  , type              = "test_data"
  , sf_districts      = clustered_Africa_districts
  , assess_time_chunk = forecast_horizon + max_lag_period
  , step_size         = max_lag_period 
  , n_spatial_folds   = NULL
  , district_id_col   = "shapeName"
  , seed              = 10001
  , holdout_start     = end_date
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
      , finalize(mtry()      , folded_data_training$inner_folds[[20]] %>% 
                   left_join(., splitted_data$train_data[[1]], by = "index") %>% filter(cluster != 1))
      ## Total number of combinations of hyperparameters
      , size = 40 
      )
    ) %>% mutate(index = seq(n()), .before = 1)
  )

, tar_target(id_cols, c("shapeName", "Country", "date", "index"))

  ## Set up location for saving intermediate output
, tar_target(outer_folds_dir, create_data_directory(
    directory_path = paste("outputs/", region_name, "_model_tuning_inner", sep = "")
  ))
, tar_target(outer_folds_dir2, create_data_directory(
    directory_path = paste("outputs/", region_name, "_model_tuning_outer", sep = "")
  ))
, tar_target(outer_folds_dir3, create_data_directory(
    directory_path = paste("outputs/", region_name, "_final_model_fits", sep = "")
  ))

  ## NOTE: temp check for debugging purposes
, tar_target(folded_data_training_debug, folded_data_training[c(1, 10, 21, 31, 41), ])
, tar_target(tuning_grid_debug, tuning_grid[1:8, ])
, tar_target(folded_data_testing_debug, folded_data_testing[c(1, 5, 10, 15), ])

  ## Final prep step for parallel processing for tuning across all inner folds
, tar_target(inner_fold_id, data.frame(inner_fold_id = seq(n_spatial_folds)))

  ## Fit across tuning_grid across all inner folds of all outer folds
  ## NOTE: temporary minimal for working on downstream pipeline
, tar_target(tuned_results_per_outer_fold, tune_results_per_outer_fold(
      folded_data = folded_data_training_debug
    , inner_ids   = inner_fold_id
    , raw_data    = splitted_data
    , tuning_grid = tuning_grid_debug
    , id_cols     = id_cols
    , out_dir     = outer_folds_dir
    , overwrite   = FALSE
    )
  , pattern = cross(folded_data_training_debug, tuning_grid_debug, inner_fold_id)
  , error   = "null"
  , format  = "file"
 )

  ## Join together all tuned inner folds and select the best per outer fold
, tar_target(tuned_results_joined, join_tuned_inner_folds(
    inner_folds = tuned_results_per_outer_fold
  ))

  ## Fit each outer fold with the best inner fold hyperparameter for each of these outer folds
, tar_target(tuned_results_across_outer_folds, tune_results_across_outer_folds(
    outer_data     = folded_data_training_debug
  , raw_data       = splitted_data
  , hyperparm_sets = tuned_results_joined
  , id_cols        = id_cols
  , out_dir        = outer_folds_dir2
  , overwrite      = FALSE
  )
  , pattern = map(folded_data_training_debug)
  , error   = "null"
  , format  = "file"
 )

  ## Extract the best parameter set
, tar_target(finalized_hyperparameters, finalize_hyperparameters(
    outer_folds   = tuned_results_across_outer_folds
  , chosen_metric = "mn_log_loss"
  , direction     = "minimize"
  ))

)

## Fitting of model on holdout data --------------------------------------------
model_fitting_targets <- tar_plan(
  
  ## Use the finalized hyperparameters to fit the model for all of the chunks of time that
   ## make up the testing phase
  tar_target(fitted_model, fit_model(
    final_hyper_set = finalized_hyperparameters
  , full_data       = folded_data_testing_debug
  , raw_data        = splitted_data
  , id_cols         = id_cols
  , out_dir         = outer_folds_dir3
  , overwrite       = FALSE
  )
  , pattern = map(folded_data_testing_debug)
  , error   = "null"
  , format  = "file"
  )
  
  ## Join fitted_model paths to folded_data_testing for parallel processing for model eval
, tar_target(model_out_for_eval, build_model_out_for_eval(
    model_fits = fitted_model
  , full_data  = folded_data_testing_debug
  ))
  
)
  
## Asses model performance -----------------------------------------------------
model_evaluation_targets <- tar_plan(
  
  ## Evaluate fit in a few ways -- comparing prob to truth, confusion matrix, map, etc.
  tar_target(examined_fits, examine_fit(
     model_out        = model_out_for_eval[1, ]
   , test_data        = splitted_data$test_data[[1]]
   , train_data       = splitted_data$train_data[[1]]
   , region_districts = region_districts
   , africa_sf        = path_to_simplifed_regions
  )
   , pattern = map(model_out_for_eval)
   , error   = "null"
  )
  
)

## Reports ---------------------------------------------------------------------
report_targets <- tar_plan(
  
  ## Write maps to disk  
  tar_target(export_pdf,
    {
      out_file <- "outputs/map_predictions.pdf"
      dir.create(dirname(out_file), showWarnings = FALSE)
      
      pdf(out_file, width = 7, height = 5)
      for (p in examined_fits$map) {
        print(p)  # each print creates a new page
      }
      dev.off()
      
      out_file
    },
    format = "file"  # ensures targets tracks this file as an output
  )
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
