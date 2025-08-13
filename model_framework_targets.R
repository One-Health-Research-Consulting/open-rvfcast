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
, tar_target(region_data, read_parquet(region_data_path))

  ## Sub-regions of region of interest
, tar_target(region_districts, rgeoboundaries::geoboundaries("South Africa", "adm2"))

  ## Last date of the training data set (all data beyond this date will be set aside for final model evaluation)
, tar_target(end_date, "2020-12-19")

  ## As in the comment in the preamble, testing my mental map of the problem and working on code dev for
   ## one forecast horizon for now
, tar_target(forecast_horizon, 90)

)

## Targets for preparing for model tuning --------------------------------------
cross_validation_targets <- tar_plan(
  
  ## Best to split the data first then fold on the training data. Can do so on end_date
   ## Going for name of target as a noun (even if it is a funny nonsense word like it is here)
   ## and the function as the related verb
  tar_target(splitted_data, split_data())
  
  ## Fold the data
  tar_target(folded_data, fold_data(
      data             = region_data %>% filter(forecast_interval == forecast_horizon)
    , sf_districts     = region_districts
    , start_date       = "2005-04-07"
    , end_date         = end_date
    , forecast_horizon = forecast_horizon
    ## For now setting this so that the next fold begins the day after the end of the prior
     ## forecast horizon. This could end up being too computationally demanding, but we shall see
    , step_size        = forecast_horizon
    ## 10 Seems sensible to me for a start
    , n_spatial_folds  = 10
    , district_id_col  = "shapeName"
    , seed             = 10001
  ))
  
)

## Targets for conducting model tuning -----------------------------------------
model_tuning_targets <- tar_plan()

## Fitting of model on holdout data --------------------------------------------
model_fitting_targets <- tar_plan()
  
## Asses model performance -----------------------------------------------------
model_evaluation_targets <- tar_plan()

## Reports ---------------------------------------------------------------------
report_targets <- tar_plan()

## Documentation ---------------------------------------------------------------
documentation_targets <- tar_plan(
  tar_render(readme, path = here::here("README.Rmd"))
)

# List targets -----------------------------------------------------------------
list(
  model_data_targets
, cross_validation_targets
, model_tuning_targets
, model_fitting_targets
, model_evaluation_targets
, report_targets
, documentation_targets
)
