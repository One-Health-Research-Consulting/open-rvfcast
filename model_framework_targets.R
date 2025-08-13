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

# Download the data from the S3 bucket and partition into training, validation, and test splits
model_data_targets <- tar_plan(
  tar_target(RSA_data, arrow::open_dataset("s3://open-rvfcast/data/RSA_rvf_model_data") |> 
               collect() |>
               pivot_wider(names_from = lag_interval, values_from = c("ndvi_anomalies", "weather_anomolies"))),
)

cross_validation_targets <- tar_plan()

# Model -----------------------------------------------------------
model_tuning_targets <- tar_plan(
  tar_target(training_data, RSA_data |> filter(date <= "2017-12-31")),
  tar_target(holdout_data, RSA_data |> filter(date > "2017-12-31")),

  tar_target(folds),

  tar_target(xgb_mod),

  tar_target(xgb_grid),

  tar_target(xgb_recipe),

  tar_target(xgb_metrics),

model_fitting_targets <- tar_plan()
  
  # # RSA --------------------------------------------------
  # tar_target(augmented_data_rsa_directory,
  #            create_data_directory(directory_path = "data/augmented_data_rsa"),
  #            format = "file"),
  
  # # Switch to parquet based to save memory. Arrow left joins automatically.
  # tar_target(model_data,
  #            left_join(aggregated_data_rsa,
  #                      rvf_outbreaks,
  #                      by = join_by(date, shapeName)) |>
  #              mutate(outbreak_30 = factor(replace_na(outbreak_30, FALSE))) |>
  #              left_join(rsa_polygon_spatial_weights, by = "shapeName") |>
  #              mutate(area = as.numeric(area))
  # ),
  # 
  # tar_target(training_data, training(model_data_split)),
  # tar_target(holdout_data, testing(model_data_split)),
  
  # # Check if combined_anomalies parquet files already exists on AWS and can be loaded
  # # The only important one is the directory. The others are there to enforce dependencies.
  # tar_target(augmented_data_rsa_AWS, AWS_get_folder(augmented_data_rsa_directory,
  #                                                   weather_anomalies, # Enforce dependency
  #                                                   ndvi_anomalies, # Enforce dependency
  #                                                   dates_to_process),
  #            error = "null",
  #            cue = tar_cue("always")), # Enforce dependency
  #
  # tar_target(aggregated_data_rsa,
  #            aggregate_augmented_data_by_adm(augmented_data,
  #                                            rsa_polygon,
  #                                            dates_to_process),
  #            pattern = dates_to_process),
  #
  # tar_target(rsa_polygon_spatial_weights, rsa_polygon |>
  #              mutate(area = sf::st_area(rsa_polygon)) |>
  #              as_tibble() |>
  #              select(shapeName, area)),
  
  # # Switch to parquet based to save memory. Arrow left joins automatically.
  # tar_target(model_data,
  #            left_join(aggregated_data_rsa,
  #                      rvf_outbreaks,
  #                      by = join_by(date, shapeName)) |>
  #              mutate(outbreak_30 = factor(replace_na(outbreak_30, FALSE))) |>
  #              left_join(rsa_polygon_spatial_weights, by = "shapeName") |>
  #              mutate(area = as.numeric(area))
  # ),
  
  # 
  # # Splitting --------------------------------------------------
  # # Initial train and test (ie holdout)
  # tar_target(split_prop, nrow(model_data[model_data$date <= "2017-12-31",])/nrow(model_data)),
  # tar_target(model_data_split, initial_time_split(model_data, prop = split_prop)), 
  # tar_target(training_data, training(model_data_split)),
  # tar_target(holdout_data, testing(model_data_split)),
  # 
  # # formula/recipe 
  # tar_target(rec, model_recipe(training_data)),
  # tar_target(rec_juiced, juice(prep(rec))),
  # 
  # # xgboost settings
  # tar_target(base_score, sum(training_data$outbreak_30==TRUE)/nrow(training_data)),
  # tar_target(interaction_constraints, '[[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [15]]'), # area is the 16th col in rec_juiced
  # tar_target(monotone_constraints, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)), # enforce positive relationship for area
  # 
  # # tuning
  # tar_target(spec, model_specs(base_score, interaction_constraints, monotone_constraints)),
  # tar_target(grid, model_grid(training_data)),
  # 
  # # workflow
  # tar_target(wf, workflows::workflow(rec, spec)),
  # 
  # # splits
  # tar_target(rolling_n, n_distinct(model_data$shapeName)),
  # tar_target(splits, rolling_origin(training_data, 
  #                                   initial = rolling_n, 
  #                                   assess = rolling_n, 
  #                                   skip = rolling_n - 1)),
  # 
  # # tuning
  # tar_target(tuned, model_tune(wf, splits, grid)),
  
  # final model
  # tar_target(final, {
  #   final_wf <- finalize_workflow(
  #     wf,
  #     tuned[5,]
  #   )
  #   
  #   library(DALEX)
  #   library(ceterisParibus)
  #   
  #   # DALEX Explainer
  #   tuned_model <- final_wf |> fit(training_data)
  #   tuned_model_xg <- extract_fit_parsnip(tuned_model)
  #   training_data_mx <- extract_mold(tuned_model)$predictors %>%
  #     as.matrix()
  #   
  #   y <- extract_mold(tuned_model)$outcomes %>%
  #     mutate(outbreak_30 = as.integer(outbreak_30 == "1")) %>%
  #     pull(outbreak_30)
  #   
  #   explainer <- DALEX::explain(
  #     model = tuned_model_xg,
  #     data = training_data_mx,
  #     y = y,
  #     predict_function = predict_raw,
  #     label = "RVF-EWS",
  #     verbose = TRUE
  #   )
  #   
  #   # CP plots
  #   predictors <- extract_mold(tuned_model)$predictors |> colnames()
  #   holdout_small <- as.data.frame(select_sample(training_data, 20)) |> 
  #     select(all_of(predictors), outbreak_30) |> 
  #     mutate(area = as.numeric(area)) |> 
  #     mutate(outbreak_30 = as.integer(outbreak_30 == "1"))
  # 
  #   
  #   
  # 
  #   cPplot <- ceterisParibus::ceteris_paribus(explainer, 
  #                                             observation = holdout_small |> select(-outbreak_30),
  #                                             y = holdout_small |>  pull(outbreak_30)#,
  #                                             #variables = "area"
  #                                             )
  #   plot(cPplot)+
  #     ceteris_paribus_layer(cPplot, show_rugs = TRUE)
  #   
  #   
  # }),  
  
  #TODO fit final model
  #TODO test that interaction constraints worked - a) extract model object b) cp - 
  # need the conditional effect - area is x, y is effect, should not change when you change other stuff
  # ceteris parabus plots - should be parallel - points can differ but profile should be the same - expectation is that it is linear if doing it on area
)

# Reports -----------------------------------------------------------
# The goal is to compare model performance. 
# We want a plot with ROC curves for every different model specification
model_evaluation_targets <- tar_plan(
  
)

# Reports -----------------------------------------------------------
report_targets <- tar_plan(
  
)

# Documentation -----------------------------------------------------------
documentation_targets <- tar_plan(
  # tar_target(readme, rmarkdown::render("README.Rmd"))
  tar_render(readme, path = here::here("README.Rmd"))
)

# List targets -----------------------------------------------------------------
# all_targets() doesn't work with tarchetypes like tar_change().
list(model_data_targets,
     cross_validation_targets,
     model_tuning_targets,
     model_fitting_targets,
     model_evaluation_targets,
     report_targets,
     documentation_targets)
