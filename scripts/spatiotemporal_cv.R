library(spatialsample)
library(rsample)
library(yardstick)
library(tidymodels)
library(vip)

# Generate dates for simulated data
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2022-12-31")

# Define district-specific effects
district_effects <- tibble(
  district = c("A", "B", "C", "D", "E"),
  base_prob = c(0.08, 0.10, 0.12, 0.09, 0.11),  # Baseline probabilities
  temp_effect = c(0.01, 0.015, 0.012, 0.008, 0.014)  # Temperature effects
)

# Create all combinations of dates and populations
set.seed(123)  # For reproducibility
fake_data <- expand.grid(
  date = seq(start_date, end_date, by = "day"),
  district = c("A", "B", "C", "D", "E")
) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    # Random temperature between 0-35°C
    temperature = round(runif(n(), 0, 35), 1)
  ) |>
  # Join district-specific effects
  left_join(district_effects, by = "district") |>
  dplyr::mutate(
    # Calculate outbreak probability using logistic function
    logit_prob = base_prob + temp_effect * temperature,
    prob = plogis(logit_prob),  # Convert logit to probability
    # Simulate outbreaks
    outbreak = as.factor(rbinom(n(), 1, prob))
  ) |>
  dplyr::select(-base_prob, -temp_effect, -logit_prob, -prob)  # Remove intermediate columns

# View the updated data
head(fake_data)

# Key here is to first sort by date and figure out how many we should be assessing and skipping
# For this to work each date must have the same number of entries.
# This does spatio-temporal cross-validation.
skip_rows = unique(table(fake_data$date))
assertthat::are_equal(length(skip_rows), 1)

train_data <- fake_data |> filter(date < "2018-01-01")
holdout_data <- fake_data |> filter(date >= "2018-01-01")

# This code sets up nested cross-resampling using the `rsample` package, combining 
# a rolling origin strategy for the outer cross-validation loop and 
# a spatial leave-location-out strategy for the inner cross-validation loop.
# https://www.tidymodels.org/learn/work/nested-resampling/
folds <- train_data |> rsample::nested_cv(
  # Outer cross-validation: Rolling origin resampling
  outside = rolling_origin(
    initial = 10*skip_rows,  # The size of the initial training set (number of rows).
    assess = skip_rows,   # The size of the assessment set (test set) for each split.
    skip = skip_rows      # The number of rows skipped between successive splits.
  ),
  # Inner cross-validation: Spatial leave-location-out
  inside = spatial_leave_location_out_cv(
    group = "district"  # The column in the data that defines spatial groups (here, districts).
    # Each "location" (district) will be left out in turn for validation.
  )
)

# Define the model specification
xgb_mod <- boost_tree(
  mode = "classification",
  trees = tune(),          # Number of boosting iterations (nrounds)
  tree_depth = tune(),     # Max depth of trees (complexity control)
  learn_rate = tune(),     # Shrinkage (eta) to prevent overfitting
  mtry = tune(),           # Fraction of predictors per split (colsample_bytree)
  min_n = 12,              # Minimum observations in terminal nodes (min_child_weight).
  loss_reduction = tune()  # Gamma: Minimum loss reduction to split a node
) |>
  parsnip::set_engine("xgboost")

xgb_grid <- dials::grid_space_filling(
  size = 100,
  trees(),        
  tree_depth(),        
  learn_rate(),     
  finalize(mtry(), fake_data),
  loss_reduction())

# Define recipe with preprocessing steps
# Note: the data contained in the data argument need not be the training set; 
# this data is only used to catalog the names of the variables and their 
# types (e.g. numeric, etc.).
xgb_recipe <- recipe(outbreak ~ ., data = train_data) |>  # Use template here
  # I don't think we are using date or district as predictors
  step_rm(date, district)
  # One-hot encode population (convert categorical to dummy variables)
  # step_dummy(district, one_hot = TRUE)

# Define a set of classification metrics to look at
xgb_metrics = yardstick::metric_set(
  roc_auc,    # Area under ROC curve
  pr_auc,     # Area under Precision-Recall curve
  recall,     # Sensitivity
  precision,  # How well a model can correctly predict positive outcomes
  f_meas,     # F1-score
  bal_accuracy # Balanced accuracy
)

# Function to perform tuning for each set of inner folds
compute_preds <- function(inner_resample, 
                          xgb_mod,
                          xgb_recipe,
                          xgb_metrics,
                          outer_id) {
  
  xgb_res <- tune_grid(
    xgb_mod,
    xgb_recipe,
    resamples = inner_resample,
    grid = xgb_grid,
    metrics = xgb_metrics,
    control = control_grid(save_pred = FALSE)
  ) 

  xgb_res |> 
    pull(.metrics) |> 
    bind_rows() |> 
    mutate(.outer_fold = outer_id)
}

# Perform nested hyper-parameter cross validation. This is quite 
# computationally intensive but can take advantage of dynamic branching over 
# the rows of the folds dataframe. It returns a nested column containing 
# performance metrics across all outer and inner cross validation folds.
nested_results <- folds |>
  mutate(results = map2(inner_resamples, id, ~compute_preds(.x, xgb_mod, xgb_recipe, xgb_metrics, .y)))

# View results
nested_results

# Function to extract the best set of hyperparameters
# Across both inner and outer folds randomly breaking ties
select_best_nested <- function(results, params, metric = "roc_auc") {
  
  results |>
    bind_rows() |>
    filter(.metric == metric) |>
    group_by_at(c(params, ".metric")) |>
    summarize(n = n(),
              .sd = sd(.estimate, na.rm = T),
              .estimate = mean(.estimate, na.rm = T),
              .groups = "drop") |>
    filter( .estimate == max(.estimate, na.rm = T)) |>
    slice_sample(n = 1)
}

# Extract the best set of hyperparameters
best_hyperparam_set <- select_best_nested(nested_results$results, params = names(xgb_grid))

# Finalize the workflow using the best hyperparameters found during tuning
xgb_model_final <- finalize_model(xgb_mod, best_hyperparam_set)
xgb_model_final

xgb_workflow <- workflow() |>
  add_recipe(xgb_recipe) |>
  add_model(xgb_model_final)

trained_workflow <- xgb_workflow |> 
  fit(data = train_data)

holdout_predictions <- predict(trained_workflow, new_data = holdout_data, type = "prob") |>
  bind_cols(predict(trained_workflow, new_data = holdout_data)) |> 
  bind_cols(holdout_data)

# Generate the ROC curve
roc_data <- holdout_predictions |>
  roc_curve(truth = outbreak, .pred_1)

# Plot the ROC curve
autoplot(roc_data)
