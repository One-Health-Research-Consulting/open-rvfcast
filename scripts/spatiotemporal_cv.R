# 2D tomorrow. Abandon nested resample approach and just mirror what Emma was doing. We need to make sure the variables we chose match what she had. I think we're missing polygon area.


library(spatialsample)
library(rsample)
library(yardstick)


# Create sample data
set.seed(123)
districts <- paste("Pop", 1:5, sep = "_")
dates <- seq.Date(from = as.Date("2025-01-01"), by = "days", length.out = 25)
temperatures <- runif(125, min = -10, max = 35)  # Temperatures between -10 and 35

# Expand grid for combinations of districts and dates
fake_data <- expand.grid(district = districts, date = dates) |> mutate(outbreak = as.factor(rbinom(n(), 1, 0.5)))

# Add temperatures to the dataframe
fake_data$temp <- temperatures

# View first few rows of the data
head(fake_data)

# Key here is to first sort by date and figure out how many we should be assessing and skipping
# For this to work each date must have the same number of entries.
# This does spatio-temporal cross-validation.
skip_rows = unique(table(fake_data$date))
assertthat::are_equal(length(skip_rows), 1)

# This code performs sets up nested cross-resampling using the `rsample` package, combining 
# a rolling origin resampling strategy for the outer cross-validation loop and 
# a spatial leave-location-out strategy for the inner cross-validation loop.
# https://www.tidymodels.org/learn/work/nested-resampling/
folds <- fake_data |> rsample::nested_cv(
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
xgb_spec <- boost_tree(
  mode = "classification",
  trees = tune(),          # Number of boosting iterations (nrounds)
  tree_depth = tune(),     # Max depth of trees (complexity control)
  learn_rate = tune(),     # Shrinkage (eta) to prevent overfitting
  mtry = tune(),           # Fraction of predictors per split (colsample_bytree)
  min_n = 12,          # Minimum observations in terminal nodes (min_child_weight)
  loss_reduction = tune()  # Gamma: Minimum loss reduction to split a node
) %>%
  set_engine("xgboost")

xgb_grid <- grid_latin_hypercube(
  trees(),        
  tree_depth(),        
  learn_rate(),     
  finalize(mtry(), fake_data),  
  loss_reduction(),    
  size = 100                          
)

# Define the workflow
xgb_wf <- workflow() %>%
  step_num2factor(outcome, levels = c("yes", "no")) |>
  add_formula(outbreak ~ .) %>%
  add_model(xgb_spec)

xgb_metrics = yardstick::metric_set(
  roc_auc,    # Area under ROC curve
  pr_auc,     # Area under Precision-Recall curve
  recall,     # Sensitivity
  precision, 
  f_meas,     # F1-score
  bal_accuracy # Balanced accuracy
)

# Function to perform tuning for each set of inner folds
compute_preds <- function(inner_resample, 
                          xgb_wf,
                          xgb_metrics,
                          outer_id) {
  
  xgb_res <- tune_grid(
    xgb_wf,
    resamples = inner_resample,
    grid = xgb_grid,
    metrics = xgb_metrics,
    control = control_grid(save_pred = FALSE)
    ) 
  
  xgb_res |> pull(.metrics) |> bind_rows() |> mutate(.outer_fold = outer_id)
  
}

# Perform nested hyper-parameter across outer folds
nested_results <- folds %>%
  mutate(results = map2(inner_resamples, id, ~compute_preds(.x, xgb_wf, xgb_metrics, .y)))

# View results
nested_results

# Function to extract best set of hyperparameters 
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

select_best_nested(nested_results$results, params = names(xgb_grid))

library(pROC)

# Predict probabilities on hold-out data
test_pred_probs <- predict(tuned_xgb_model, newdata = test_data, type = "prob")[, "OutbreakClass"]

# Calculate ROC curve
roc_obj <- roc(test_data$OutbreakClass, test_pred_probs)
auc_value <- auc(roc_obj)

# Plot ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"),
     col = "blue", print.auc = TRUE)


