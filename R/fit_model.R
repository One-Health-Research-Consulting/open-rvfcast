#' Little function to build the model recipe. Model run for a given training dataset
#'
#'
#' @title make_recipe

#' @param final_hyper_set The choice of hyperparameters for final model fitting
#' @param full_data Full training set and test data
#' @param train_data Training data (splitted_data_fitting$train_data[[1]])
#' @param test_data Test data (splitted_data_fitting$test_data[[1]])
#' @param threshold For assigning a 1 | estimated prob
#' @param weightings weight assigned to 1s in binomial loss metric
#' @param start_p initilization point for base intercept
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @param DEBUG If TRUE reduce to a small dataset for code testing
#' @return Tibble of model fit output
#' @author Morgan Kain
#' @export

fit_model <- function(final_hyper_set, full_data, train_data, test_data, threshold, weightings, start_p, id_cols, out_dir, overwrite, DEBUG) {

  ## Set filenames
  save_filename <- paste(
    out_dir
    , "/"
    , "model_fit_"
    , full_data$outer_fold_id
    , "_"
    , final_hyper_set$index
    , ".Rds"
    , sep = "")

  ## partial filenames as well for partial dependence plots so the entire workflow doesn't need to be loaded
  save_filename2 <- paste(
      out_dir
    , "/"
    , "parsnip_fit_"
    , full_data$outer_fold_id
    , "_"
    , final_hyper_set$index
    , ".Rds"
    , sep = "")

  save_filename3 <- paste(
    out_dir
    , "/"
    , "recipe_"
    , full_data$outer_fold_id
    , "_"
    , final_hyper_set$index
    , ".Rds"
    , sep = "")

  ## Loading these Rds are very slow, so just checking that they exist
  if (file.exists(save_filename) && !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }

  ## Extract the needed data
  ## 1) full amount of training data (all the stuff from the hyperparameter tuning step)
  ## 2) some portion of the data from the left-out period depending on what forecast window is being predicted
  outer_tbl_train <- rbind(
    train_data |>
      dplyr::filter(index %in% full_data$train_data[[1]]) |>
      dplyr::select(-c(cases)) |>
      mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  , test_data |>
      dplyr::filter(index %in% full_data$train_data[[1]]) |>
      dplyr::select(-c(cases)) |>
      mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  ) |>
    mutate(forecast_interval = as.factor(forecast_interval)) |>
    ## Get class imbalance ratio per forecast horizon
    group_by(forecast_interval) |>
    mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
    , weights = ifelse(outbreak == "0", 1, weights)
    , weights = hardhat::importance_weights(weights)
    , .after = "index"
    )

  outer_tbl_assess  <- test_data |>
    dplyr::filter(index %in% full_data$assess_data[[1]]) |>
    dplyr::select(-c(cases)) |>
    mutate(outbreak = factor(outbreak, levels = c(1, 0))) |>
    mutate(forecast_interval = as.factor(forecast_interval)) |>
    mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
    , weights = ifelse(outbreak == "0", 1, weights)
    , weights = hardhat::importance_weights(weights)
    , .after = "index"
    )

  if (DEBUG) {
    outer_tbl_train  <- outer_tbl_train[sample(seq_len(nrow(outer_tbl_train)), 100000), ]
  }

  ## Attempt to clear up some ram
  rm(train_data, test_data)
  gc()

  ## Set up and fit the final model for this outer fold
  rec       <- make_recipe(outer_tbl_train, id_cols = id_cols)
  mod       <- make_model(params = final_hyper_set, start_p = start_p)
  wf        <- workflow() |> add_model(mod) |> add_recipe(rec) |> add_case_weights(weights)
  model_fit  <- fit(wf, data = outer_tbl_train)

  ## Predict probabilities and class labels on outer assessment
  preds <- predict(model_fit, outer_tbl_assess, type = "prob") |>
    bind_cols(predict(model_fit, outer_tbl_assess, type = "class")) |>
    bind_cols(
        outer_tbl_assess |>
        select(outbreak)
    ) |>
    mutate(
      outbreak    = factor(outbreak, levels = c("1", "0"))
    , .pred_class = factor(.pred_class, levels = c("1", "0"))
    )

  ## Predictions: prob only
  prob1     <- predict(model_fit, outer_tbl_assess, type = "prob")$.pred_1
  truth     <- factor(outer_tbl_assess[["outbreak"]], levels = c("1", "0"))
  class_hat <- apply(
      threshold |> matrix()
    , 1
    , FUN = function(x) factor(ifelse(prob1 >= x, "1", "0"), levels = c("1", "0"))
  )

  ## Compute metrics
  metrics <- compute_metrics_vec(
      truth       = truth
    , threshold   = threshold
    , weightings  = weightings
    , caseweights = outer_tbl_assess |> pull(weights)
    , prob1       = prob1
    , class_hat   = class_hat
    , event_level = "first"
  ) |>
  mutate(
    outer_fold = full_data$outer_fold_id
  , .before    = 1
  ) |>
  bind_cols(final_hyper_set)

  ## Return
  model_out <- tibble(
    outer_fold_id = full_data$outer_fold_id
  , path          = save_filename
  , preds         = preds |> list()
  , hyperparams   = final_hyper_set |> list()
  , metrics       = metrics |> list()
  )

  saveRDS(model_out, save_filename)
  saveRDS(model_fit |> extract_fit_parsnip(), save_filename2)
  saveRDS(model_fit |> extract_recipe(estimated = TRUE), save_filename3)

  save_filename

}

build_model_out_for_eval <- function(model_fits, full_data) {
  safe_join_names <- purrr::map(seq_along(model_fits), .f = function(i) {
    readRDS(model_fits[i])
  }) |> bind_rows()
  full_data |> left_join(safe_join_names)
}
