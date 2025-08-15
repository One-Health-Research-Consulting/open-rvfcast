#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_per_outer_fold

#' @param data a given inner fold
#' @param tuning_grid set of potential hyperparameters
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold <- function(data, tuning_grid) {
  
  ## Establish metric set
  all_metrics <- metric_set(mn_log_loss, pr_auc, roc_auc, recall, precision)
  
  ## Extract out the data
  inner_tbl_train  <- data$train_inner[[1]]
  inner_tbl_assess <- data$assess_inner[[1]]
  
  ## Get class imbalance ratio
  neg_pos_ratio <- sum(inner_tbl_train$outbreak == 0) / sum(inner_tbl_train$outbreak == 1)
  
  ## Create scaffold recipe + model
  rec <- make_recipe(inner_tbl_train)
  mod <- make_model(scale_pos_weight = neg_pos_ratio)
  
  ## create rsample object from inner folds
  inner_splits <- manual_rset(
    splits = map2(inner_tbl_train, inner_tbl_assess,
                  ~ rsample::make_splits(list(analysis = .x, assessment = .y), .x)),
    ids = paste0("Inner", data$inner_fold_id)
  )
  
  ## Initial workflow scaffold
  wf <- workflow() %>% add_model(mod) %>% add_recipe(rec)
  
  ## Tune over inner folds
  tuned <- tune_grid(
    wf
  , resamples = inner_splits
  , grid      = tuning_grid
  , metrics   = all_metrics
  )
  
  ## Select best for this outer fold
  best <- select_best(tuned, "mn_log_loss")
  
  ## Return the best for this inner fold
  tibble(
    outer_fold_id = outer_id[i]
  , best_params   = list(best)
  )
  
}


#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_per_outer_fold

#' @param data outer fold
#' @param hyperparm_sets maximized hyperparameter sets across all inner folds of all outer folds
#' @param scale_pos_weight weight for positive responses if desired, default is null
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_across_outer_folds <- function(data, hyperparm_sets) {
  
  inner_tbl_train  <- data$train_data[[1]]
  inner_tbl_assess <- data$assess_data[[1]]
  
  this_param_set   <- filter(outer_fold_id == data$outer_fold_id)
  
  neg_pos_ratio    <- sum(inner_tbl_train$outbreak == 0) / sum(inner_tbl_train$outbreak == 1)
  
  final_spec       <- make_model(scale_pos_weight = neg_pos_ratio) %>% finalize_model(this_param_set)
  
  rec <- make_recipe(inner_tbl_train)
  
  wf  <- workflow() %>% add_model(final_spec) %>% add_recipe(rec)
  
  
  fit(wf, data = train_data)
  
}
  