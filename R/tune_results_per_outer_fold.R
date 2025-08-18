#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_per_outer_fold

#' @param inner_data all inner folds for a given outer fold
#' @param outer_data outer folds
#' @param tuning_grid set of potential hyperparameters
#' @param id_cols Columns that define a unique data point
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold <- function(inner_data, outer_data, tuning_grid, id_cols) {

  ## Extract the needed data
  inner_tbl_train  <- inner_data$train_inner[[1]] %>% 
    dplyr::select(-c(cluster, forecast_interval, cases)) %>%
    mutate(outbreak = as.factor(outbreak))
  inner_tbl_assess <- inner_data$assess_inner[[1]] %>% 
    dplyr::select(-c(cluster, forecast_interval, cases)) %>%
    mutate(outbreak = as.factor(outbreak))
  outer_tbl_train  <- (outer_data %>% 
    filter(outer_fold_id == inner_data$outer_fold_id) %>%
    pull(train_data))[[1]]
  outer_tbl_train <- outer_tbl_train %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = as.factor(outbreak))
  
  ## Get class imbalance ratio
  neg_pos_ratio <- sum(inner_tbl_train$outbreak == 0) / sum(inner_tbl_train$outbreak == 1)
  
  ## Create scaffold recipe + model
  rec <- make_recipe(inner_tbl_train, id_cols = id_cols)
  mod <- make_model(scale_pos_weight = neg_pos_ratio)
  
  ## create rsample object from inner folds
  inner_splits <- build_inner_rset(
    inner_train   = inner_tbl_train
  , inner_assess  = inner_tbl_assess
  , outer_train   = outer_tbl_train
  , id_cols       = id_cols
  , inner_fold_id = inner_data$inner_fold_id
  )
  
  ## Initial workflow scaffold
  wf <- workflow() %>% add_model(mod) %>% add_recipe(rec)
  
  ## Establish metric set
  inner_metric_set <- metric_set(mn_log_loss)
  
  ## Tune over inner folds
  tuned <- tune_grid(
    wf
  , resamples = inner_splits
  , grid      = tuning_grid
  , metrics   = inner_metric_set
  , control   = control_grid(save_pred = TRUE)
  )
  
  ## Select best for this outer fold
  best <- select_best(tuned, metric = "mn_log_loss")
  
  ## Return the best for this inner fold
  best %>% mutate(
     outer_fold_id = inner_data$outer_fold_id
   , inner_fold_id = inner_data$inner_fold_id
   , .before = 1
  )
  
}


#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_across_outer_folds

#' @param data outer fold
#' @param hyperparm_sets maximized hyperparameter sets across all inner folds of all outer folds
#' @param id_cols Columns that define a unique data point
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_across_outer_folds <- function(data, hyperparm_sets, id_cols) {
  
  ## Extract the needed data
  outer_tbl_train  <- data$train_data[[1]] %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  outer_tbl_assess <- data$assess_data[[1]] %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  
  ## Establish metric set
  outer_metric_set <- metric_set(mn_log_loss, pr_auc, roc_auc, recall, precision)
  
  ## Select the param set for this outer fold
  this_param_set   <- hyperparm_sets %>% filter(outer_fold_id == outer_tbl_train)
  best_set         <- this_param_set %>% arrange(loss_reduction) %>% dplyr::slice(1) %>%
    dplyr::select(-outer_fold_id, inner_fold_id)
  
  ## Get the neg/pos ratio for this outer set
  neg_pos_ratio    <- sum(outer_tbl_train$outbreak == 0) / sum(outer_tbl_train$outbreak == 1)
  
  ## Set up the final model
  final_spec       <- make_model(scale_pos_weight = neg_pos_ratio) %>% finalize_model(best_set)
  rec              <- make_recipe(outer_tbl_train, id_cols = id_cols)
  wf               <- workflow() %>% add_model(final_spec) %>% add_recipe(rec) 
  
  ## Fit
  model_fit        <- fit(wf, data = outer_tbl_train)
  
  ## Predict probabilities and class labels on outer assessment
  preds <- predict(model_fit, outer_tbl_assess, type = "prob") %>%
    bind_cols(predict(model_fit, outer_tbl_assess, type = "class")) %>%
    bind_cols(outer_tbl_assess %>% select(outbreak)) %>%
    mutate(
      outbreak    = factor(outbreak, levels = c("1", "0"))
    , .pred_class = factor(.pred_class, levels = c("1", "0"))
    )
  
  ## Evaluate metrics on assessment data
  metric_evals <- outer_metric_set(
    data        = preds
  , truth       = outbreak
  , estimate    = .pred_class
  , .pred_1   
  , event_level = "first" 
  )
  
  ## Return
  tibble(
    fit     = model_fit %>% list()
  , metrics = metric_evals %>% list()
  )
  
}
  