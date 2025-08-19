#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_per_outer_fold

#' @param inner_data all inner folds for a given outer fold
#' @param outer_data outer folds
#' @param tuning_grid set of potential hyperparameters
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold <- function(inner_data, outer_data, tuning_grid, id_cols, out_dir, overwrite) {

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
  
  ## Set filename
  save_filename <- paste(
      out_dir
    , "/"
    , "inner_tuning_"
    , paste(c(inner_data$outer_fold_id, inner_data$inner_fold_id), collapse = "_")
    , ".csv"
    , sep = ""
  )
  
  error_safe_read_file <- possibly(read.csv, NULL)
  
  if (!is.null(error_safe_read_file(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
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
  this_best <- best %>% mutate(
     outer_fold_id = inner_data$outer_fold_id
   , inner_fold_id = inner_data$inner_fold_id
   , .before = 1
  )
  
  write.csv(this_best, save_filename)
  
  return(save_filename)
  
}


#' Load in and combine saved output from all inner folds
#'
#'
#' @title join_tuned_inner_folds

#' @param inner_folds list of file paths for all tuned hyperparameter sets across all inner folds of all outer folds
#' @return Tibble of best parameter sets
#' @author Morgan Kain
#' @export

join_tuned_inner_folds <- function(inner_folds) {
  
  joined_files <- apply(inner_folds %>% matrix(), 1, FUN = function(x) {
    read.csv(x)
  }) %>% do.call("rbind", .) %>% 
    dplyr::select(-X) %>%
    group_by(outer_fold_id) %>% 
    filter(loss_reduction == min(loss_reduction)) %>%
    dplyr::slice(1) %>%
    ungroup()
  
  joined_files
  
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
  
  all_out <- lapply(data %>% split_tibble(., "outer_fold_id"), FUN = function(this_outer) {
  
  ## Extract the needed data
  outer_tbl_train  <- this_outer$train_data[[1]] %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  outer_tbl_assess <- this_outer$assess_data[[1]] %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  
  ## Establish metric set
  outer_metric_set <- metric_set(mn_log_loss, pr_auc, roc_auc, recall, precision)
  
  ## Select the param set for this outer fold
  best_set <- hyperparm_sets %>% filter(outer_fold_id == this_outer$outer_fold_id)
  
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
    outer_fold  = this_outer$outer_fold_id
  , fit         = model_fit %>% list()
  , hyperparams = best_set %>% list()
  , metrics     = metric_evals %>% list()
  )
  
  }) %>% do.call("rbind", .)
  
  return(all_out)
  
}


#' Across all outer folds select the single best hyperparameter set for fitting the complete data
#'
#'
#' @title finalize_hyperparameters

#' @param outer_folds Tibble of output across all outer folds 
#' @param chosen_metric Metric used for selection
#' @return Set of best hyperparameters
#' @author Morgan Kain
#' @export

finalize_hyperparameters <- function(outer_folds, chosen_metric) { 
  
  opt_set <- lapply(outer_folds %>% split_tibble(., "outer_fold"), FUN = function(this_outer) {
  
    cbind(
      this_outer$hyperparams[[1]]
    , this_outer$metrics[[1]] %>% filter(.metric == chosen_metric)
    )
    
  }) %>% do.call("rbind", .) %>% 
    arrange(desc(.estimate)) %>% 
    slice(1)
  
  return(opt_set)
  
}

  