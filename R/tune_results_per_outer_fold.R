#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_per_outer_fold

#' @param folded_data one row of the folded data
#' @param inner_ids seq of 1:n_spatial_folds
#' @param raw_data complete set of raw data
#' @param tuning_grid set of potential hyperparameters
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold <- function(folded_data, inner_ids, raw_data
                                        , tuning_grid, id_cols, out_dir, overwrite) {

  inner_ids <- inner_ids$inner_fold_id
  
  ## Extract the needed data
  all_inner <- folded_data$inner_folds[[1]] %>% 
    left_join(., raw_data$train_data[[1]], by = "index")
  
  ## Inner training data: exclude a cluster
  train_inner <- all_inner %>%
    dplyr::filter(cluster != inner_ids) %>%
    relocate(cluster, .after = "date") %>%
    dplyr::select(-c(cluster, forecast_interval, cases)) %>%
    mutate(outbreak = as.factor(outbreak))
  
  ## Inner test data: extract one cluster
  assess_inner <- all_inner %>%
    dplyr::filter(cluster == inner_ids) %>%
    relocate(cluster, .after = "date") %>%
    dplyr::select(-c(cluster, forecast_interval, cases)) %>%
    mutate(outbreak = as.factor(outbreak))
  
  inner_tbl_set <- tibble(
    inner_fold_id = inner_ids
  , train_inner   = list(train_inner)
  , assess_inner  = list(assess_inner)
  )

  outer_tbl_train  <- raw_data$train_data[[1]] %>% 
    dplyr::filter(index %in% folded_data$train_data[[1]]) %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = as.factor(outbreak))
  
  ## Set filename
  save_filename <- paste(
      out_dir
    , "/"
    , "inner_tuning_"
    , "outer_fold_"
    , paste(folded_data$outer_fold_id, collapse = "_")
    , "_inner_fold_"
    , inner_ids
    , "_tune_grid_"
    ,  tuning_grid$index
    , ".csv"
    , sep = ""
  )
  
  error_safe_read_file <- possibly(read.csv, NULL)
  
  if (!is.null(error_safe_read_file(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  ## For the inner folds for this outer fold do the fitting for one set of hyperparameters
  inner_tbl_train  <- inner_tbl_set$train_inner[[1]] 
  inner_tbl_assess <- inner_tbl_set$assess_inner[[1]]
  inner_fold_id    <- inner_tbl_set$inner_fold_id
    
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
   , inner_fold_id = inner_fold_id
  )
    
  ## Initial workflow scaffold
  wf <- workflow() %>% add_model(mod) %>% add_recipe(rec)
    
  ## Establish metric set
  inner_metric_set <- metric_set(mn_log_loss)
    
  ## Tune over inner folds
  tuned <- tune_grid(
    wf
  , resamples = inner_splits
  , grid      = tuning_grid %>% dplyr::select(-index)
  , metrics   = inner_metric_set
  , control   = control_grid(save_pred = TRUE)
  ) %>% 
  ## For memory purposes throw out all of the memory intensive splits and predictions and just
  ## Retain metrics + the full parameter set + config id. Can recreate the predictions later for
  ## just the retained
  collect_metrics(summarize = TRUE) %>%
  ## Finally, add the ID columns for this cross
  mutate(
    outer_fold_id = folded_data$outer_fold_id
  , inner_fold_id = inner_ids
  , .before = 1
  )
  
  write.csv(tuned, save_filename)
  
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
    filter(mean == min(mean)) %>%
    dplyr::slice(1) %>%
    ungroup()
  
  joined_files
  
}


#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_across_outer_folds

#' @param outer_data an outer fold
#' @param raw_data complete set of raw data
#' @param hyperparm_sets maximized hyperparameter sets across all inner folds of all outer folds
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_across_outer_folds <- function(outer_data, raw_data, hyperparm_sets, id_cols, out_dir, overwrite) {
  
  ## The best hyperparameter set for this outer fold across all inner folds for this outer fold
  hyper_set <- hyperparm_sets %>% dplyr::filter(outer_fold_id == outer_data$outer_fold_id)
  
  ## Set filename
  save_filename <- paste(
      out_dir
    , "/"
    , "outer_tuning_"
    , outer_data$outer_fold_id
    , ".csv"
    , sep = ""
  )
  
  error_safe_read_file <- possibly(read.csv, NULL)
  
  if (!is.null(error_safe_read_file(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  ## Extract the needed data
  outer_tbl_train   <- raw_data$train_data[[1]] %>% 
    dplyr::filter(index %in% outer_data$train_data[[1]]) %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  outer_tbl_assess  <- raw_data$train_data[[1]] %>% 
    dplyr::filter(index %in% outer_data$assess_data[[1]]) %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  
  ## Establish metric set
  outer_metric_set <- metric_set(mn_log_loss, pr_auc, roc_auc, recall, precision)
  
  ## Get the neg/pos ratio for this outer set
  neg_pos_ratio    <- sum(outer_tbl_train$outbreak == 0) / sum(outer_tbl_train$outbreak == 1)
  
  ## Set up the final model
  final_spec       <- make_model(scale_pos_weight = neg_pos_ratio) %>% finalize_model(hyper_set)
  rec              <- make_recipe(outer_tbl_train, id_cols = id_cols)
  wf               <- workflow() %>% add_model(final_spec) %>% add_recipe(rec) 
  
  ## Fit
  model_fit        <- fit(wf, data = outer_tbl_train)
  
  ## Predict probabilities and class labels on outer assessment
  preds <- predict(model_fit, outer_tbl_assess, type = "prob") %>%
    bind_cols(predict(model_fit, outer_tbl_assess, type = "class")) %>%
    bind_cols(outer_tbl_assess %>% select(outbreak)) %>%
    mutate(
      outbreak   = factor(outbreak, levels = c("1", "0"))
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
  
  outer_out <- hyper_set %>% 
    mutate(outer_fold = outer_data$outer_fold_id, .before = 1) %>%
    cbind(
      .
    , metric_evals %>% 
      pivot_wider(id_cols = .estimator, values_from = .estimate, names_from = .metric) %>% 
      dplyr::select(-.estimator))
  
  write.csv(outer_out, save_filename)
  
  return(save_filename)
  
}


#' Across all outer folds select the single best hyperparameter set for fitting the complete data
#'
#'
#' @title finalize_hyperparameters

#' @param outer_folds Tibble of output across all outer folds 
#' @param chosen_metric Metric used for selection
#' @param direction maximize or minimize corresponding to chosen_metric
#' @return Set of best hyperparameters
#' @author Morgan Kain
#' @export

finalize_hyperparameters <- function(outer_folds, chosen_metric, direction) { 
  
  joined_files <- apply(outer_folds %>% matrix(), 1, FUN = function(x) {
    read.csv(x)
  }) %>% do.call("rbind", .) %>% 
    dplyr::select(-X)
  
  if (direction == "maximize") {
    joined_files %>% arrange(desc(get(chosen_metric))) %>% dplyr::slice(1)
  } else if (direction == "minimize") {
    joined_files %>% arrange(get(chosen_metric)) %>% dplyr::slice(1)
  } else {
    stop("choose minimize or maximize for direction")
  }

}

  