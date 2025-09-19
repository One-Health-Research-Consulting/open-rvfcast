#' Little function to build the model recipe. Model run for a given training dataset
#'
#'
#' @title make_recipe

#' @param final_hyper_set The choice of hyperparameters for final model fitting
#' @param full_data Full training set and test data
#' @param raw_data complete set of raw data
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return Tibble of model fit output
#' @author Morgan Kain
#' @export

fit_model <- function(final_hyper_set, full_data, raw_data, id_cols, out_dir, overwrite) {
  
  ## Set filename
  save_filename <- paste(
    out_dir
    , "/"
    , "model_fit_"
    , full_data$outer_fold_id
    , ".Rds"
    , sep = ""
  )
  
  error_safe_read_file <- possibly(readRDS, NULL)
  
  if (!is.null(error_safe_read_file(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  ## Extract the needed data
  ## 1) full amount of training data (all the stuff from the hyperparameter tuning step)
  outer_tbl_train_train   <- raw_data$train_data[[1]] %>% 
    dplyr::filter(index %in% full_data$train_data[[1]]) %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  
  ## 2) some portion of the data from the left-out period depending on what forecast window
   ## is being predicted
  outer_tbl_train_assess  <- raw_data$test_data[[1]] %>% 
    dplyr::filter(index %in% full_data$train_data[[1]]) %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  
  outer_tbl_train <- rbind(outer_tbl_train_train, outer_tbl_train_assess)
  
  outer_tbl_assess  <- raw_data$test_data[[1]] %>% 
    dplyr::filter(index %in% full_data$assess_data[[1]]) %>% 
    dplyr::select(-c(forecast_interval, cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0)))
  
  ## Establish metric set
  outer_metric_set <- metric_set(mn_log_loss, pr_auc, roc_auc, recall, precision)
  
  ## Get the neg/pos ratio for this outer set
  neg_pos_ratio    <- sum(outer_tbl_train$outbreak == 0) / sum(outer_tbl_train$outbreak == 1)
  
  ## Set up the final model
  final_spec       <- make_model(scale_pos_weight = neg_pos_ratio) %>% finalize_model(final_hyper_set)
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
  model_fit <- tibble(
    fit         = model_fit %>% list()
  , preds       = preds %>% list()
  , hyperparams = final_hyper_set %>% list()
  , metrics     = metric_evals %>% list()
  )
  
  saveRDS(model_fit, save_filename)
  
  return(save_filename)
  
}
  
