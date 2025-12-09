#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_per_outer_fold

#' @param folded_data one row of the folded data
#' @param inner_ids All needed fits created with prep_fold_ids
#' @param raw_data complete set of raw data
#' @param threshold For assigning a 1 | estimated prob
#' @param weightings weight assigned to 1s in binomial loss metric 
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @param DEBUG If TRUE reduce to a small dataset for code testing
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold <- function(folded_data, inner_ids, raw_data, threshold
                                      , weightings, id_cols, out_dir, overwrite, DEBUG) {

  folded_data <- folded_data %>% filter(outer_fold_id == inner_ids$outer_fold_id)
  inner_id    <- inner_ids$inner_fold_id
  tuning_grid <- inner_ids %>% dplyr::select(-contains("fold_id"))
  
  ## Inner training data: exclude a cluster
  inner_tbl_train <- folded_data$inner_folds[[1]] %>% 
    left_join(., raw_data, by = "index") %>%
    dplyr::filter(cluster != inner_id) %>%
    relocate(cluster, .after = "date") %>%
    dplyr::select(-c(cluster, cases)) %>%
    mutate(outbreak = as.factor(outbreak)) %>% 
    mutate(forecast_interval = as.factor(forecast_interval)) %>% 
    group_by(forecast_interval) %>% 
    ## Get class imbalance ratio per forecast horizon
    mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
    , weights = ifelse(outbreak == "0", 1, weights)
    , weights = hardhat::importance_weights(weights)
    , .after = "index"
    )
  
  ## Inner test data: extract one cluster
  inner_tbl_assess <- folded_data$inner_folds[[1]] %>% 
    left_join(., raw_data, by = "index") %>%
    dplyr::filter(cluster == inner_id) %>%
    relocate(cluster, .after = "date") %>%
    dplyr::select(-c(cluster, cases)) %>%
    mutate(outbreak = as.factor(outbreak)) %>% 
    mutate(forecast_interval = as.factor(forecast_interval)) %>% 
    ## Get class imbalance ratio per forecast horizon
    mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
    , weights = ifelse(outbreak == "0", 1, weights)
    , weights = hardhat::importance_weights(weights)
    , .after = "index"
    )
  
  if (DEBUG) { inner_tbl_train  <- inner_tbl_train[1:10000, ] }
  
  ## Attempt to clear some ram
  rm(raw_data); gc()
  
  ## Set filename
  save_filename <- paste(
      out_dir
    , "/"
    , "inner_tuning_"
    , "outer_fold_"
    , paste(folded_data$outer_fold_id, collapse = "_")
    , "_inner_fold_"
    , inner_id
    , "_tune_grid_"
    ,  tuning_grid$index
    , ".Rds"
    , sep = ""
  )
  
  error_safe_read_file <- possibly(readRDS, NULL)
  
  if (!is.null(error_safe_read_file(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }

  ## Create scaffold recipe + model + workflow and fit model
  rec <- make_recipe(inner_tbl_train, id_cols = id_cols)
  mod <- make_model(params = tuning_grid)
  wf  <- workflow() %>% add_model(mod) %>% add_recipe(rec) %>% add_case_weights(weights)
  
  print("At model fitting")
  fit <- fit(wf, data = inner_tbl_train)
  print("Finished with model fitting")
  
  ## Predictions: prob only
  prob1     <- predict(fit, inner_tbl_assess, type = "prob")$.pred_1
  truth     <- factor(inner_tbl_assess[["outbreak"]], levels = c("1","0"))
  class_hat <- apply(
    threshold %>% matrix()
  , 1
  , FUN = function(x) factor(ifelse(prob1 >= x, "1", "0"), levels = c("1","0"))
  )
  all_intervals     <- inner_tbl_assess$forecast_interval
  forecast_interval <- all_intervals %>% unique() %>% as.character() %>% as.numeric() %>% sort()
  
  ## Compute metrics 
  metrics <- purrr:::map(forecast_interval, .f = function(this_int) {
    
    truth.t     <- truth[which(all_intervals == this_int)]
    prob1.t     <- prob1[which(all_intervals == this_int)]
    class_hat.t <- class_hat[which(all_intervals == this_int), ]
    
    metrics.t <- compute_metrics_vec(
      truth       = truth.t
    , threshold   = threshold
    , weightings  = weightings
    , caseweights = inner_tbl_assess %>% filter(forecast_interval == this_int) %>% pull(weights)
    , prob1       = prob1.t
    , class_hat   = class_hat.t
    , event_level = "first"
  ) %>% mutate(
      outer_fold_id = folded_data$outer_fold_id
    , inner_fold_id = inner_id
    , interval      = this_int
    , .before       = 1 
    ) %>% bind_cols(., tuning_grid)
    
  }) %>% do.call("rbind", .)

  saveRDS(metrics, save_filename)
  
  return(save_filename)
  
}

#' Finalize inner folds for all outer folds
#'
#'
#' @title prep_fold_ids

#' @param folded_data one row of the folded data
#' @param raw_data complete set of raw data
#' @return Tibble of inner folds per outer fold that have an outbreak
#' @author Morgan Kain
#' @export

prep_fold_ids  <- function(folded_data, raw_data) {
  
  all_inner_outer <- purrr::map(1:nrow(folded_data), function(outid) {
    
    ## Extract the needed data
    all_inner <- folded_data[outid, ]$inner_folds[[1]] %>% left_join(., raw_data$train_data[[1]], by = "index")
    
    ## Build the set of all inner train and assess datasets
    inner_tbl_set <- purrr::map(seq_along(unique(all_inner$cluster)), function(clust) {
      
      ## Inner assess data: only the left-out cluster
      assess_inner <- all_inner %>%
        dplyr::filter(cluster == clust) %>%
        relocate(cluster, .after = "date") %>%
        dplyr::select(-c(cluster, forecast_interval, cases)) %>% 
        summarize(
          nrow = n()
        , tot_out = sum(outbreak))
      
      tibble(
        inner_fold_id = clust
      , nrow          = assess_inner$nrow
      , assess_inner  = assess_inner$tot_out
      )
      
    }) %>% 
      dplyr::bind_rows() %>% filter(nrow > 0)
    
    inner_tbl_set %>% mutate(outer_fold_id = folded_data[outid, ]$outer_fold_id, .before = 1)
    
  }) %>% do.call("rbind", .) 
  
  all_inner_outer
  
}
prep_outer_ids <- function(folded_data, raw_data, inner_ids) {
  
  all_outer <- purrr::map(1:nrow(folded_data), function(outid) {
    
    ## Extract the needed data and check for 1s in outbreak
    all_asses <- raw_data$train_data[[1]] %>% 
      filter(index %in% folded_data[outid, ]$assess_data[[1]]) %>%
      summarize(n_out = n_distinct(outbreak)) %>%
      mutate(outer_fold_id = folded_data[outid, ]$outer_fold_id, .before = 1) %>%
      mutate(has_outbreak = ifelse(n_out > 1, 1, 0)) %>% 
      dplyr::select(-n_out)
      
    all_asses

  }) %>% do.call("rbind", .)
  
 # inner_ids %>% left_join(., all_outer) %>% filter(has_outbreak == 1) %>% dplyr::select(-has_outbreak)
  inner_ids %>% left_join(., all_outer)
}


#' Load in and combine saved output from all inner folds
#'
#'
#' @title join_tuned_inner_folds

#' @param inner_folds list of file paths for all tuned hyperparameter sets across all inner folds of all outer folds
#' @param training_dat The training data
#' @param metric which metric is being optimized
#' @param weightval how to scale the weighting on the metric on 1s
#' @param direction min or max
#' @return Tibble of best parameter sets
#' @author Morgan Kain
#' @export

join_tuned_inner_folds <- function(inner_folds, training_dat, metric, weightval, direction) {
  
  metric_summary <- apply(inner_folds %>% matrix(), 1, FUN = function(x) {
    readRDS(x)
  }) %>% do.call("rbind", .) 
  
  if (metric == "mix") {
    metric_summary.s <- metric_summary %>%
      mutate(
        weight = if_else(n_pos > 0, n_pos, n_all)
      , contrib = case_when(
          n_pos > 0 ~ pr_auc * n_pos
        , n_pos == 0 ~ -weightval * exlogloss * n_all
        )) %>%
      group_by(outer_fold_id, index) %>%
      summarise(
        final_score = sum(contrib, na.rm = TRUE) / sum(weight, na.rm = TRUE)
      , .groups     = "drop"
      )
  } else if (metric == "logloss") {
    metric_summary.s <- metric_summary %>% 
      dplyr::select(outer_fold_id, logloss_weighted, index) %>% 
      unnest(cols = c(logloss_weighted)) %>%
      filter(weighting == weightval) %>%
      group_by(outer_fold_id, index) %>%
      summarise(
        final_score = max(precision)
      , .groups     = "drop"
      )
  } else {
    stop("Choose mix or logloss for metric")
  }
  
  if (direction == "min") {
    if (metric %in% c("mix", "logloss")) {
      stop("Using either of these metrics and min does not make sense, use max")
    }
    metric_summary.s2 <- metric_summary.s %>% 
      group_by(outer_fold_id) %>% 
      arrange(final_score) %>% 
      dplyr::slice(1) %>%
      ungroup()
  } else if (direction == "max") {
    metric_summary.s2 <- metric_summary.s %>% 
      group_by(outer_fold_id) %>% 
      arrange(desc(final_score)) %>% 
      dplyr::slice(1) %>%
      ungroup() 
  } else {
    stop("choose min or max for direction")
  }
  
  metric_summary.s2 %>% 
    left_join(
      .
    , metric_summary %>% dplyr::select(
       -interval, -contains("fold_id"), -contains("auc")
     , -recall, -precision, -contains("log"), -n_pos, -n_all
     ) %>% distinct()
    ) %>% mutate(
      metric = metric, weightval = weightval, .before = "index"
    )
  
}


#' Conduct the tuning over the inner folds for a given outer fold
#'
#'
#' @title tune_results_across_outer_folds

#' @param outer_data an outer fold
#' @param raw_data complete set of raw data
#' @param threshold For assigning a 1 | estimated prob
#' @param weightings weight assigned to 1s in binomial loss metric 
#' @param hyperparm_sets maximized hyperparameter sets across all inner folds of all outer folds
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @param DEBUG If TRUE reduce to a small dataset for code testing
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

tune_results_across_outer_folds <- function(outer_data, raw_data, threshold, weightings
                                          , hyperparm_sets, id_cols, out_dir, overwrite, DEBUG) {
  
  ## The best hyperparameter set for this outer fold across all inner folds for this outer fold
  hyper_set <- hyperparm_sets %>% dplyr::filter(outer_fold_id == outer_data$outer_fold_id)
  
  ## Set filename
  save_filename <- paste(
      out_dir
    , "/"
    , "outer_tuning_"
    , outer_data$outer_fold_id
    , ".Rds"
    , sep = ""
  )
  
  error_safe_read_file <- possibly(readRDS, NULL)
  
  if (!is.null(error_safe_read_file(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  ## Extract the needed data
  outer_tbl_train   <- raw_data$train_data[[1]] %>% 
    dplyr::filter(index %in% outer_data$train_data[[1]]) %>% 
    dplyr::select(-c(cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0))) %>% 
    mutate(forecast_interval = as.factor(forecast_interval)) %>%
    ## Get class imbalance ratio per forecast horizon
    group_by(forecast_interval) %>% 
    mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
      , weights = ifelse(outbreak == "0", 1, weights)
      , weights = hardhat::importance_weights(weights)
      , .after = "index"
    )
  
  outer_tbl_assess  <- raw_data$train_data[[1]] %>% 
    dplyr::filter(index %in% outer_data$assess_data[[1]]) %>% 
    dplyr::select(-c(cases)) %>%
    mutate(outbreak = factor(outbreak, levels = c(1, 0))) %>% 
    mutate(forecast_interval = as.factor(forecast_interval)) %>% 
    mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
    , weights = ifelse(outbreak == "0", 1, weights)
    , weights = hardhat::importance_weights(weights)
    , .after = "index"
    )
  
  if (DEBUG) { outer_tbl_train  <- outer_tbl_train[1:10000, ] }
  
  ## Clear up some ram
  rm(raw_data); gc()
  
  ## Set up and fit the final model for this outer fold
  rec <- make_recipe(outer_tbl_train, id_cols = id_cols)
  mod <- make_model(params = hyper_set)
  wf  <- workflow() %>% add_model(mod) %>% add_recipe(rec) %>% add_case_weights(weights)
  fit <- fit(wf, data = outer_tbl_train)
  
  ## Predictions: prob only
  prob1     <- predict(fit, outer_tbl_assess, type = "prob")$.pred_1
  truth     <- factor(outer_tbl_assess[["outbreak"]], levels = c("1","0"))
  class_hat <- apply(
    threshold %>% matrix()
  , 1
  , FUN = function(x) factor(ifelse(prob1 >= x, "1", "0"), levels = c("1","0"))
  )
  all_intervals     <- outer_tbl_assess$forecast_interval
  forecast_interval <- all_intervals %>% unique() %>% as.character() %>% as.numeric() %>% sort()
  
  ## Compute metrics 
  metrics <- purrr:::map(forecast_interval, .f = function(this_int) {
    
    truth.t     <- truth[which(all_intervals     == this_int)]
    prob1.t     <- prob1[which(all_intervals     == this_int)]
    class_hat.t <- class_hat[which(all_intervals == this_int), ]
    
    metrics.t <- compute_metrics_vec(
        truth       = truth.t
      , threshold   = threshold
      , weightings  = weightings
      , caseweights = outer_tbl_assess %>% filter(forecast_interval == this_int) %>% pull(weights)
      , prob1       = prob1.t
      , class_hat   = class_hat.t
      , event_level = "first"
    ) %>% mutate(
      outer_fold_id = outer_data$outer_fold_id
    , interval      = this_int
    , .before       = 1 
    ) %>% 
      left_join(., hyper_set)
    
  }) %>% do.call("rbind", .)
  
  saveRDS(metrics, save_filename)
  
  return(save_filename)
  
}


#' Across all outer folds select the single best hyperparameter set for fitting the complete data
#'
#'
#' @title finalize_hyperparameters

#' @param outer_folds Tibble of output across all outer folds 
#' @param training_dat The training data
#' @param metric Metric used for selection
#' @param weightval how to scale the weighting on the metric on 1s
#' @param direction min or max
#' @return Set of best hyperparameters
#' @author Morgan Kain
#' @export

finalize_hyperparameters <- function(outer_folds, training_dat, metric, weightval, direction) { 
  
  joined_files <- apply(outer_folds %>% matrix(), 1, FUN = function(x) { readRDS(x) }) 
  
  if (length(joined_files) > 1) {
    joined_files <- joined_files %>% do.call("rbind", .)
  } else {
    joined_files <- joined_files[[1]]
  }
  
    ## Removing the score that was used to choose the hyperparameter set from the inner folding step
    joined_files <- joined_files %>% dplyr::select(-final_score)
  
  ## Get waited metric
  if (metric == "mix") {
    metric_summary.s <- joined_files %>%
      mutate(
        weight = ifelse(n_pos > 0, n_pos, n_all)
      , contrib = case_when(
        n_pos > 0 ~ pr_auc * n_pos
      , n_pos == 0 ~ -weightval * exlogloss * n_all
        )) %>%
      group_by(index) %>%
      summarise(
        final_score = sum(contrib, na.rm = TRUE) / sum(weight, na.rm = TRUE)
      , .groups     = "drop"
      )
  } else if (metric == "logloss") {
    metric_summary.s <- joined_files %>% 
      dplyr::select(outer_fold_id, logloss_weighted, index) %>% 
      unnest(cols = c(logloss_weighted)) %>%
      filter(weighting == weightval) %>%
      group_by(index) %>%
      summarise(
        final_score = max(precision)
      , .groups     = "drop"
      )
  } else {
    stop("Choose mix or logloss for metric")
  }
  
  if (direction == "min") {
    if (metric %in% c("mix", "logloss")) {
      stop("Using either of these metrics and min does not make sense, use max")
    }
    metric_summary.s2 <- metric_summary.s %>% 
      arrange(final_score) %>% 
      dplyr::slice(1) %>%
      ungroup()
  } else if (direction == "max") {
    metric_summary.s2 <- metric_summary.s %>% 
      arrange(desc(final_score)) %>% 
      dplyr::slice(1)
  } else {
    stop("choose min or max for direction")
  }
  
  metric_summary.s2 %>% 
    left_join(
      .
      , joined_files %>% dplyr::select(
        -interval, -contains("fold_id"), -contains("auc")
        , -recall, -precision, -contains("log"), -n_pos, -n_all
      ) %>% distinct()
    ) %>% mutate(
      metric = metric, weightval = weightval, .before = "index"
    )

}

  