#' Little function to build the model recipe. Model run for a given training dataset
#'
#'
#' @title make_recipe

#' @param train_data One set of training data
#' @param id_cols Columns that define a unique data point
#' @return a recipe from package recipe
#' @author Morgan Kain
#' @export

make_recipe <- function(train_data, id_cols) {
  recipe(outbreak ~ ., data = train_data) %>%
    update_role(all_of(id_cols), new_role = "ID") %>%
    step_rm(all_of(id_cols)) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
}

#' Build base model scaffold
#'
#'
#' @title make_model

#' @return base model scaffold 
#' @param params Set of hyperparameters for this fit
#' @param scale_pos_weight weight for positive responses 
#' @author Morgan Kain
#' @export

make_model <- function(params) {
 
  boost_tree(
    trees          = params$trees
  , tree_depth     = params$tree_depth
  , learn_rate     = params$learn_rate
  , min_n          = params$min_n
  , loss_reduction = params$loss_reduction
  , mtry           = params$mtry
  ) %>%
    set_mode("classification") %>%
    set_engine(
      "xgboost"
    , objective = "binary:logistic"
    , nthread   = 1
    ) 
  
}

##### Some helpers ----------------------------------------------------------------

## Memory-efficient metrics calculation
compute_metrics_vec <- function(truth, threshold, weightings, caseweights, prob1, class_hat, event_level = "first") {

  n_pos <- length(which(truth == "1"))
  n_all <- length(truth)
  p     <- n_pos / n_all
  q     <- 1 - p
  logloss_baseline <- if(p == 0){0}else{-(p*log(p) + q*log(q))}
  
  ttib <- tibble(
      n_pos     = n_pos
    , n_all     = n_all
    , pr_auc    = pr_auc_vec(truth, prob1, event_level = event_level)
    , roc_auc   = roc_auc_vec(truth, prob1, event_level = event_level)
    , recall    = tibble(
        threshold = threshold
      , recall    = apply(class_hat, 2, FUN = function(x) recall_vec(truth, x %>% factor(levels = c("1", "0")), event_level = event_level))
    ) %>% list()
    , precision = tibble(
        threshold = threshold
      , precision = apply(class_hat, 2, FUN = function(x) precision_vec(truth, x %>% factor(levels = c("1", "0")), event_level = event_level))
    ) %>% list()
    , logloss          = yardstick::mn_log_loss_vec(truth, prob1)
    , logloss_weighted = tibble(
        weighting = weightings
      , precision = apply(weightings %>% matrix(), 1, FUN = function(x) {
        tweights <- as.numeric(caseweights)
        tweights <- ifelse(tweights > 1, x, 1)
        yardstick::mn_log_loss_vec(truth, prob1, case_weights = tweights)})
      ) %>% list()
  ) %>% mutate(
    exlogloss   = max(0, logloss - logloss_baseline)
  )
 
  ttib
  
}

calc_spw <- function(df, outcome = "outbreak") {
  y     <- as.character(df[[outcome]])
  n_pos <- sum(y == "1", na.rm = TRUE)
  n_neg <- sum(y == "0", na.rm = TRUE)
  if (n_pos == 0) return(1)
  n_neg / n_pos
}

##### ------------------------------------------------------------------------------------

#' Port the manual splits into a tidymodels object 
#'
#'
#' @title build_inner_rset

#' @return base model scaffold 
#' @param inner_train inner fold training data (spatial regions left in)
#' @param inner_asses inner fold assess data (left out spatial region)
#' @param outer_train full set of data for the given outer fold
#' @param id_cols Columns that define a unique data point
#' @param inner_fold_id which of the inner folds is this split
#' @author Morgan Kain
#' @export

build_inner_rset <- function(inner_train, inner_assess, outer_train, id_cols, inner_fold_id) {
  
  ## Quick check for consistency between outer and inner folds
  stopifnot(all(id_cols %in% names(outer_train)))
  
  ## Steps to map rows of inner folds training and assess back to the complete outer folds data
  keyfun    <- function(df) paste(df[[id_cols[1]]], df[[id_cols[2]]], sep = "||")
  outer_key <- keyfun(outer_train)
  tr_idx    <- match(keyfun(inner_train), outer_key)
  te_idx    <- match(keyfun(inner_assess), outer_key)
  tr_idx    <- tr_idx[!is.na(tr_idx)]
  te_idx    <- te_idx[!is.na(te_idx)]
  splits    <- rsample::make_splits(list(analysis = tr_idx, assessment = te_idx), outer_train)
  
  rsample::manual_rset(
    splits = splits %>% list()
  , ids    = paste("Inner fold", inner_fold_id, sep = " ")
  )
  
}
