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
    step_dummy(all_nominal_predictors())
}

#' Build base model scaffold
#'
#'
#' @title make_model

#' @return base model scaffold 
#' @param scale_pos_weight weight for positive responses if desired, default is null
#' @author Morgan Kain
#' @export

make_model <- function(scale_pos_weight = NULL) {
 
  boost_tree(
      trees          = tune()
    , tree_depth     = tune()
    , learn_rate     = tune()
    , min_n          = tune()
    , loss_reduction = tune()
    , mtry           = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine(
      "xgboost"
    , objective = "binary:logistic"
    ## Can set later via finalize_model or pass ratio
    , scale_pos_weight = NULL
    ) 
  
}


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
