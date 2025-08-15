#' Little function to build the model recipe. Model run for a given training dataset
#'
#'
#' @title make_recipe

#' @param train_data One set of training data
#' @return a recipe from package recipe
#' @author Morgan Kain
#' @export

make_recipe <- function (train_data) {
  recipe(outbreak ~ ., data = train_data) %>%
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

