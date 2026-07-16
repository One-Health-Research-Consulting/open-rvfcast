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
  recipe(outbreak ~ ., data = train_data) |>
    ## Almost always corrected in the data pipeline, but every now and again a cell slips through with a
     ## crazy scaled value, so cap here as a extra precaution (generally has no impact)
    step_mutate(across(starts_with("anomaly_scaled_"), ~ pmin(pmax(.x, -6), 6))) |>
    update_role(all_of(id_cols), new_role = "ID") |>
    step_rm(all_of(id_cols)) |>
    step_zv(all_predictors()) |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
}

#' Build base model scaffold
#'
#'
#' @title make_model

#' @return base model scaffold
#' @param params Set of hyperparameters for this fit
#' @param start_p base_score initialization; should equal empirical prevalence for calibrated probability output
#' @param spw scale_pos_weight passed to xgboost engine (n_neg / n_pos); handles class imbalance
#'   at the gradient level without corrupting min_child_weight semantics via case weights.
#'   max_delta_step = 1 caps per-tree leaf output to prevent margin accumulation to 0/1 extremes.
#' @author Morgan Kain
#' @export

make_model <- function(params, start_p, spw) {

  boost_tree(
    trees          = params$trees
  , tree_depth     = params$tree_depth
  , learn_rate     = params$learn_rate
  , min_n          = params$min_n
  , loss_reduction = params$loss_reduction
  , mtry           = params$mtry
  ) |>
    set_mode("classification") |>
    set_engine(
      "xgboost"
    , objective        = "binary:logistic"
    , base_score       = start_p
    , scale_pos_weight = spw
    , max_delta_step   = 1
    , nthread          = 1
    , verbosity        = 0
    )

}

##### Some helpers ----------------------------------------------------------------

## Memory-efficient metrics calculation
compute_metrics_vec <- function(truth, threshold, weightings, caseweights, prob1, class_hat, event_level = "first") {

  n_pos <- length(which(truth == "1"))
  n_all <- length(truth)
  ## Constant predictions produce a degenerate two-point PR curve whose trapezoidal
  ## AUC equals (1 + prevalence) / 2 regardless of calibration -- appearing near 0.501
  ## for rare events and scoring far above the true no-skill baseline of prevalence.
  ## ROC-AUC handles ties correctly (returns 0.5) so needs no special casing.
  no_discrimination <- diff(range(prob1)) < .Machine$double.eps

  ttib <- tibble(
      n_pos     = n_pos
    , n_all     = n_all
      ## Ranking metrics
      ## Calculate Area Under the Precision-Recall Curve (good for cases where getting
       ## positives correct is important, but still quite sensitive to huge class imbalance).
       ## Problem is that being a ranking metric it is insensitive to magnitude. If all 1s are
       ## predicted with a prob = 0.002 and all 0s predicted with 0.001 the score is perfect.
    , pr_auc    = if (no_discrimination || n_pos == 0) NA_real_ else pr_auc_vec(truth, prob1, event_level = event_level)
      ## Calculate Receiver Operating Characteristic - Area Under the Curve. 
       ## Measures ranking ability averaged over all possible thresholds. Not a great 
       ## metric for large class imbalance. Problem is that with thousands of negatives 
       ## some hundreds of false-positives can barely shift the score for the worse
    , roc_auc   = roc_auc_vec(truth, prob1, event_level = event_level)
      ## Measure of sensitivity (see https://yardstick.tidymodels.org/reference/recall.html)
    , recall    = tibble(
        threshold = threshold
      , recall    = apply(class_hat, 2, FUN = function(x) recall_vec(truth, x |> factor(levels = c("1", "0")), event_level = event_level))
    ) |> list()
      ## See https://yardstick.tidymodels.org/reference/precision.html
    , precision = tibble(
        threshold = threshold
      , precision = apply(class_hat, 2, FUN = function(x) precision_vec(truth, x |> factor(levels = c("1", "0")), event_level = event_level))
    ) |> list()
      ## A way to measure deviation from baseline by explicitly considering magnitude;
       ## potentially a better method for measuring differentiation of estimated 
       ## probabilities for true positives from simply their relative abundance 
       ## (i.e., the starting point for fitting, see argument start_p)
    , logloss     = yardstick::mn_log_loss_vec(truth, prob1)
      ## When true positives are very rare, class *unconditional* log loss can lead to a
      ## scenario where a model that predicts nearly all probabilities small, with little
      ## deviation in probability from the "intercept" (overall relative
      ## abundance of true 1s) gets a good score
       ## Different weighting for ones and zeros to help the class imbalance scoring formula problem.
       ## See finalize_hyperparameters_from_inner for how this is used. Basically allows
       ## positive deviations in probability for true 1s to be rewarded (with the magintude
       ## explicity as compared to roc_auc and to a greater degree than pr_auc)
    , logloss_pos = if (n_pos == 0) NA_real_ else
                    mean(-log(pmax(prob1[truth == "1"], 1e-15)))
    , logloss_neg = mean(-log(pmax(1 - prob1[truth == "0"], 1e-15)))
      ## Another form of logloss where explicitly taking into consideration a weighting
       ## on positives is used
    , logloss_weighted = tibble(
        weighting = weightings
      , precision = apply(weightings |> matrix(), 1, FUN = function(x) {
        tweights <- as.numeric(caseweights)
        tweights <- ifelse(tweights > 1, x, 1)
        yardstick::mn_log_loss_vec(truth, prob1, case_weights = tweights)})
      ) |> list()
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
    splits = splits |> list()
  , ids    = paste("Inner fold", inner_fold_id, sep = " ")
  )

}
