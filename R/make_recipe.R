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
#'   Damped by params$spw_multiplier when present (see build_local_hyperparameter_grid) --
#'   scale_pos_weight set to the raw imbalance ratio is a known source of overconfident
#'   predicted probabilities; a tunable multiplier lets the local hyperparameter search
#'   trade off some of that against calibration.
#' @param int_con Interaction constraints as xgboost's list of 0-based column positions, or
#'   NULL for none. Build it with fit_constrained_workflow() rather than by hand
#' @author Morgan Kain
#' @export

make_model <- function(params, start_p, spw, int_con = NULL) {

  spw_mult <- resolve_spw_multiplier(params)

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
    , scale_pos_weight = spw * spw_mult
    , max_delta_step   = 1
    , nthread          = 1
    , verbosity        = 0
    , interaction_constraints = int_con
    )

}

#' Resolve a hyperparameter row's spw_multiplier, defaulting to 1 (no damping)
#'
#' params$spw_multiplier is only present on rows that came from the local
#' refinement grid; absent (NULL) on global-grid rows, and can also come through 
#' as NA elsewhere in the tuning pipeline. Shared by make_model() and 
#' tune_results_per_outer_fold()'s save_raw_predictions logic so both compute 
#' the same effective scale_pos_weight.
#'
#' @title resolve_spw_multiplier
#'
#' @param params Set of hyperparameters for this fit (may or may not have an
#'   spw_multiplier column)
#' @return Single numeric: params$spw_multiplier if present and non-NA, else 1
#' @author Morgan Kain
#' @export

resolve_spw_multiplier <- function(params) {
  spw_mult <- suppressWarnings(params$spw_multiplier)
  if (is.null(spw_mult) || length(spw_mult) == 0 || is.na(spw_mult)) spw_mult <- 1
  spw_mult
}

##### Some helpers ----------------------------------------------------------------

## Memory-efficient metrics calculation
compute_metrics_vec <- function(
    truth
  , threshold
  , weightings
  , caseweights
  , prob1
  , class_hat
  , event_level = "first"
  , index_flag = NULL) {

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
      ## Count of country-level index-case rows in this call, needed to correctly weight
       ## logloss_index when it's pooled (by n_pos_index) across many such calls in
       ## tuning diagnostics, exactly as n_pos already does for logloss_pos
    , n_pos_index = if (is.null(index_flag)) NA_integer_ else sum(index_flag == 1, na.rm = TRUE)
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
      ## Standardized partial ROC AUC over false-alarm rates 0 to 5% (McClish 1989): ranking
       ## quality in the region where alarms are actually raised. 0.5 = random, 1 = perfect.
       ## Used for hyperparameter selection (see score_rank_based)
    , pauc_std    = if (n_pos == 0 || n_pos == n_all) NA_real_ else
                      standardized_partial_auc(prob1, truth == "1", max_fpr = 0.05)
      ## Share of outbreaks among the top 1% / 5% of rows by predicted probability (tied rows
       ## shared in expectation); diagnostics matching the final rare-event verification report
    , recall_top1 = if (n_pos == 0) NA_real_ else recall_at_top(prob1, truth == "1", frac = 0.01)
    , recall_top5 = if (n_pos == 0) NA_real_ else recall_at_top(prob1, truth == "1", frac = 0.05)
      ## Measure of sensitivity (see https://yardstick.tidymodels.org/reference/recall.html)
    , recall    = tibble(
        threshold = threshold
      , recall    = apply(class_hat, 2, FUN = function(x) recall_vec(truth, x |> factor(levels = c("1", "0")), event_level = event_level))
    ) |>
    list()
      ## See https://yardstick.tidymodels.org/reference/precision.html
    , precision = tibble(
        threshold = threshold
      , precision = apply(class_hat, 2, FUN = function(x) precision_vec(truth, x |> factor(levels = c("1", "0")), event_level = event_level))
    ) |>
    list()
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
       ## explicitly as compared to roc_auc and to a greater degree than pr_auc)
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
      ) |>
      list()
      ## Performance restricted to country-level index cases specifically (see
       ## get_rvf_response/lag_join_aggregate) -- the cases a real warning system most needs to
       ## catch, as opposed to later, already-known-about cases in the same chain/country
    , logloss_index = if (is.null(index_flag) || sum(index_flag == 1, na.rm = TRUE) == 0) NA_real_ else
                       mean(-log(pmax(prob1[index_flag == 1], 1e-15)))
      ## Both branches must return a list-column of the same shape (tibble(threshold, recall))
    , recall_index   = if (is.null(index_flag) || sum(index_flag == 1, na.rm = TRUE) == 0) {
                         tibble(threshold = threshold, recall = NA_real_) |> list()
                       } else {
                         tibble(
                           threshold = threshold
                         , recall    = apply(class_hat[index_flag == 1, , drop = FALSE], 2, FUN = function(x) {
                             recall_vec(
                               truth[index_flag == 1] |> factor(levels = c("1", "0"))
                             , x |> factor(levels = c("1", "0"))
                             , event_level = event_level)
                           })
                         ) |>
                         list()
                       }
  )

  ttib

}

## Standardized partial ROC AUC for false-alarm rates in [0, max_fpr] (McClish 1989), so
## 0.5 = random ranking and 1 = perfect. The ROC curve has one point per distinct score, so
## tied rows move together as a straight segment (the tie-correct treatment); the curve is
## interpolated linearly at max_fpr.
standardized_partial_auc <- function(score, is_event, max_fpr = 0.05) {

  o <- order(score, decreasing = TRUE)
  s <- score[o]
  y <- is_event[o]
  n1 <- sum(y)
  n0 <- length(y) - n1

  ## Cumulative true- and false-positive rates at the end of each tied group
  last <- c(s[-1] != s[-length(s)], TRUE)
  tpr  <- c(0, cumsum(y)[last] / n1)
  fpr  <- c(0, cumsum(!y)[last] / n0)

  ## Keep the curve up to max_fpr, adding an interpolated end point when it is crossed
  keep <- fpr <= max_fpr
  x <- fpr[keep]
  t <- tpr[keep]
  if (max(x) < max_fpr) {
    j <- which(fpr > max_fpr)[1]
    t <- c(t, tpr[j - 1] + (tpr[j] - tpr[j - 1]) * (max_fpr - fpr[j - 1]) / (fpr[j] - fpr[j - 1]))
    x <- c(x, max_fpr)
  }

  pauc     <- sum(diff(x) * (head(t, -1) + tail(t, -1)) / 2)
  min_area <- max_fpr^2 / 2
  0.5 * (1 + (pauc - min_area) / (max_fpr - min_area))

}

## Share of events among the top `frac` of rows by score. Rows tied at the cut-off are shared
## in expectation (the recall a random ordering of the tied rows would give on average)
recall_at_top <- function(score, is_event, frac) {

  k     <- ceiling(frac * length(score))
  cut   <- sort(score, decreasing = TRUE)[k]
  above <- score > cut
  tied  <- score == cut

  hits <- sum(is_event[above]) + (k - sum(above)) / sum(tied) * sum(is_event[tied])
  hits / sum(is_event)

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
