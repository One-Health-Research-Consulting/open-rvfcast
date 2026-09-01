#' Fit a given hyperparameter set on one outer fold's full training window and predict on that 
#' same outer fold's own genuinely held-out assess window. Used to determine post-fit
#' probability calibration. Determined using the full outer folds instead of each inner 
#' fold per outer fold as the tuning search does (tune_results_per_outer_fold) as this
#' calibration needs more consistent positives -- too little info for this calibration
#' in each inner fold
#'
#' @title fit_and_predict_outer_fold
#'
#' @param params Hyperparameter set to fit with (trees, tree_depth, learn_rate, min_n,
#'   loss_reduction, mtry, and optionally spw_multiplier -- see resolve_spw_multiplier)
#' @param outer_fold_row One row of folded_data_training (has outer_fold_id, and the train_data /
#'   assess_data row-index lists fold_data() built for that outer fold)
#' @param full_data The full (unfolded) training data that outer_fold_row's train_data/assess_data
#'   row indices refer into (the train_data target)
#' @param start_p,id_cols,hex_id_col Passed straight through to make_recipe/make_model, same
#'   meaning as in tune_results_per_outer_fold
#' @return Tibble of raw per-row predictions: prob1, truth, hex_id, forecast_interval,
#'   outer_fold_id, spw_used (the effective scale_pos_weight this fit actually used)
#' @author Morgan Kain
#' @export

fit_and_predict_outer_fold <- function(params, outer_fold_row, full_data, start_p, id_cols, hex_id_col = "shapeName") {

  outer_fold_id <- outer_fold_row$outer_fold_id

  ## This outer fold's full training window (no inner-cluster exclusion) and its own 
   ## held-out assess window, from the row-index lists fold_data() already computed. 
   ## Same column exclusions as inner_tbl_train/inner_tbl_assess 
   ## in tune_results_per_outer_fold
  outer_tbl_train <- full_data |>
    dplyr::filter(index %in% outer_fold_row$train_data[[1]]) |>
    dplyr::select(-dplyr::any_of(c("cases", "country_index_outbreak"))) |>
    dplyr::mutate(outbreak = as.factor(outbreak), forecast_interval = as.factor(forecast_interval))

  outer_tbl_assess <- full_data |>
    dplyr::filter(index %in% outer_fold_row$assess_data[[1]]) |>
    dplyr::select(-dplyr::any_of(c("cases", "country_index_outbreak"))) |>
    dplyr::mutate(outbreak = as.factor(outbreak), forecast_interval = as.factor(forecast_interval))

  spw <- calc_spw(outer_tbl_train)

  rec <- make_recipe(outer_tbl_train, id_cols = id_cols)
  mod <- make_model(params = params, start_p = start_p, spw = spw)
  wf  <- workflow() |> add_model(mod) |> add_recipe(rec)

  fit <- fit(wf, data = outer_tbl_train)

  prob1 <- predict(fit, outer_tbl_assess, type = "prob")$.pred_1

  tibble::tibble(
    prob1             = prob1
  , truth             = as.numeric(as.character(outer_tbl_assess[["outbreak"]]))
  , hex_id            = outer_tbl_assess[[hex_id_col]]
  , forecast_interval = outer_tbl_assess$forecast_interval
  , outer_fold_id     = outer_fold_id
    ## effective scale_pos_weight this fit actually used (spw damped by spw_multiplier, if
     ## present) -- needed by fit_k_correction() since it varies per outer fold
  , spw_used          = spw * resolve_spw_multiplier(params)
  )

}


#' Fit k on real, outer-fold-scale harvested predictions
#'
#' spw_multiplier is fixed at 1 here (no pre-fit damping of scale_pos_weight); see
#' note in model_framework_targets.R. In short, a grid search (0.3-1.0) was attempted
#' and the parameter seemd mostly unidentifiable. Because spw_multiplier and k do
#' a very similar thing (absolute prediction confidence, not model discrimination),
#' my conclusion is to fix the one (spw) and adjust the other (k). 
#' This function holds every hyperparameter fixed at the already-chosen winning set, 
#' fits once per outer fold on that fold's full training window, pools the held-out predictions, 
#' fits k on that pool via the existing fit_k_correction() (which targets false positives 
#' in the upper prediction bin), and additionally scores the resulting k-corrected 
#' predictions with this pipeline's standard final_score formula for cross-comparison 
#' (note: this means that this is not the standard approach of an mle-based logistic 
#' regression fitted k like Platt because the mle-based approach doens't get us to
#' an optimized point for this projects objective function).
#'
#' @title fit_k_correction_on_outer_folds
#'
#' @param winning_hyperparam_path Path to the finalized (structural) hyperparameter CSV
#' @param folded_data_training All outer folds (fold_data()'s output; one fit per row)
#' @param full_data The full (unfolded) training data (the train_data target)
#' @param start_p,id_cols,hex_id_col Passed straight through to fit_and_predict_outer_fold
#' @param top_n_multiplier,weightval_upper,prior_mean,prior_lambda Passed through to
#'   fit_k_correction()
#' @param out_dir Directory for the harvested per-outer-fold predictions
#' @param overwrite Boolean to recalculate and save over previously saved harvest files or not
#' @return One-row tibble: k, converged, n, n_pos, n_upper_bin, n_neg_upper_bin,
#'   S_neg_penalty_upper_raw, S_neg_penalty_upper_fit (fit_k_correction()'s own upper-bin
#'   paper trail -- see its docstring), plus S_pos, S_neg_penalty, final_score computed on
#'   the k-corrected pooled predictions using this pipeline's standard (flat-pooled,
#'   weightval_raw-weighted) scoring formula, purely so this candidate can be compared to
#'   every other hyperparameter choice on the same footing -- not used to fit k itself
#' @author Morgan Kain
#' @export

fit_k_correction_on_outer_folds <- function(
    winning_hyperparam_path, folded_data_training, full_data
  , start_p, id_cols, hex_id_col = "shapeName"
  , top_n_multiplier = 5, weightval_upper = 1
  , prior_mean = 0.47, prior_lambda = 0.01
  , out_dir, overwrite = FALSE
) {

  base_params   <- read.csv(winning_hyperparam_path)
  weightval_raw <- base_params$weightval_raw
  eps           <- 1e-15

  ## spw_multiplier fixed at 1 -- see docstring for why a grid search over this value was
   ## dropped
  base_params$spw_multiplier <- 1

  create_data_directory(directory_path = out_dir)

  pooled <- purrr::map_dfr(seq_len(nrow(folded_data_training)), function(i) {

    outer_fold_row <- folded_data_training[i, ]
    save_filename  <- file.path(out_dir, paste0("outer_raw_outer_fold_", outer_fold_row$outer_fold_id, ".Rds"))

    error_safe_read_file <- possibly(readRDS, NULL)
    existing <- error_safe_read_file(save_filename)
    if (!is.null(existing) && !overwrite) return(existing)

    out <- fit_and_predict_outer_fold(base_params, outer_fold_row, full_data, start_p, id_cols, hex_id_col)
    saveRDS(out, save_filename)
    out

  })

  ## k is fit to control false positives 
   ## specifically in the upper (highest-confidence) prediction bin. Global k
   ## leads to a large negative and drastically inflates the false positive rate
   ## even if it does improve the predicted p for true 1s (not worth it)
  k_fit <- fit_k_correction(
    raw_prob         = pooled$prob1
  , true_out         = pooled$truth
  , spw_used         = pooled$spw_used
  , top_n_multiplier = top_n_multiplier
  , weightval_upper  = weightval_upper
  , prior_mean       = prior_mean
  , prior_lambda     = prior_lambda)
  corrected <- apply_k_correction(pooled$prob1, pooled$spw_used, k_fit$k)

  ## Computed here purely solely so this candidate's k can be compared in the same
   ## was as every other hyperparameter choice in this pipeline (not used to 
   ## choose k itself)
  S_pos         <- -mean(-log(pmax(corrected[pooled$truth == 1], eps)))
  S_neg_penalty <- mean(-log(pmax(1 - corrected[pooled$truth == 0], eps)))

  tibble::tibble(
    k                        = k_fit$k
  , converged                = k_fit$converged
  , n                        = k_fit$n
  , n_pos                    = k_fit$n_pos
  , n_upper_bin              = k_fit$n_upper_bin
  , n_neg_upper_bin          = k_fit$n_neg_upper_bin
  , S_neg_penalty_upper_raw  = k_fit$S_neg_penalty_upper_raw
  , S_neg_penalty_upper_fit  = k_fit$S_neg_penalty_upper_fit
  , S_pos                    = S_pos
  , S_neg_penalty            = S_neg_penalty
  , final_score              = S_pos - weightval_raw * S_neg_penalty
  )

}


#' Write the finalized hyperparameter set (structural hyperparameter values unchanged),
#' spw_multiplier (fixed at 1) and k added as columns
#'
#' @title write_calibrated_hyperparameters
#'
#' @param k_correction_result One-row tibble from fit_k_correction_on_outer_folds()
#' @param structural_hyperparam_path Path to the structural-only hyperparameter CSV
#'   (finalize_hyperparameters_from_inner's output, before spw_multiplier/k are layered on)
#' @param outpath Where to save the finalized (spw- and k-calibrated) hyperparameter CSV
#' @return outpath
#' @author Morgan Kain
#' @export

write_calibrated_hyperparameters <- function(k_correction_result, structural_hyperparam_path, outpath) {

  if (file.exists(outpath)) return(outpath)

  params <- read.csv(structural_hyperparam_path)
  ## spw_multiplier is fixed at 1 -- see fit_k_correction_on_outer_folds()'s docstring for why
   ## a grid search over this value was dropped
  params$spw_multiplier <- 1
  params$k              <- k_correction_result$k
  params$k_converged    <- k_correction_result$converged
  params$k_n            <- k_correction_result$n
  params$k_n_pos        <- k_correction_result$n_pos

  create_data_directory(directory_path = dirname(outpath))
  write.csv(params, outpath, row.names = FALSE)

  outpath

}


#' Fit a scale_pos_weight damping-fraction correction, targeting false positives in the
#' upper (highest-confidence) prediction bin specifically
#'
#' scale_pos_weight set to the full class-imbalance ratio is known to shift a
#' fitted model's log-odds upward by approximately log(scale_pos_weight)
#' (see various sources and documentation in xgboost); how much of that
#' shift actually manifests in an xgboost fit is relatively model-specific.
#' k is that damping fraction: k=1 is a full correction k=0 is no correction.
#' This is a 1-parameter penalized fit rather than an unconstrained one because
#' of the extreme rarity of positives (hence use of a prior).
#'
#' k is fit by maximizing S_pos (mean log-probability on true positives) minus a penalty on
#' false positives restricted to the "upper bin" -- the top `top_n_multiplier * n_pos`
#' highest RAW-scored rows (positives and negatives together). 
#' Note: with a global (not focused on upper bin), with ~1.3M negatives and ~0.1% prevalence, 
#' the overwhelming majority of negatives already sit at very low raw probability, deep in
#' the sigmoid's flat region, where a logit-level shift barely moves them at all. A flat mean 
#' over that population is dominated by rows that were never actually a false-positive risk, 
#' so it couldn't "see" (and thus didn't resist) the fit walking toward negative k, boosting
#' predicted probability upward, making the false-negative rate in the upper bin worse.
#' Restricting the penalty to the upper bin allows k to vary more depending on the weight
#' put on false-positive vs improving true-positive rate in this upper bin.
#'
#' @title fit_k_correction
#'
#' @param raw_prob Vector of a fitted model's raw (uncorrected) predicted probabilities
#' @param true_out Vector of true 0/1 outcomes, same length/order as raw_prob
#' @param spw_used Vector of the scale_pos_weight value actually handed to the
#'   engine for each row (varies per fold -- see calc_spw -- so this is a vector,
#'   not a single constant)
#' @param top_n_multiplier Size of the "upper bin" this correction targets, as a multiple of
#'   n_pos (number of true positives) -- e.g. the default of 5 means the top 5*n_pos
#'   highest-raw-scored rows, a standard "precision at k" framing for rare-event alerting
#'   (roughly, "how many false alarms alongside every 5 candidates investigated"). Bin
#'   membership is computed once from raw_prob and held fixed while k is optimized
#' @param weightval_upper Penalty weight on the upper-bin negative-class term. Empirical
#'   exploration reveals about a value of 1000 as the tipping point between neg and pos k
#' @param prior_mean Prior mean for k; default of somehwere near 0.47 (0.5 fine) as the 
#'   empirically-observed damping fraction from diagnosis on fits for this model 
#'   (log(30)/log(1436) where 1436 is the neg:pos ratio in the empirical data and 30 is 
#'   the fold times over-confident the model turned out to be across a few hyper-parameter
#'  optimized fits for the worst-calibrated probability bin when creating calibration curves
#' @param prior_lambda Penalty strength pulling k toward prior_mean; larger values trust the
#'   prior more over the data. Default lowered to 0.01 (weak effect)
#' @return List: k (the fitted damping fraction), converged (logical), n, n_pos,
#'   n_upper_bin, n_neg_upper_bin, S_neg_penalty_upper_raw (upper-bin penalty before any
#'   correction, k=0), S_neg_penalty_upper_fit (same, at the fitted k) -- the raw-vs-fit pair
#'   is the paper trail showing how much the fitted k actually improved upper-bin calibration
#' @author Morgan Kain
#' @export

fit_k_correction <- function(raw_prob, true_out, spw_used, top_n_multiplier = 5, weightval_upper = 1, prior_mean = 0.47, prior_lambda = 0.01) {

  eps       <- 1e-9
  logit_raw <- qlogis(pmin(pmax(raw_prob, eps), 1 - eps))
  log_spw   <- log(spw_used)
  n_pos     <- sum(true_out == 1)

  ## "Upper bin" = the top top_n_multiplier * n_pos highest raw-scored rows (positives and
   ## negatives together) 
   ## Membership is fixed from the RAW probability (not recomputed as k changes below), so
   ## the objective stays smooth in k instead of having its own target population shift
   ## underneath it as the fit progresses
  upper_n   <- min(top_n_multiplier * n_pos, length(raw_prob))
  upper_bin <- rank(-raw_prob, ties.method = "first") <= upper_n
  neg_upper <- true_out == 0 & upper_bin

  ## Negative penalized objective to minimize: rewards keeping predicted probability high on
   ## true positives (S_pos), penalizes keeping it high on FALSE positives specifically within
   ## the upper bin (S_neg_penalty_upper), plus a ridge-style penalty pulling k toward
   ## prior_mean
  neg_penalized_final_score <- function(k) {
    p                   <- plogis(logit_raw - k * log_spw)
    p                   <- pmin(pmax(p, eps), 1 - eps)
    S_pos               <- -mean(-log(p[true_out == 1]))
    S_neg_penalty_upper <- if (any(neg_upper)) mean(-log(1 - p[neg_upper])) else 0
    final_score         <- S_pos - weightval_upper * S_neg_penalty_upper
    penalty             <- prior_lambda * (k - prior_mean)^2
    -(final_score - penalty)
  }

  ## 1-D bounded optimization (Brent)
  opt <- stats::optim(par = prior_mean, fn = neg_penalized_final_score, method = "Brent", lower = -2, upper = 1.5)

  ## Before/after (k=0 vs the fitted k) upper-bin penalty
  p_raw                   <- pmin(pmax(raw_prob, eps), 1 - eps)
  S_neg_penalty_upper_raw <- if (any(neg_upper)) mean(-log(1 - p_raw[neg_upper])) else 0
  p_fit                   <- pmin(pmax(plogis(logit_raw - opt$par * log_spw), eps), 1 - eps)
  S_neg_penalty_upper_fit <- if (any(neg_upper)) mean(-log(1 - p_fit[neg_upper])) else 0

  list(
    k                        = opt$par
  , converged                = opt$convergence == 0
  , neg_penalized_final_score = opt$value
  , n                        = length(true_out)
  , n_pos                    = n_pos
  , n_upper_bin              = upper_n
  , n_neg_upper_bin          = sum(neg_upper)
  , S_neg_penalty_upper_raw  = S_neg_penalty_upper_raw
  , S_neg_penalty_upper_fit  = S_neg_penalty_upper_fit
  )

}


#' Apply a fitted scale_pos_weight damping-fraction correction
#'
#' @title apply_k_correction
#'
#' @param raw_prob Vector of a fitted model's raw (uncorrected) predicted probabilities
#' @param spw_used Vector (or single value, recycled) of the scale_pos_weight
#'   actually used to fit the model that produced raw_prob
#' @param k Damping fraction from fit_k_correction()$k
#' @return Vector of corrected probabilities, same length/order as raw_prob
#' @author Morgan Kain
#' @export

apply_k_correction <- function(raw_prob, spw_used, k) {
  eps       <- 1e-9
  logit_raw <- qlogis(pmin(pmax(raw_prob, eps), 1 - eps))
  plogis(logit_raw - k * log(spw_used))
}
