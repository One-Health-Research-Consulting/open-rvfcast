#' Rerun tuning-phase fits for the already-chosen winning hyperparameter index only, 
#' this time saving raw per-row predictions
#'
#' harvest_k_correction_predictions used to build the predictions for fit_k_correction_from_paths()
#' extra function used here to avoid having to save raw predictions for the full 
#' hyperparameter search which would be more expensive than just doing this one refit
#' per fold with the optimal set. 
#'
#' @title harvest_k_correction_predictions
#'
#' @param winning_hyperparam_path Path to the finalized hyperparameter CSV
#'   (finalize_hyperparameters_from_inner's output) -- read to find the winning index
#' @param prejoined_data One outer fold's pre-joined data (one row of outer_fold_prejoined)
#' @param inner_fold_id_finalized Global-grid inner-fold-id tibble
#' @param local_inner_fold_id_finalized Local-grid inner-fold-id tibble
#' @param threshold,weightings,start_p,id_cols,checktime_path,hex_id_col Passed
#'   straight through to tune_results_per_outer_fold()
#' @param out_dir Directory for this harvest's output -- MUST differ from the
#'   original tuning sweep's out_dir (see header note)
#' @return Character vector of raw-prediction file paths for this outer fold
#' @author Morgan Kain
#' @export

harvest_k_correction_predictions <- function(
    winning_hyperparam_path, prejoined_data, inner_fold_id_finalized, local_inner_fold_id_finalized
  , threshold, weightings, start_p, id_cols, out_dir, checktime_path, hex_id_col = "shapeName"
) {

  winning_index      <- read.csv(winning_hyperparam_path)$index
  this_outer_fold_id <- prejoined_data$outer_fold_id

  winning_ids <- dplyr::bind_rows(
    inner_fold_id_finalized       |> dplyr::filter(outer_fold_id == this_outer_fold_id, index == winning_index)
  , local_inner_fold_id_finalized |> dplyr::filter(outer_fold_id == this_outer_fold_id, index == winning_index)
  )

  metrics_paths <- tune_results_per_outer_fold(
    prejoined_data       = prejoined_data
  , inner_ids_all        = winning_ids
  , threshold            = threshold
  , weightings           = weightings
  , start_p              = start_p
  , id_cols              = id_cols
  , out_dir              = out_dir
    ## identify the step
  , tuning_grid_id       = "k_correction_harvest"
  , overwrite            = TRUE
  , DEBUG                = FALSE
  , chunk_id             = 1
  , checktime_path       = checktime_path
  , hex_id_col           = hex_id_col
  , save_raw_predictions = TRUE)

  ## tune_results_per_outer_fold()'s return value is the METRICS file paths; the
   ## raw-prediction files it also written (save_raw_predictions=TRUE) following the
   ## naming convention with "inner_raw_" in place of "inner_tuning_"
  stringr::str_replace(metrics_paths, "inner_tuning_", "inner_raw_")

}


#' Pool per-outer-fold raw predictions from the winning hyperparameter set and fit
#' the scale_pos_weight damping-fraction correction
#'
#' @title fit_k_correction_from_paths
#'
#' @param raw_prediction_paths Character vector of file paths from
#'   harvest_k_correction_predictions()
#' @param save_path Where to save the fitted correction (k, converged, n, n_pos)
#' @param prior_mean,prior_lambda Passed through to fit_k_correction(); prior_lambda
#'   should be chosen via scr_r/scr_validate_k_correction.R, not guessed
#' @return save_path
#' @author Morgan Kain
#' @export

fit_k_correction_from_paths <- function(raw_prediction_paths, save_path, prior_mean = 0.47, prior_lambda) {

  pooled <- purrr::map(raw_prediction_paths, .f = function(x) {
    tload <- try(readRDS(x), silent = TRUE)
    if (class(tload)[1] != "try-error") tload else NULL
  }) |> dplyr::bind_rows()

  ## Call function to fit the damping parameter k used for post-fit recalibration
  fit <- fit_k_correction(
    raw_prob     = pooled$prob1
  , true_out     = pooled$truth
  , spw_used     = pooled$spw_used
  , prior_mean   = prior_mean
  , prior_lambda = prior_lambda)

  create_data_directory(directory_path = dirname(save_path))
  write.csv(tibble::as_tibble(fit[c("k", "converged", "n", "n_pos")]), save_path, row.names = FALSE)

  save_path

}


#' Derive the k-correction path paired with a given finalized hyperparameter set
#'
#' @title derive_k_correction_path
#'
#' @param finalized_hyperparameters_path Path in the same family as
#'   finalize_hyperparameters_from_inner()'s output (e.g. local_hyperparam_path)
#' @return The paired k-correction path (may or may not exist yet)
#' @author Morgan Kain
#' @export

derive_k_correction_path <- function(finalized_hyperparameters_path) {
  sub("best_hyperparameters_combined_", "k_correction_", finalized_hyperparameters_path)
}


#' Locate the k-correction fit paired with a given finalized hyperparameter set
#' to be used for PURPOSE = forecast (grabs the k-correction from the
#' finalized_hyperparameters path from the most recent model tuning)
#'
#' @title get_latest_k_correction
#'
#' @param finalized_hyperparameters_path Path returned by
#'   finalize_hyperparameters_from_inner() / get_latest_finalized_hyperparameters()
#' @return Path to the paired k-correction CSV
#' @author Morgan Kain
#' @export

get_latest_k_correction <- function(finalized_hyperparameters_path) {
  k_path <- derive_k_correction_path(finalized_hyperparameters_path)
  if (!file.exists(k_path)) {
    stop(
      "PURPOSE = forecast needs a k-correction paired with the finalized hyperparameters at '"
    , finalized_hyperparameters_path, "', but none was found at the expected path '", k_path
    , "'. Run PURPOSE = train at least once (with this pipeline version) first."
    )
  }
  k_path
}


#' Fit a scale_pos_weight damping-fraction correction
#'
#' scale_pos_weight set to the full class-imbalance ratio is known to shift a
#' fitted model's log-odds upward by approximately log(scale_pos_weight) 
#' (see various sources and documentation in xgboost); how much of that
#' shift actually manifests in an xgboost fit is relatively model-specific. 
#' k is that damping fraction: k=1 is a full correction k=0 is no correction. 
#' This is a 1-parameter penalized logistic fit rather than an unconstrained one
#' because of the extreme rarity of positives (hence use of a prior)
#'
#' @title fit_k_correction
#'
#' @param raw_prob Vector of a fitted model's raw (uncorrected) predicted probabilities
#' @param true_out Vector of true 0/1 outcomes, same length/order as raw_prob
#' @param spw_used Vector of the scale_pos_weight value actually handed to the
#'   engine for each row (varies per fold -- see calc_spw -- so this is a vector,
#'   not a single constant)
#' @param prior_mean Prior mean for k; default 0.47 is the empirically-observed damping 
#'   fraction from diagnosis on fits for this model (log(30)/log(1436) where 1436 is the 
#'   neg:pos ratio in the empirical data and 30 is the fold times over-confident the model
#'   turned out to be across a few hyper-parameter optimized fits for the worst-calibrated
#'   probability bin when creating calibration curves
#' @param prior_lambda Penalty strength pulling k toward prior_mean; larger values
#'   trust the prior more over the data. Chosen by playing around with some sims
#' @return List: k (the fitted damping fraction), converged (logical), n, n_pos
#' @author Morgan Kain
#' @export

fit_k_correction <- function(raw_prob, true_out, spw_used, prior_mean = 0.47, prior_lambda) {

  eps       <- 1e-9
  logit_raw <- qlogis(pmin(pmax(raw_prob, eps), 1 - eps))
  log_spw   <- log(spw_used)

  ## Negative penalized log-likelihood to minimize: ordinary Bernoulli log-lik of
   ## the k-corrected probability against true_out, plus a ridge-style penalty
   ## pulling k toward prior_mean
  neg_penalized_loglik <- function(k) {
    p <- plogis(logit_raw - k * log_spw)
    p <- pmin(pmax(p, eps), 1 - eps)
    loglik  <- sum(true_out * log(p) + (1 - true_out) * log(1 - p))
    penalty <- prior_lambda * (k - prior_mean)^2
    -(loglik - penalty)
  }

  ## 1-D bounded optimization (Brent) -- k is a single scalar, so this is exact
   ## and fast; bounds are wide enough to detect a badly-behaved fit (k outside
   ## [0,1]) without allowing a runaway solution
  opt <- stats::optim(par = prior_mean, fn = neg_penalized_loglik, method = "Brent", lower = -0.5, upper = 1.5)

  list(
    k                     = opt$par
  , converged             = opt$convergence == 0
  , neg_penalized_loglik  = opt$value
  , n                     = length(true_out)
  , n_pos                 = sum(true_out == 1)
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
