#' Fit a given hyperparameter set on one outer fold's full training window and predict on that 
#' same outer fold's own genuinely held-out assess window. Used to determine post-fit
#' probability calibration. Determined using the full outer folds instead of each inner 
#' fold per outer fold as the tuning search does (tune_results_per_outer_fold) as this
#' calibration needs more consistant positives -- too little info for this calibration
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

  ## This outer fold's full training window (no inner-cluster exclusion) and its own genuinely
   ## held-out assess window, from the row-index lists fold_data() already computed. Same column
   ## exclusions as inner_tbl_train/inner_tbl_assess in tune_results_per_outer_fold: cases and
   ## country_index_outbreak are never training predictors (by construction they are only
   ## non-zero where outbreak is also 1, so leaving them in would be leakage)
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


#' Choose spw_multiplier by post-correction calibration quality
#'
#' spw_multiplier's effect is on absolute prediction confidence, not on model
#' discrimination (i.e. AUC). This function holds every hyperparameter fixed at the 
#' already-chosen winning set, and for each candidate spw_multiplier fits once per 
#' outer fold on that fold's full training window, pools the held-out predictions, fits
#' k on that pool via the existing fit_k_correction(), and scores the resulting 
#' k corrected predictions with the final_score formula.
#'
#' @title evaluate_spw_multiplier_candidate
#'
#' @param spw_mult The single candidate spw_multiplier value to evaluate
#' @param winning_hyperparam_path Path to the finalized (structural) hyperparameter CSV --
#'   read for every hyperparameter value except spw_multiplier, which this function overrides,
#'   and for weightval_raw, used to score this candidate the same way the rest of this
#'   pipeline does
#' @param folded_data_training All outer folds (fold_data()'s output; one fit per row)
#' @param full_data The full (unfolded) training data (the train_data target)
#' @param start_p,id_cols,hex_id_col Passed straight through to fit_and_predict_outer_fold
#' @param prior_mean,prior_lambda Passed through to fit_k_correction()
#' @param out_dir Directory for this candidate's harvested per-outer-fold predictions
#' @param overwrite Boolean to recalculate and save over previously saved harvest files or not
#' @return One-row tibble: spw_multiplier, k, converged, n, n_pos, S_pos, S_neg_penalty,
#'   final_score (all computed on the k-corrected pooled predictions)
#' @author Morgan Kain
#' @export

evaluate_spw_multiplier_candidate <- function(
    spw_mult, winning_hyperparam_path, folded_data_training, full_data
  , start_p, id_cols, hex_id_col = "shapeName"
  , prior_mean = 0.47, prior_lambda = 5
  , out_dir, overwrite = FALSE
) {

  base_params   <- read.csv(winning_hyperparam_path)
  weightval_raw <- base_params$weightval_raw
  eps           <- 1e-15

  these_params <- base_params
  these_params$spw_multiplier <- spw_mult

  cand_dir <- file.path(out_dir, paste0("spw_", spw_mult))
  create_data_directory(directory_path = cand_dir)

  pooled <- purrr::map_dfr(seq_len(nrow(folded_data_training)), function(i) {

    outer_fold_row <- folded_data_training[i, ]
    save_filename  <- file.path(cand_dir, paste0("outer_raw_outer_fold_", outer_fold_row$outer_fold_id, ".Rds"))

    error_safe_read_file <- possibly(readRDS, NULL)
    existing <- error_safe_read_file(save_filename)
    if (!is.null(existing) && !overwrite) return(existing)

    out <- fit_and_predict_outer_fold(these_params, outer_fold_row, full_data, start_p, id_cols, hex_id_col)
    saveRDS(out, save_filename)
    out

  })

  k_fit     <- fit_k_correction(pooled$prob1, pooled$truth, pooled$spw_used, prior_mean, prior_lambda)
  corrected <- apply_k_correction(pooled$prob1, pooled$spw_used, k_fit$k)

  S_pos         <- -mean(-log(pmax(corrected[pooled$truth == 1], eps)))
  S_neg_penalty <- mean(-log(pmax(1 - corrected[pooled$truth == 0], eps)))

  tibble::tibble(
    spw_multiplier = spw_mult
  , k              = k_fit$k
  , converged      = k_fit$converged
  , n              = k_fit$n
  , n_pos          = k_fit$n_pos
  , S_pos          = S_pos
  , S_neg_penalty  = S_neg_penalty
  , final_score    = S_pos - weightval_raw * S_neg_penalty
  )

}


#' Pick the best-scoring spw_multiplier from evaluate_spw_multiplier_candidate()'s per-candidate
#' rows and write the finalized hyperparameter set (structural hyperparameter values unchanged),
#' spw_multiplier and k added as columns 
#'
#' @title write_calibrated_hyperparameters
#'
#' @param calibration_results Row-bound output of evaluate_spw_multiplier_candidate() across every
#'   spw_mult_grid candidate (model_framework_targets.R runs it as pattern = map(spw_mult_grid),
#'   so this arrives as one combined tibble, one row per candidate)
#' @param structural_hyperparam_path Path to the structural-only hyperparameter CSV
#'   (finalize_hyperparameters_from_inner's output, before spw_multiplier/k are layered on)
#' @param outpath Where to save the finalized (spw- and k-calibrated) hyperparameter CSV
#' @return outpath
#' @author Morgan Kain
#' @export

write_calibrated_hyperparameters <- function(calibration_results, structural_hyperparam_path, outpath) {

  if (file.exists(outpath)) return(outpath)

  best   <- calibration_results |> dplyr::arrange(dplyr::desc(final_score)) |> dplyr::slice(1)
  params <- read.csv(structural_hyperparam_path)
  params$spw_multiplier <- best$spw_multiplier
  params$k              <- best$k
  params$k_converged    <- best$converged
  params$k_n            <- best$n
  params$k_n_pos        <- best$n_pos

  create_data_directory(directory_path = dirname(outpath))
  write.csv(params, outpath, row.names = FALSE)

  outpath

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
