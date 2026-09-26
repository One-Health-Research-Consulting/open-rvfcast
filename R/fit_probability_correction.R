#' Harvest held-out predictions from one outer fold for probability calibration
#'
#' Fits the winning (structural) hyperparameter set on one outer fold's full training window,
#' with the same interaction constraints as every other fit, and predicts that fold's own
#' held-out assess window. Mapped over outer folds so each fit runs as its own branch.
#' Calibration is fit on these predictions rather than on inner folds because it needs more
#' positives than any single inner fold has.
#'
#' The cache file name carries a digest of the hyperparameters, start_p and the constraints,
#' so a changed model never reuses predictions from an older one.
#'
#' @title harvest_outer_fold_predictions
#'
#' @param winning_hyperparam_path Path to the finalized (structural) hyperparameter CSV
#' @param outer_fold_row One row of folded_data_training
#' @param full_data The full (unfolded) training data (the train_data target)
#' @param start_p,id_cols,hex_id_col Passed to fit_and_predict_outer_fold()
#' @param interaction_constraints Output of define_interaction_constraints(), or NULL
#' @param out_dir Directory for the harvested per-outer-fold predictions
#' @param overwrite Boolean to refit and save over a previously saved harvest file or not
#' @return Path to this outer fold's harvest file
#' @author Morgan Kain
#' @export

harvest_outer_fold_predictions <- function(
    winning_hyperparam_path, outer_fold_row, full_data
  , start_p, id_cols, interaction_constraints = NULL, hex_id_col = "shapeName"
  , out_dir, overwrite = FALSE
) {

  params <- read.csv(winning_hyperparam_path)

  ## spw_multiplier fixed at 1: calibration below absorbs the scale_pos_weight shift
  params$spw_multiplier <- 1

  create_data_directory(directory_path = out_dir)

  ## Identify this exact model (hyperparameters, base score and constraints) in the file name
  model_tag <- substr(digest::digest(list(params, start_p, interaction_constraints)), 1, 8)
  save_filename <- file.path(out_dir, paste0(
    "outer_raw_outer_fold_", outer_fold_row$outer_fold_id, "_tune_index_", params$index,
    "_", model_tag, ".Rds"))

  if (file.exists(save_filename) && !overwrite) return(save_filename)

  out <- fit_and_predict_outer_fold(
    params                  = params
  , outer_fold_row          = outer_fold_row
  , full_data               = full_data
  , start_p                 = start_p
  , id_cols                 = id_cols
  , interaction_constraints = interaction_constraints
  , hex_id_col              = hex_id_col)

  saveRDS(out, save_filename)

  save_filename

}


#' Fit the post-fit probability calibration on the pooled outer-fold harvest
#'
#' The input to the calibration is the raw log-odds minus log(scale_pos_weight used), 
#' which undoes the known up-weighting from scale_pos_weight and puts every fold (each 
#' with its own scale_pos_weight) on a common scale. A logistic regression then fits one 
#' intercept per forecast window and one shared slope, by maximum likelihood:
#'
#'   calibrated p = plogis(intercept[window] + slope * (qlogis(raw p) - log(spw_used)))
#'
#' A slope below 1 means the raw log-odds are too spread out (top predictions too high, 
#' low-to-middle predictions too low).
#'
#' Cross-fits the calibration (fit on all outer folds but one, predict the held-out fold)
#'
#' @title fit_probability_calibration_on_outer_folds
#'
#' @param harvest_files Paths returned by harvest_outer_fold_predictions() (all branches)
#' @param out_dir Directory to save the calibration diagnostics into
#' @param min_events_per_window Stop if any forecast window has fewer events than this
#' @return List: coefficients (one row per forecast window: forecast_interval, intercept,
#'   slope), diagnostics (list of summary tibbles, see summarize_calibration_diagnostics()),
#'   n, n_pos
#' @author Morgan Kain
#' @export

fit_probability_calibration_on_outer_folds <- function(harvest_files, out_dir, min_events_per_window = 20) {

  pooled <- purrr::map_dfr(harvest_files, readRDS)

  ## Every forecast window needs enough events to estimate its own intercept
  events_per_window <- tapply(pooled$truth, as.character(pooled$forecast_interval), sum)
  if (any(events_per_window < min_events_per_window)) {
    stop("Too few events to calibrate forecast window(s): ",
         paste(names(events_per_window)[events_per_window < min_events_per_window], collapse = ", "))
  }

  ## Final calibration, fit on every harvested prediction
  coefficients <- fit_probability_calibration(
    prob1             = pooled$prob1
  , truth             = pooled$truth
  , spw_used          = pooled$spw_used
  , forecast_interval = pooled$forecast_interval)

  ## Honest (held-out) calibrated predictions for the diagnostics
  pooled$p_calibrated_in_sample <- apply_probability_calibration(
    pooled$prob1, pooled$spw_used, pooled$forecast_interval, coefficients)
  pooled$p_calibrated_cross_fit <- cross_fit_probability_calibration(pooled)

  diagnostics <- summarize_calibration_diagnostics(pooled)

  create_data_directory(directory_path = out_dir)
  saveRDS(
    list(coefficients = coefficients, diagnostics = diagnostics)
  , file.path(out_dir, paste0("probability_calibration_", Sys.Date(), ".Rds")))

  list(
    coefficients = coefficients
  , diagnostics  = diagnostics
  , n            = nrow(pooled)
  , n_pos        = sum(pooled$truth)
  )

}


#' Write the finalized hyperparameter set with the calibration coefficients added
#'
#' @title write_calibrated_hyperparameters
#'
#' @param probability_calibration_result Output of fit_probability_calibration_on_outer_folds()
#' @param structural_hyperparam_path Path to the structural-only hyperparameter CSV
#'   (finalize_hyperparameters_from_inner's output)
#' @param outpath Where to save the finalized, calibrated hyperparameter CSV
#' @return outpath
#' @author Morgan Kain
#' @export

write_calibrated_hyperparameters <- function(probability_calibration_result, structural_hyperparam_path, outpath) {

  params <- read.csv(structural_hyperparam_path)
  params$spw_multiplier <- 1

  ## Calibration coefficients as columns: one shared slope, one intercept per window
  co <- probability_calibration_result$coefficients
  params$calib_slope <- co$slope[1]
  for (i in seq_len(nrow(co))) {
    params[[paste0("calib_intercept_", co$forecast_interval[i])]] <- co$intercept[i]
  }
  params$calib_n     <- probability_calibration_result$n
  params$calib_n_pos <- probability_calibration_result$n_pos

  ## Always written (no skip-if-exists): this target only reruns when the calibration or the
   ## structural set changes, and a skipped write would leave stale coefficients in place
  create_data_directory(directory_path = dirname(outpath))
  write.csv(params, outpath, row.names = FALSE)

  outpath

}


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
#' @param interaction_constraints Output of define_interaction_constraints(), or NULL
#' @return Tibble of raw per-row predictions: prob1, truth, hex_id, date, forecast_interval,
#'   outer_fold_id, spw_used (the effective scale_pos_weight this fit actually used)
#' @author Morgan Kain
#' @export

fit_and_predict_outer_fold <- function(params, outer_fold_row, full_data, start_p, id_cols,
                                       interaction_constraints = NULL, hex_id_col = "shapeName") {

  outer_fold_id <- outer_fold_row$outer_fold_id

  ## This outer fold's full training window (no inner-cluster exclusion) and its own
   ## held-out assess window, from the row-index lists fold_data() already computed.
   ## Same column exclusions as inner_tbl_train/inner_tbl_assess
   ## in tune_results_per_outer_fold
  outer_tbl_train <- full_data |>
    dplyr::filter(index %in% outer_fold_row$train_data[[1]]) |>
    dplyr::select(-dplyr::any_of(c("cases", "country_index_outbreak"))) |>
    dplyr::mutate(outbreak = factor(outbreak, levels = c(1, 0)), forecast_interval = as.factor(forecast_interval))

  outer_tbl_assess <- full_data |>
    dplyr::filter(index %in% outer_fold_row$assess_data[[1]]) |>
    dplyr::select(-dplyr::any_of(c("cases", "country_index_outbreak"))) |>
    dplyr::mutate(outbreak = factor(outbreak, levels = c(1, 0)), forecast_interval = as.factor(forecast_interval))

  spw <- calc_spw(outer_tbl_train)

  rec <- make_recipe(outer_tbl_train, id_cols = id_cols)
  fit <- fit_constrained_workflow(
    rec                     = rec
  , training                = outer_tbl_train
  , params                  = params
  , start_p                 = start_p
  , spw                     = spw
  , interaction_constraints = interaction_constraints)

  prob1 <- predict(fit, outer_tbl_assess, type = "prob")$.pred_1

  tibble::tibble(
    prob1             = prob1
  , truth             = as.numeric(as.character(outer_tbl_assess[["outbreak"]]))
  , hex_id            = outer_tbl_assess[[hex_id_col]]
    ## Kept so calibration can be checked by year (large outbreak years vs quiet ones)
  , date              = outer_tbl_assess$date
  , forecast_interval = outer_tbl_assess$forecast_interval
  , outer_fold_id     = outer_fold_id
    ## effective scale_pos_weight this fit actually used (spw damped by spw_multiplier, if
     ## present) -- needed to undo its log-odds shift since it varies per outer fold
  , spw_used          = spw * resolve_spw_multiplier(params)
  )

}


#' Fit the calibration model: one intercept per forecast window and a shared slope
#'
#' @title fit_probability_calibration
#'
#' @param prob1 Raw predicted probabilities
#' @param truth 0/1 outcomes
#' @param spw_used scale_pos_weight each prediction's model was fit with
#' @param forecast_interval Forecast window of each prediction
#' @return Tibble with one row per forecast window: forecast_interval, intercept, slope
#' @author Morgan Kain
#' @export

fit_probability_calibration <- function(prob1, truth, spw_used, forecast_interval) {

  z      <- calibration_logit(prob1, spw_used)
  window <- factor(as.character(forecast_interval))

  ## No global intercept, so each window gets its own
  cal_fit <- stats::glm(truth ~ 0 + window + z, family = stats::binomial())
  co      <- stats::coef(cal_fit)

  tibble::tibble(
    forecast_interval = levels(window)
  , intercept         = unname(co[paste0("window", levels(window))])
  , slope             = unname(co["z"])
  )

}


#' Apply the fitted calibration to raw predicted probabilities
#'
#' @title apply_probability_calibration
#'
#' @param prob1 Raw predicted probabilities
#' @param spw_used scale_pos_weight the predicting model was fit with (single value or vector)
#' @param forecast_interval Forecast window of each prediction
#' @param coefficients Output of fit_probability_calibration() or read_calibration_coefficients()
#' @return Vector of calibrated probabilities, same length and order as prob1
#' @author Morgan Kain
#' @export

apply_probability_calibration <- function(prob1, spw_used, forecast_interval, coefficients) {

  row <- match(as.character(forecast_interval), coefficients$forecast_interval)
  if (anyNA(row)) {
    stop("No calibration intercept for forecast window(s): ",
         paste(unique(as.character(forecast_interval)[is.na(row)]), collapse = ", "))
  }

  plogis(coefficients$intercept[row] + coefficients$slope[row] * calibration_logit(prob1, spw_used))

}


#' Read calibration coefficients back from a finalized hyperparameter set
#'
#' @title read_calibration_coefficients
#'
#' @param hyper_set One-row data frame read from the finalized hyperparameter CSV
#' @return Tibble in the format of fit_probability_calibration(), or NULL if the set has no
#'   calibration columns (e.g. a CSV written before this calibration existed)
#' @author Morgan Kain
#' @export

read_calibration_coefficients <- function(hyper_set) {

  intercept_cols <- grep("^calib_intercept_", names(hyper_set), value = TRUE)
  if (!length(intercept_cols) || is.null(hyper_set$calib_slope)) return(NULL)

  tibble::tibble(
    forecast_interval = sub("^calib_intercept_", "", intercept_cols)
  , intercept         = unlist(hyper_set[1, intercept_cols], use.names = FALSE)
  , slope             = hyper_set$calib_slope[1]
  )

}


#' Leave-one-outer-fold-out calibrated predictions
#'
#' For each outer fold, fits the calibration on every other fold and predicts this one, so
#' diagnostics reflect how the calibration performs on data it was not fit to.
#'
#' @title cross_fit_probability_calibration
#'
#' @param pooled Pooled harvest (prob1, truth, spw_used, forecast_interval, outer_fold_id)
#' @return Vector of cross-fitted calibrated probabilities (NA where a fold's complement could
#'   not support a fit)
#' @author Morgan Kain
#' @export

cross_fit_probability_calibration <- function(pooled) {

  out <- rep(NA_real_, nrow(pooled))

  for (f in unique(pooled$outer_fold_id)) {
    held_out <- pooled$outer_fold_id == f
    rest     <- pooled[!held_out, ]
    ## A complement missing events in some window cannot support a fit; leave NA
    co <- tryCatch(
      fit_probability_calibration(rest$prob1, rest$truth, rest$spw_used, rest$forecast_interval)
    , error = function(e) NULL)
    if (is.null(co)) next
    out[held_out] <- tryCatch(
      apply_probability_calibration(pooled$prob1[held_out], pooled$spw_used[held_out],
                                    pooled$forecast_interval[held_out], co)
    , error = function(e) NA_real_)
  }

  out

}


#' Summaries of how well raw and calibrated predictions match observed outbreaks
#'
#' @title summarize_calibration_diagnostics
#'
#' @param pooled Pooled harvest with p_calibrated_in_sample and p_calibrated_cross_fit added
#' @return List of tibbles: overall, by_forecast_interval, by_outer_fold, by_year (when a date
#'   column is present), and cross_fit_slope (slope of a logistic regression of the outcome on
#'   the cross-fitted log-odds; 1 means the calibration transfers well to held-out folds)
#' @author Morgan Kain
#' @export

summarize_calibration_diagnostics <- function(pooled) {

  base_rate <- mean(pooled$truth)

  ## Per-group counts, observed/expected and information gain per event vs climatology
  summarise_group <- function(d) {
    d |>
      dplyr::summarise(
        n                        = dplyr::n()
      , events                   = sum(truth)
      , expected_raw             = sum(prob1)
      , expected_cross_fit       = sum(p_calibrated_cross_fit, na.rm = TRUE)
        ## Events on the rows the cross-fit could score, so rows it left NA don't inflate ratios
      , events_cross_fit         = sum(truth[!is.na(p_calibrated_cross_fit)])
      , obs_over_exp_raw         = events / expected_raw
      , obs_over_exp_cross_fit   = events_cross_fit / expected_cross_fit
      , info_gain_per_event_raw  = (bernoulli_loglik(prob1, truth) -
                                     bernoulli_loglik(rep(base_rate, dplyr::n()), truth)) / events
        ## Climatology scored on the same rows (cross-fit NAs dropped from both)
      , info_gain_per_event_cross_fit = (bernoulli_loglik(p_calibrated_cross_fit, truth) -
                                     bernoulli_loglik(dplyr::if_else(is.na(p_calibrated_cross_fit),
                                                                     NA_real_, base_rate), truth)) / events_cross_fit
      , .groups = "drop")
  }

  out <- list(
    overall              = summarise_group(pooled)
  , by_forecast_interval = summarise_group(dplyr::group_by(pooled, forecast_interval))
  , by_outer_fold        = summarise_group(dplyr::group_by(pooled, outer_fold_id))
  )
  if ("date" %in% names(pooled)) {
    out$by_year <- summarise_group(dplyr::group_by(pooled, year = format(date, "%Y")))
  }

  ## Slope of the outcome on the cross-fitted log-odds (1 = calibration transfers well)
  ok <- !is.na(pooled$p_calibrated_cross_fit)
  zc <- qlogis(pmin(pmax(pooled$p_calibrated_cross_fit[ok], 1e-15), 1 - 1e-15))
  ## NA when the cross-fit scored no rows or no events (e.g. too few folds)
  out$cross_fit_slope <- if (sum(pooled$truth[ok]) == 0) NA_real_ else
    unname(stats::coef(stats::glm(pooled$truth[ok] ~ zc, family = stats::binomial()))["zc"])

  out

}


## Calibration input: raw log-odds with the scale_pos_weight up-weighting removed
calibration_logit <- function(prob1, spw_used) {
  qlogis(pmin(pmax(prob1, 1e-15), 1 - 1e-15)) - log(spw_used)
}

## Total Bernoulli log-likelihood (NA predictions are dropped)
bernoulli_loglik <- function(p, y) {
  ok <- !is.na(p)
  p  <- pmin(pmax(p[ok], 1e-15), 1 - 1e-15)
  sum(y[ok] * log(p) + (1 - y[ok]) * log1p(-p))
}
