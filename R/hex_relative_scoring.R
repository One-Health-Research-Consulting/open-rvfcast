#' Alternative to tune_results_per_outer_fold that additionally scores predictions
#' RELATIVE TO each hex's own baseline predicted probability, instead of only on the
#' raw (absolute) predicted probability. The original S_pos / S_neg_penalty score (see 
#' finalize_hyperparameters_from_inner) pools log-loss across every hex and date together, 
#' so it is not well equipped to separate high-risk hexes overall from high risk at the
#' right time.
#' This file take a different (currently experimental) approach of adding a within-hex 
#' relative scoring track (without touching any of the existing functions in R/tune_results_per_outer_fold.R 
#' or R/build_hyperparameter_grid.R), so the so it can easily be scrapped or switched
#' to if the experimentation works out
#'
#' @title tune_results_per_outer_fold_hexrelative
#'
#' @param prejoined_data 1-row tibble with outer_fold_id and data (pre-joined inner fold covariates)
#' @param inner_ids_all Tibble of all (inner_fold_id, tune_grid_index, hyperparameters) for this outer fold
#' @param threshold For assigning a 1 | estimated prob
#' @param weightings weight assigned to 1s in binomial loss metric
#' @param start_p initilization point for base intercept
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param tuning_grid_id id of the current tuning grid to track tuning better
#' @param hyperparam_path path to where the best hyperparameter file will be / is saved
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @param DEBUG If TRUE reduce to a small dataset for code testing
#' @param chunk_id Index of the (inner_fold x tune_grid) chunk this branch is responsible for
#' @param checktime_path path to save csv tracking computation time
#' @param hex_id_col Column identifying the spatial hex (matches district_id_col in
#'   model_framework_targets.R -- "shapeName" even when using_hexes == TRUE)
#' @return Character vector of file paths, one per (inner_fold_id, tune_grid_index) combination
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold_hexrelative <- function(prejoined_data, inner_ids_all, threshold
                                      , weightings, start_p, id_cols, out_dir
                                      , tuning_grid_id, hyperparam_path, overwrite, DEBUG
                                      , chunk_id, checktime_path, hex_id_col = "shapeName") {

  if (file.exists(hyperparam_path)) {
    return(hyperparam_path)
  }

  outer_fold_id <- prejoined_data$outer_fold_id
  joined_data   <- prejoined_data$data[[1]]

  error_safe_read_file <- possibly(readRDS, NULL)
  save_filenames       <- character(nrow(inner_ids_all))

  checktime_tibble    <- tibble(user = numeric(0), sys = numeric(0), elapsed = numeric(0))
  checktime_path.full <- paste0(checktime_path, "/outer_fold_", outer_fold_id, "_chunk_", chunk_id, "_hexrel.csv")

  for (i in seq_len(nrow(inner_ids_all))) {

    inner_ids   <- inner_ids_all[i, ]
    inner_id    <- inner_ids$inner_fold_id
    tuning_grid <- inner_ids |> dplyr::select(-contains("fold_id"))

    ## Create a new file saving convention so it doesn't conflict with the other option
    save_filename <- paste(
        out_dir
      , "/"
      , "inner_tuning_hexrel_"
      , "outer_fold_"
      , paste(outer_fold_id, collapse = "_")
      , "_inner_fold_"
      , inner_id
      , "_tune_grid_"
      , tuning_grid_id
      , "_tune_index_"
      , tuning_grid$index
      , ".Rds"
      , sep = ""
    )

    if (!is.null(error_safe_read_file(save_filename)) && !overwrite) {
      message("file already exists and can be loaded, skipping processing")
      save_filenames[i] <- save_filename
      next
    }

    ## Note: Lifted more or less directly from the other function, comments there
    checktime <- system.time({

    inner_tbl_train <- joined_data |>
      dplyr::filter(cluster != inner_id) |>
      relocate(cluster, .after = "date") |>
      dplyr::select(-c(cluster, cases)) |>
      mutate(outbreak = as.factor(outbreak)) |>
      mutate(forecast_interval = as.factor(forecast_interval))

    spw <- calc_spw(inner_tbl_train)

    inner_tbl_assess <- joined_data |>
      dplyr::filter(cluster == inner_id) |>
      relocate(cluster, .after = "date") |>
      dplyr::select(-c(cluster, cases)) |>
      mutate(outbreak = as.factor(outbreak)) |>
      mutate(forecast_interval = as.factor(forecast_interval)) |>
      mutate(
        weights = length(which(outbreak == "0")) / max(length(which(outbreak == "1")), 1)
      , weights = ifelse(outbreak == "0", 1, weights)
      , .after = "index"
      )

    if (DEBUG) {
      inner_tbl_train <- inner_tbl_train[1:10000, ]
    }

    rec <- make_recipe(inner_tbl_train, id_cols = id_cols)
    mod <- make_model(params = tuning_grid, start_p = start_p, spw = spw)
    wf  <- workflow() |> add_model(mod) |> add_recipe(rec)

    fit <- fit(wf, data = inner_tbl_train)

    rm(inner_tbl_train, rec, mod, wf)
    gc()

    ## Predictions: prob only, plus the hex id needed to compute each hex's own baseline
    prob1     <- predict(fit, inner_tbl_assess, type = "prob")$.pred_1
    truth     <- factor(inner_tbl_assess[["outbreak"]], levels = c("1", "0"))
    hex_id    <- inner_tbl_assess[[hex_id_col]]
    class_hat <- apply(
      threshold |> matrix()
    , 1
    , FUN = function(x) factor(ifelse(prob1 >= x, "1", "0"), levels = c("1", "0"))
    )
    all_intervals     <- inner_tbl_assess$forecast_interval
    forecast_interval <- all_intervals |> unique() |> as.character() |> as.numeric() |> sort()

    rm(fit)
    gc()

    ## Compute metrics -- hex baseline is computed WITHIN each forecast_interval (rather than
     ## pooling across all five horizons) since predicted probability at a 0-30 day horizon and
     ## a 121-150 day horizon are not expected to share the same typical level for a given hex
    metrics <- purrr:::map(forecast_interval, .f = function(this_int) {

      this_rows   <- which(all_intervals == this_int)
      truth.t     <- truth[this_rows]
      prob1.t     <- prob1[this_rows]
      hex_id.t    <- hex_id[this_rows]
      class_hat.t <- class_hat[this_rows, ]

      compute_metrics_vec_hexrelative(
        truth       = truth.t
      , threshold   = threshold
      , weightings  = weightings
      , caseweights = inner_tbl_assess |> filter(forecast_interval == this_int) |> pull(weights)
      , prob1       = prob1.t
      , hex_id      = hex_id.t
      , class_hat   = class_hat.t
      , event_level = "first"
      ) |>
        mutate(
          outer_fold_id = outer_fold_id
        , inner_fold_id = inner_id
        , interval      = this_int
        , .before       = 1
        ) |>
        bind_cols(tuning_grid)

    }) |>
    bind_rows()

    saveRDS(metrics, save_filename)

    rm(inner_tbl_assess, metrics)
    gc()

    save_filenames[i] <- save_filename

  })

    checktime_tibble <- bind_rows(
      checktime_tibble
    , tibble(user = checktime[1], sys = checktime[2], elapsed = checktime[3])
    )

    write.csv(checktime_tibble, checktime_path.full)

  }

  save_filenames

}


#' Hex-relative counterpart to build_local_hyperparameter_grid: builds a local refinement grid
#' centred on the top-k global results, but ranks candidates by the blended score
#' (final_score_combined = final_score_hex + gamma * final_score, see score_hexrelative_results)
#' instead of the raw pooled score. 
#' The saved grid file's name is a hash of every parameter that affects its content (weightval_raw,
#' weightval_hex, gamma, top_k, expansion, size, seed) rather than just the random string tied to
#' seed alone. 
#' The returned tibble also carries weightval_raw/weightval_hex/gamma as columns, so
#' finalize_hyperparameters_from_inner_hexrelative can read them directly from the grid it is
#' given instead of needing the same three values passed to it separately and kept in sync by hand.
#'
#' @title build_local_hyperparameter_grid_hexrelative
#'
#' @param inner_fold_paths Character vector of file paths from tune_results_per_outer_fold_hexrelative
#' @param global_grid Single-row tibble returned by build_hyperparameter_grid (par_grid + grid_id)
#' @param tune_pars Data frame of global search bounds (same object passed to build_hyperparameter_grid);
#'   used to cap the local grid so it never searches outside where the global grid already looked
#' @param top_k Number of top global parameter sets whose ranges define the local neighbourhood
#' @param size Number of local grid points to generate (space-filling)
#' @param weightval_raw Numeric penalty weight on S_neg_penalty (raw, non-hex); see score_hexrelative_results
#' @param weightval_hex Numeric penalty weight on S_neg_penalty_hex; see score_hexrelative_results
#' @param gamma Numeric weight on the raw (non-hex) final_score in the blend; see score_hexrelative_results
#' @param expansion Fraction of the top-k range to extend on each side (e.g. 0.5 = +/-50%)
#' @param grid_path Directory in which to save the local grid Rds
#' @param folded_data_training Folded training data (needed to finalise mtry upper bound)
#' @param splitted_data Split data object (needed to finalise mtry upper bound)
#' @param seed Random seed for reproducibility
#' @return Single-row tibble with columns par_grid (list), grid_id (character, prefixed "localhex_"),
#'   weightval_raw, weightval_hex, gamma
#' @author Morgan Kain
#' @export

build_local_hyperparameter_grid_hexrelative <- function(
    inner_fold_paths
  , global_grid
  , tune_pars
  , top_k
  , size
  , weightval_raw
  , weightval_hex
  , gamma
  , expansion
  , grid_path
  , folded_data_training
  , splitted_data
  , seed
) {

  create_data_directory(directory_path = grid_path)

  all_results <- purrr::map(inner_fold_paths, .f = function(x) {
    tload <- try(readRDS(x), silent = TRUE)
    if (class(tload)[1] != "try-error") {
      return(tload)
    } else {
      return(NULL)
    }
    }) |> bind_rows()

  scores <- score_hexrelative_results(all_results, weightval_raw, weightval_hex, gamma)

  top_indices <- scores |>
    arrange(desc(final_score_combined)) |>
    dplyr::slice(seq_len(top_k)) |>
    pull(index)

  top_params <- all_results |>
    dplyr::filter(index %in% top_indices) |>
    dplyr::select(index, trees, tree_depth, learn_rate, min_n, loss_reduction, mtry) |>
    distinct()

  ## Same hard-bound-capped expansion logic as build_local_hyperparameter_grid (R/build_hyperparameter_grid.R);
   ## reused via expand_range rather than duplicated since that helper is scoring-strategy agnostic
  trees_range   <- expand_range(top_params$trees, lo_hard = tune_pars$tree_min, hi_hard = tune_pars$tree_max, expansion = expansion, min_half_width = 50)
  depth_range   <- expand_range(top_params$tree_depth, lo_hard = tune_pars$tree_dep_min, hi_hard = tune_pars$tree_dep_max, expansion = expansion, min_half_width = 1)
  lr_range      <- expand_range(log10(top_params$learn_rate), lo_hard = tune_pars$learn_rate_min, hi_hard = tune_pars$learn_rate_max, expansion = expansion, min_half_width = 0.2)
  minn_range    <- expand_range(top_params$min_n, lo_hard = tune_pars$minn_min, hi_hard = tune_pars$minn_max, expansion = expansion, min_half_width = 5)
  lossred_range <- expand_range(log10(top_params$loss_reduction + .Machine$double.eps), lo_hard = tune_pars$loss_red_min, hi_hard = tune_pars$loss_red_max, expansion = expansion, min_half_width = 0.5)
  mtry_range_lo <- max(tune_pars$mtry_min, min(top_params$mtry) - 3L)

  ## Hash every parameter that determines this grid's content into its id, so a change in any of
   ## them produces a new file (forcing a rebuild) instead of silently reusing a stale one -- see
   ## the note above the function.
  param_sig <- digest::digest(list(weightval_raw, weightval_hex, gamma, top_k, expansion, size, seed))
  hyper_id  <- paste0("localhex_", param_sig)
  save_path <- paste0(grid_path, "/hypergrid_", hyper_id, ".Rds")

  if (file.exists(save_path)) {

    par_grid <- readRDS(save_path)

  } else {

    idx_offset <- max(global_grid$par_grid[[1]]$index)

    set.seed(seed)
    par_grid <- grid_space_filling(
        trees(range          = as.integer(trees_range))
      , tree_depth(range     = as.integer(depth_range))
      , learn_rate(range     = lr_range)
      , min_n(range          = as.integer(minn_range))
      , loss_reduction(range = lossred_range)
      , finalize(mtry(range  = c(mtry_range_lo, unknown())),
                 folded_data_training$inner_folds[[10]] |>
                   left_join(splitted_data$train_data[[1]], by = "index") |>
                   filter(cluster != 1))
      , size = size
    ) |>
      mutate(index = idx_offset + seq_len(n()), .before = 1)

    saveRDS(par_grid, save_path)

  }

  tibble(
    par_grid      = par_grid |> list()
  , grid_id       = hyper_id
  , weightval_raw = weightval_raw
  , weightval_hex = weightval_hex
  , gamma         = gamma
  )

}


#' Hex-relative counterpart to finalize_hyperparameters_from_inner. Selects the winning
#' hyperparameter set by final_score_combined (final_score_hex + gamma * final_score, see
#' score_hexrelative_results). Also reports which index a pure hex-relative selection 
#' (gamma = 0) and a pure raw selection would each have picked from the same pool, 
#' so the strategies can be diffed from one tuning run.
#'
#' Takes local_tuning_grid (the object build_local_hyperparameter_grid_hexrelative returns) rather
#' than separate weightval_raw/weightval_hex/gamma arguments
#'
#' @title finalize_hyperparameters_from_inner_hexrelative
#'
#' @param inner_folds Character vector of all file paths from tune_results_per_outer_fold_hexrelative
#' @param local_tuning_grid Single-row tibble returned by build_local_hyperparameter_grid_hexrelative;
#'   must carry weightval_raw, weightval_hex, gamma columns
#' @param tuning_grid_id string for this tuning grid
#' @param outpath where to save the best hyperparameter set
#' @return Single-row tibble containing final_score_combined, final_score_hex, S_pos_hex,
#'   S_neg_penalty_hex, within_hex_auc, the raw (non-hex) final_score/S_pos/S_neg_penalty,
#'   hex_only_would_have_picked_index, raw_only_would_have_picked_index, and all hyperparameter values
#' @author Morgan Kain
#' @export

finalize_hyperparameters_from_inner_hexrelative <- function(inner_folds, local_tuning_grid, tuning_grid_id, outpath) {

  if (file.exists(outpath)) return(outpath)

  create_data_directory(directory_path = strsplit(outpath, "/best_hyperparameters")[[1]][1])

  weightval_raw <- local_tuning_grid$weightval_raw
  weightval_hex <- local_tuning_grid$weightval_hex
  gamma         <- local_tuning_grid$gamma

  stopifnot(is.numeric(weightval_raw), length(weightval_raw) == 1, weightval_raw >= 0)
  stopifnot(is.numeric(weightval_hex), length(weightval_hex) == 1, weightval_hex >= 0)
  stopifnot(is.numeric(gamma), length(gamma) == 1, gamma >= 0)

  all_results <- purrr::map(inner_folds, .f = function(x) {
    tload <- try(readRDS(x), silent = TRUE)
    if (class(tload)[1] != "try-error") {
      return(tload)
    } else {
      return(NULL)
    }
  }) |> bind_rows()

  scores <- score_hexrelative_results(all_results, weightval_raw, weightval_hex, gamma)

  best <- scores |>
    arrange(desc(final_score_combined)) |>
    dplyr::slice(1)

  ## What a pure hex-relative selection (gamma = 0) and a pure raw selection would each have
   ## picked from this same pool of fits -- kept alongside so all three strategies are directly
   ## comparable from one tuning run
  hex_only_best_index <- scores |> arrange(desc(final_score_hex)) |> dplyr::slice(1) |> pull(index)
  raw_only_best_index <- scores |> arrange(desc(final_score))     |> dplyr::slice(1) |> pull(index)

  best <- best |>
    left_join(
      all_results |>
        dplyr::select(index, trees, tree_depth, learn_rate, min_n, loss_reduction, mtry) |>
        distinct(), by = "index") |>
    mutate(
      tuning_grid_id                    = tuning_grid_id
    , hex_only_would_have_picked_index  = hex_only_best_index
    , raw_only_would_have_picked_index  = raw_only_best_index
    , .before = index
    )

  write.csv(best, outpath)

  outpath

}



##### Helpers ----------------------------------------------------------------------------

#' Hex-relative counterpart to compute_metrics_vec. Computes the exact same set of columns
#' (pr_auc, roc_auc, recall, precision, logloss, logloss_pos, logloss_neg, logloss_weighted --
#' unchanged, on the raw probability, for direct comparison) and additionally computes
#' logloss_pos_hex / logloss_neg_hex on hex-demeaned probabilities, plus a within-hex ranking
#' metric (within_hex_auc).
#'
#' logloss_neg_hex is restricted to negative rows belonging to hexes that have at least one event
#' THIS call. Chronic hexes' own demeaned negative rows sit at a near-fixed ~-log(0.5) regardless 
#' of that hex's absolute calibration (demeaning removes a hex's level exactly, whether the underlying
#' prediction was well- or badly-calibrated), so including them would only dilute the one signal
#' this term can actually measure (within-hex timing precision among hexes where timing is
#' measurable) with a large mass of rows that cannot inform it either way. Absolute miscalibration
#' is penalized through the raw (non-hex) S_neg_penalty term via gamma.
#'
#' @title compute_metrics_vec_hexrelative
#'
#' @param truth Factor of true outbreak labels ("1"/"0")
#' @param threshold Vector of probability thresholds passed through to compute_metrics_vec
#' @param weightings Vector of case-weight multipliers passed through to compute_metrics_vec
#' @param caseweights Per-row case weights passed through to compute_metrics_vec
#' @param prob1 Predicted probability of outbreak (raw, un-adjusted)
#' @param hex_id Vector identifying which spatial hex each row belongs to
#' @param class_hat Matrix of thresholded class predictions passed through to compute_metrics_vec
#' @param event_level Passed through to compute_metrics_vec
#' @return Tibble: all compute_metrics_vec columns plus n_hex, logloss_pos_hex, logloss_neg_hex,
#'   n_neg_eventful, within_hex_auc, within_hex_auc_n_pairs
#' @author Morgan Kain
#' @export

compute_metrics_vec_hexrelative <- function(truth, threshold, weightings, caseweights, prob1, hex_id
                                            , class_hat, event_level = "first") {

  base_metrics <- compute_metrics_vec(
    truth       = truth
  , threshold   = threshold
  , weightings  = weightings
  , caseweights = caseweights
  , prob1       = prob1
  , class_hat   = class_hat
  , event_level = event_level
  )

  prob1_hex <- compute_hex_relative_prob(prob1 = prob1, hex_id = hex_id, truth = truth)

  n_pos          <- length(which(truth == "1"))
  eventful_hexes <- unique(hex_id[truth == "1"])
  is_eventful    <- hex_id %in% eventful_hexes
  n_neg_eventful <- sum(truth == "0" & is_eventful)

  hex_metrics <- tibble(
    n_hex           = n_distinct(hex_id)
  , logloss_pos_hex = if (n_pos == 0) NA_real_ else mean(-log(pmax(prob1_hex[truth == "1"], 1e-15)))
  , logloss_neg_hex = if (n_pos == 0) NA_real_ else
                      mean(-log(pmax(1 - prob1_hex[truth == "0" & is_eventful], 1e-15)))
  , n_neg_eventful  = n_neg_eventful
  )

  bind_cols(
    base_metrics
  , hex_metrics
  , within_hex_auc_vec(truth = truth, prob1 = prob1, hex_id = hex_id)
  )

}


#' Re-express each row's predicted probability RELATIVE to its own hex's typical level, by
#' subtracting the hex's mean log-odds and mapping back to a probability; what survives is only 
#' the WITHIN-hex temporal shape, which is the part of the prediction that
#' can actually distinguish "outbreak coming soon" from "this is generally a high-risk area".
#' The baseline is computed from that hex's TRUE-NEGATIVE rows only, not all of its rows. If the
#' baseline included true-event rows, a correctly (or incorrectly) elevated prediction on the
#' actual event day would inflate the very reference point it is then compared against, shrinking
#' its own apparent signal -- using only non-event rows keeps the baseline an uncontaminated read
#' of that hex's normal/background level.
#'
#' @title compute_hex_relative_prob
#'
#' @param prob1 Predicted probability of outbreak (raw)
#' @param hex_id Vector identifying which spatial hex each row belongs to
#' @param truth Factor of true outbreak labels ("1"/"0"); used only to exclude event rows from
#'   the baseline, not to change what gets demeaned (every row, event or not, is still returned)
#' @return Numeric vector, same length/order as prob1, of hex-demeaned probabilities
#' @author Morgan Kain
#' @export

compute_hex_relative_prob <- function(prob1, hex_id, truth) {

  eps      <- 1e-9
  logit_p  <- qlogis(pmin(pmax(prob1, eps), 1 - eps))

  hex_baseline_logit <- tibble(hex_id = hex_id, logit_p = logit_p, truth = truth) |>
    group_by(hex_id) |>
    mutate(
      hex_baseline_logit = if (any(truth == "0")) mean(logit_p[truth == "0"]) else mean(logit_p)
    ) |>
    ungroup() |>
    pull(hex_baseline_logit)

  plogis(logit_p - hex_baseline_logit)

}

#' Within-hex ranking metric: the probability that a random true-1 row outranks a random true-0
#' row FROM THE SAME HEX, pooled across hexes (weighted by number of pos/neg pairs available).
#' Equivalent to a stratified Mann-Whitney AUC with hex as the stratum. Unlike a pooled ROC-AUC,
#' a hex that is simply predicted elevated all year (but with no real within-hex timing signal)
#' scores 0.5 here rather than benefiting from being compared against OTHER hexes' lower baseline.
#'
#' @title within_hex_auc_vec
#'
#' @param truth Factor of true outbreak labels ("1"/"0")
#' @param prob1 Predicted probability of outbreak (raw -- demeaning would not change within-hex
#'   rank order since it only subtracts a per-hex constant, so there is no need to pass prob1_hex)
#' @param hex_id Vector identifying which spatial hex each row belongs to
#' @return One-row tibble: within_hex_auc (NA if no hex has both a positive and a negative row)
#'   and within_hex_auc_n_pairs (total pos x neg pairs the estimate is based on)
#' @author Morgan Kain
#' @export

within_hex_auc_vec <- function(truth, prob1, hex_id) {

  by_hex <- tibble(hex_id = hex_id, truth = truth, prob1 = prob1) |>
    group_by(hex_id) |>
    summarise(
      n_pos = sum(truth == "1")
    , n_neg = sum(truth == "0")
      ## Mann-Whitney rank-sum form of AUC, restricted to this hex's own rows
    , auc   = {
        r <- rank(prob1)
        if (sum(truth == "1") > 0 && sum(truth == "0") > 0) {
          (sum(r[truth == "1"]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
        } else {
          NA_real_
        }
      }
    , .groups = "drop"
    ) |>
    filter(n_pos > 0, n_neg > 0)

  if (nrow(by_hex) == 0) {
    return(tibble(within_hex_auc = NA_real_, within_hex_auc_n_pairs = 0))
  }

  n_pairs <- by_hex$n_pos * by_hex$n_neg

  tibble(
    within_hex_auc         = sum(by_hex$auc * n_pairs) / sum(n_pairs)
  , within_hex_auc_n_pairs = sum(n_pairs)
  )

}


#' Shared scoring helper used by both build_local_hyperparameter_grid_hexrelative and
#' finalize_hyperparameters_from_inner_hexrelative
#'
#' Reports the hex-relative score (S_pos_hex / S_neg_penalty_hex / final_score_hex), the original
#' raw score (S_pos / S_neg_penalty / final_score), AND their blend (final_score_combined =
#' final_score_hex + gamma * final_score) side by side per index -- all computed from the same
#' all_results tibble
#'
#' final_score and final_score_hex live on different natural scales for two separate reasons, so
#' they get independent penalty weights rather than one shared weightval:
#' (1) raw S_neg_penalty is naturally tiny (true prevalence ~0.0005, so -log(1-p) on true
#'     negatives is tiny near that operating point).
#' (2) hex-demeaned S_neg_penalty_hex is computed only over negative rows in hexes that
#'     have at least one event this fold x interval (see compute_metrics_vec_hexrelative) -- chronic
#'     (never-event) hexes are excluded entirely, since demeaning removes a hex's level exactly
#'     regardless of whether its raw prediction was well- or badly-calibrated, so those rows carry
#'     no information this term could use. Absolute miscalibration is penalized through the raw 
#'     S_neg_penalty term
#'
#' @title score_hexrelative_results
#'
#' @param all_results Combined tibble of per-(outer x inner x interval x index) result rows,
#'   as produced by tune_results_per_outer_fold_hexrelative
#' @param weightval_raw Numeric penalty weight on S_neg_penalty (raw, non-hex) relative to S_pos
#' @param weightval_hex Numeric penalty weight on S_neg_penalty_hex relative to S_pos_hex
#' @param gamma Numeric weight on the raw (non-hex) final_score when blending it into
#'   final_score_combined; gamma = 0 reduces to pure hex-relative selection
#' @return Tibble with one row per tuning-grid index: S_pos, S_neg_penalty, final_score (raw),
#'   S_pos_hex, S_neg_penalty_hex, final_score_hex, final_score_combined, within_hex_auc,
#'   n_pos_folds, n_total_folds, total_n_pos, weightval_raw, weightval_hex, gamma
#' @author Morgan Kain
#' @export

score_hexrelative_results <- function(all_results, weightval_raw, weightval_hex, gamma) {

  ## Much reused from the final_score calculation without the within-hex component
   ## The rest explained in comments above
  all_results |>
    group_by(index) |>
    summarise(
      S_pos             = -sum(logloss_pos * n_pos, na.rm = TRUE) /
                            pmax(sum(n_pos[!is.na(logloss_pos)], na.rm = TRUE), 1L)
    , S_neg_penalty     = sum(logloss_neg * n_all, na.rm = TRUE) / sum(n_all, na.rm = TRUE)
    , S_pos_hex         = -sum(logloss_pos_hex * n_pos, na.rm = TRUE) /
                            pmax(sum(n_pos[!is.na(logloss_pos_hex)], na.rm = TRUE), 1L)
      ## Weighted by n_neg_eventful, NOT n_all -- logloss_neg_hex is only computed over negative
       ## rows in hexes that have an event this call, so it must be weighted by that same count
    , S_neg_penalty_hex = sum(logloss_neg_hex * n_neg_eventful, na.rm = TRUE) /
                            pmax(sum(n_neg_eventful[!is.na(logloss_neg_hex)], na.rm = TRUE), 1L)
    , within_hex_auc    = sum(within_hex_auc * within_hex_auc_n_pairs, na.rm = TRUE) /
                            pmax(sum(within_hex_auc_n_pairs[!is.na(within_hex_auc)], na.rm = TRUE), 1L)
    , n_pos_folds       = sum(n_pos > 0)
    , n_total_folds     = n()
    , total_n_pos       = sum(n_pos)
    , .groups           = "drop"
    ) |>
    mutate(
      final_score          = S_pos - weightval_raw * S_neg_penalty
    , final_score_hex      = S_pos_hex - weightval_hex * S_neg_penalty_hex
    , final_score_combined = final_score_hex + gamma * final_score
    , weightval_raw        = weightval_raw
    , weightval_hex        = weightval_hex
    , gamma                = gamma
    )

}
