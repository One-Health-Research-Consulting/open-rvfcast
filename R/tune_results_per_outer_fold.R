#' Conduct the tuning over all inner folds for a given outer fold
#'
#'
#' @title tune_results_per_outer_fold

#' @param prejoined_data 1-row tibble with outer_fold_id and data (pre-joined inner fold covariates)
#' @param inner_ids_all Tibble of all (inner_fold_id, tune_grid_index, hyperparameters) for this outer fold
#' @param threshold For assigning a 1 | estimated prob
#' @param weightings weight assigned to 1s in binomial loss metric
#' @param start_p initilization point for base intercept
#' @param id_cols Columns that define a unique data point
#' @param out_dir Where to save output
#' @param tuning_grid_id id of the current tuning grid to track tuning better
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @param DEBUG If TRUE reduce to a small dataset for code testing
#' @param chunk_id Index of the (inner_fold x tune_grid) chunk this branch is responsible for.
#' @param checktime_path path to save csv tracking computation time
#' @param hex_id_col Column identifying the spatial hex
#' @param save_raw_predictions If TRUE, additionally persist per-row raw predictions
#'   (prob1, truth, hex_id, forecast_interval, inner_fold_id, spw_used) alongside the
#'   usual summarized metrics, for whichever (inner_fold_id, tune_grid_index) rows this
#'   call processes. Normally FALSE (raw predictions are computed but discarded --
#'   saving them for the full grid search would be a large, unnecessary storage cost);
#'   set TRUE only for a small, targeted rerun restricted to an already-chosen winning
#'   index, to harvest data for fit_k_correction() (see model_framework_targets.R).
#' @return Character vector of file paths, one per (inner_fold_id, tune_grid_index) combination
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold <- function(
    prejoined_data, inner_ids_all, threshold
  , weightings, start_p, id_cols, out_dir
  , tuning_grid_id, overwrite, DEBUG
  , chunk_id, checktime_path, hex_id_col = "shapeName"
  , save_raw_predictions = FALSE
) {

  ## Extract the outer fold ID and the pre-joined covariate data for this branch.
  ## joined_data already contains inner fold indices left-joined with train_data covariates,
  ## so no join is needed inside the loop -- only cluster-based filtering per iteration.
  outer_fold_id <- prejoined_data$outer_fold_id
  joined_data   <- prejoined_data$data[[1]]

  error_safe_read_file <- possibly(readRDS, NULL)

  ## Iterate over every (inner_fold_id, tune_grid_index) combination for this outer fold.
  ## Each fit is saved to its own file so partial progress survives a restart or error.
  save_filenames       <- character(nrow(inner_ids_all))

  checktime_tibble    <- tibble(user = numeric(0), sys = numeric(0), elapsed = numeric(0))

  ## chunk_id is folded into the filename because multiple chunks of the same outer fold now
  ## run concurrently (see cross(outer_fold_prejoined, chunk_id) in model_framework_targets.R);
  ## without it, concurrent branches would overwrite each other's timing log
  checktime_path.full <- paste0(checktime_path, "/outer_fold_", outer_fold_id, "_chunk_", chunk_id, "_hexrel.csv")

  for (i in seq_len(nrow(inner_ids_all))) {

    inner_ids   <- inner_ids_all[i, ]
    inner_id    <- inner_ids$inner_fold_id
    tuning_grid <- inner_ids |> dplyr::select(-contains("fold_id"))

    ## Create a new file saving convention so it doesn't conflict with the other option
    save_filename <- paste(
      out_dir
      , "/"
      , "inner_tuning_"
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

    checktime <- system.time({

      ## Inner training data: exclude one spatial cluster. country_index_outbreak is
       ## dropped here -- never a training predictor (by construction it's 1 only where
       ## outbreak is also 1, so leaving it in would be leakage), same treatment as cases
      inner_tbl_train <- joined_data |>
        dplyr::filter(cluster != inner_id) |>
        relocate(cluster, .after = "date") |>
        dplyr::select(-c(cluster, cases, country_index_outbreak)) |>
        mutate(outbreak = as.factor(outbreak)) |>
        mutate(forecast_interval = as.factor(forecast_interval))

      ## Class imbalance handled via scale_pos_weight in engine, not case weights
      spw <- calc_spw(inner_tbl_train)

      ## Inner assessment data: extract the held-out spatial cluster. country_index_outbreak
       ## is kept here (unlike inner_tbl_train above) -- a native column on joined_data (see
       ## get_rvf_response/lag_join_aggregate), used below as index_flag for
       ## compute_metrics_vec_hexrelative so hyperparameter selection can weight
       ## country-level index-case performance via score_hexrelative_results' delta
      ## weights stored as plain numeric (not hardhat_importance_weights) so that
      ## predict() on a workflow without add_case_weights() does not raise a type error
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

      if (DEBUG) inner_tbl_train <- inner_tbl_train[1:10000, ]

      ## Create scaffold recipe + model + workflow and fit model
      rec <- make_recipe(inner_tbl_train, id_cols = id_cols)
      mod <- make_model(params = tuning_grid, start_p = start_p, spw = spw)
      wf  <- workflow() |> add_model(mod) |> add_recipe(rec)

      fit <- fit(wf, data = inner_tbl_train)

      ## Free training objects before predictions to reduce peak memory within the loop
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

      ## Persist raw per-row predictions before they're discarded below, when requested
       ## (see save_raw_predictions doc above -- normally FALSE). spw_used is the actual
       ## effective scale_pos_weight this fit used (spw damped by spw_multiplier, if any),
       ## needed by fit_k_correction() since it varies per inner fold.
      if (save_raw_predictions) {
        raw_save_filename <- paste(
          out_dir, "/", "inner_raw_", "outer_fold_", paste(outer_fold_id, collapse = "_")
        , "_inner_fold_", inner_id, "_tune_grid_", tuning_grid_id, "_tune_index_", tuning_grid$index
        , ".Rds", sep = ""
        )
        saveRDS(
          tibble(
            prob1             = prob1
          , truth             = as.numeric(as.character(truth))
          , hex_id            = hex_id
          , forecast_interval = all_intervals
          , inner_fold_id     = inner_id
          , spw_used          = spw * resolve_spw_multiplier(tuning_grid)
          )
        , raw_save_filename
        )
      }

      ## Free fitted model before metrics computation
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
          , index_flag  = inner_tbl_assess |> filter(forecast_interval == this_int) |> pull(country_index_outbreak)
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

      ## Free assessment data and metrics before the next iteration
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

#' Finalize inner folds for all outer folds
#'
#'
#' @title prep_fold_ids

#' @param folded_data one row of the folded data
#' @param raw_data complete set of raw data
#' @return Tibble of inner folds per outer fold that have an outbreak
#' @author Morgan Kain
#' @export

prep_fold_ids  <- function(folded_data, raw_data) {

  all_inner_outer <- purrr::map(seq_len(nrow(folded_data)), function(outid) {

    ## Extract the needed data
    all_inner <- folded_data[outid, ]$inner_folds[[1]] |> left_join(raw_data$train_data[[1]], by = "index")

    ## Build the set of all inner train and assess datasets
    inner_tbl_set <- purrr::map(seq_along(unique(all_inner$cluster)), function(clust) {

      ## Inner assess data: only the left-out cluster
      assess_inner <- all_inner |>
        dplyr::filter(cluster == clust) |>
        relocate(cluster, .after = "date") |>
        dplyr::select(-c(cluster, forecast_interval, cases)) |>
        summarize(
          nrow = n()
        , tot_out = sum(outbreak))

      tibble(
        inner_fold_id = clust
      , nrow          = assess_inner$nrow
      , assess_inner  = assess_inner$tot_out
      )

    }) |>
      dplyr::bind_rows() |>
      filter(nrow > 0)

    inner_tbl_set |> mutate(outer_fold_id = folded_data[outid, ]$outer_fold_id, .before = 1)

  }) |>
  bind_rows()

  all_inner_outer

}
prep_outer_ids <- function(folded_data, raw_data, inner_ids) {

  all_outer <- purrr::map(seq_len(nrow(folded_data)), function(outid) {

    ## Extract the needed data and check for 1s in outbreak
    all_asses <- raw_data$train_data[[1]] |>
      filter(index %in% folded_data[outid, ]$assess_data[[1]]) |>
      summarize(n_out = n_distinct(outbreak)) |>
      mutate(outer_fold_id = folded_data[outid, ]$outer_fold_id, .before = 1) |>
      mutate(has_outbreak = ifelse(n_out > 1, 1, 0)) |>
      dplyr::select(-n_out)

    all_asses

  }) |>
  bind_rows()

 # inner_ids |> left_join(., all_outer) |> filter(has_outbreak == 1) |> dplyr::select(-has_outbreak)
  inner_ids |> left_join(all_outer)
}

#' Split one outer fold's (inner_fold x tune_grid) rows into n_chunks contiguous pieces
#' and return a single piece. Used to fan a single outer fold's tuning work out across
#' multiple concurrent branches (see inner_fold_ids_per_outer_chunked in model_framework_targets.R).
#'
#' @title chunk_rows
#' @param dat Tibble of rows to split (already filtered to one outer_fold_id)
#' @param n_chunks Number of pieces to split dat into
#' @param which Which piece (1-indexed) to return
#' @return Subset of dat's rows belonging to piece `which`
#' @author Morgan Kain
#' @export

chunk_rows <- function(dat, n_chunks, which) {

  n_chunks <- max(1L, n_chunks)

  if (nrow(dat) == 0) return(dat)

  ## Round-robin assignment rather than cut() -- cut() errors when n_chunks == 1 and gives
  ## uneven bins for small nrow(dat). Rows are already shuffled upstream
  ## (inner_fold_id_finalized / local_inner_fold_id_finalized), so round-robin over shuffled
  ## rows already balances load across chunks.
  dat[(seq_len(nrow(dat)) - 1L) %% n_chunks + 1L == which, ]

}


#' Select finalized hyperparameters by aggregating ALL inner-fold tuning results
#'
#' @title finalize_hyperparameters_from_inner

#' @param inner_folds Character vector of all file paths returned by the
#'   tuned_results_per_outer_fold target (one path per outer x inner x index branch)
#' @param weight_set Single-row tibble from chose_weight_set carrying weightval_raw,
#'   weightval_hex, gamma, delta
#' @param tuning_grid_id string for this tuning grid
#' @param outpath where to save the best hyperparameter set
#' @return Single-row tibble containing final_score_combined, final_score_hex, S_pos_hex,
#'   S_neg_penalty_hex, within_hex_auc, the raw (non-hex) final_score/S_pos/S_neg_penalty,
#'   hex_only_would_have_picked_index, raw_only_would_have_picked_index, and all hyperparameter values
#' @author Morgan Kain
#' @export

finalize_hyperparameters_from_inner <- function(inner_folds, weight_set, tuning_grid_id, outpath) {

  ## First, check if this tuning_grid_id already has a saved best parameter set
  if (file.exists(outpath)) return(outpath)

  ## Make the outpath if it doesn't exist yet
  create_data_directory(directory_path = strsplit(outpath, "/best_hyperparameters")[[1]][1])

  weightval_raw <- weight_set$weightval_raw
  weightval_hex <- weight_set$weightval_hex
  gamma         <- weight_set$gamma
  delta         <- weight_set$delta

  stopifnot(is.numeric(weightval_raw), length(weightval_raw) == 1, weightval_raw >= 0)
  stopifnot(is.numeric(weightval_hex), length(weightval_hex) == 1, weightval_hex >= 0)
  stopifnot(is.numeric(gamma), length(gamma) == 1, gamma >= 0)
  stopifnot(is.numeric(delta), length(delta) == 1, delta >= 0)

  ## Read every per-(outer x inner x index) result file into one long tibble
  all_results <- purrr::map(inner_folds, .f = function(x) {
    tload <- try(readRDS(x) |> dplyr::select(-recall_index), silent = TRUE)
    if (class(tload)[1] != "try-error") {
      tload
    } else {
      NULL
    }
  }) |>
  bind_rows()

  ## Do the scoring. Detaailed info on the scoring inside this function
  scores <- score_hexrelative_results(all_results, weightval_raw, weightval_hex, gamma, delta)

  ## Find the single best
  best <- scores |>
    arrange(desc(final_score_combined)) |>
    dplyr::slice(1)

  ## What a pure hex-relative selection (gamma = 0) and a pure raw selection would each have
  ## picked from this same pool of fits -- kept alongside so all three strategies are directly
  ## comparable from one tuning run
  hex_only_best_index <- scores |> arrange(desc(final_score_hex)) |> dplyr::slice(1) |> pull(index)
  raw_only_best_index <- scores |> arrange(desc(final_score))     |> dplyr::slice(1) |> pull(index)

  ## Cleanup/add details for export. any_of() rather than a bare column list: fits
   ## from the global grid never carry spw_multiplier (see calc_dial_best_set above
   ## for the same reasoning)
  best <- best |>
    left_join(
      all_results |>
        dplyr::select(index, trees, tree_depth, learn_rate, min_n, loss_reduction, mtry, dplyr::any_of("spw_multiplier")) |>
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

#' Locate the finalized hyperparameter set from the most recent PURPOSE = train run
#'
#' Used when PURPOSE = forecast so that finalized_hyperparameters can be populated without
#' rebuilding (or even defining) any of the tuning targets (tuning_grid, tuned_results_per_outer_fold,
#' local_tuning_grid, local_tuned_results, etc.) -- those targets only exist in the targets
#' graph when PURPOSE = train (see model_framework_targets.R), so forecasting just needs to
#' find whatever finalized hyperparameter file that train run already wrote out.
#'
#' @title get_latest_finalized_hyperparameters
#'
#' @param hyperparam_dir Directory finalize_hyperparameters_from_inner saves
#'   best_hyperparameters_combined_*.csv files into
#' @return Path to the most recently modified finalized hyperparameter csv
#' @author Morgan Kain
#' @export

get_latest_finalized_hyperparameters <- function(hyperparam_dir) {

  ## Only match the final (global + local) combined output, not the intermediate top-k
   ## checkpoint csv that build_local_hyperparameter_grid writes out mid-tuning
  candidates <- list.files(
    hyperparam_dir
  , pattern    = "^best_hyperparameters_combined_.*\\.csv$"
  , full.names = TRUE
  )

  if (length(candidates) == 0) {
    stop(
      "PURPOSE = forecast needs a hyperparameter set finalized by a prior PURPOSE = train run, "
    , "but none were found in '", hyperparam_dir, "'. Run PURPOSE = train at least once first."
    )
  }

  ## Most recently modified file == most recently completed training run
  candidates[which.max(file.mtime(candidates))]

}


#' Determine the best set across dial_hyperspace
#'
#' @title calc_dial_best_set

#' @param fits Character vector of individual result-file paths to score (e.g. the
#'   tuned_results_per_outer_fold target, or c(tuned_results_per_outer_fold, local_tuned_results)
#'   for a second pass over the combined global+local pool) -- NOT a directory to list; matches
#'   the convention already used by finalize_hyperparameters_from_inner
#' @param dial_hyperspace sobol of dial values
#' @param tuning_grid_id string for this tuning grid
#' @return List of full and summarized tibbles combining dial_hyperspace and details from best hyperset
#' @author Morgan Kain
#' @export

calc_dial_best_set <- function(fits, dial_hyperspace, tuning_grid_id) {

  ## Read every result file into one long tibble
  all_results <- purrr::map(fits, .f = function(x) {
    tload <- try(readRDS(x) |> dplyr::select(-recall_index), silent = TRUE)
    if (class(tload)[1] != "try-error") {
      tload
    } else {
      NULL
    }
  }) |>
  bind_rows()

  all_dials <- purrr::map(seq_len(nrow(dial_hyperspace)), .f = function(i) {

    this_set <- dial_hyperspace[i, ]

    scores <- score_hexrelative_results(
      all_results   = all_results
    , weightval_raw = this_set$weightval_raw_for_scoring
    , weightval_hex = this_set$weightval_hex_for_scoring
    , gamma         = this_set$gamma_for_combined_score
    , delta         = this_set$delta_for_index_score)

    ## Find the single best
    best <- scores |>
      arrange(desc(final_score_combined)) |>
      dplyr::slice(1)

    ## What a pure hex-relative selection (gamma = 0) and a pure raw selection would each have
    ## picked from this same pool of fits -- kept alongside so all three strategies are directly
    ## comparable from one tuning run
    hex_only_best_index <- scores |> arrange(desc(final_score_hex)) |> dplyr::slice(1) |> pull(index)
    raw_only_best_index <- scores |> arrange(desc(final_score))     |> dplyr::slice(1) |> pull(index)

    ## Cleanup/add details for export
    ## any_of() rather than a bare column list: fits from the global grid never carry
     ## spw_multiplier (that dimension only exists in the local refinement grid, see
     ## build_local_hyperparameter_grid), and a bare select() on a column that isn't
     ## present in every source would error
    best |>
      left_join(
        all_results |>
          dplyr::select(index, trees, tree_depth, learn_rate, min_n, loss_reduction, mtry, dplyr::any_of("spw_multiplier")) |>
          distinct(), by = "index") |>
      mutate(
          tuning_grid_id                    = tuning_grid_id
        , hex_only_would_have_picked_index  = hex_only_best_index
        , raw_only_would_have_picked_index  = raw_only_best_index
        , .before = index
      )

  }) |>
  bind_rows()

  all_dials.s <- all_dials |>
    group_by(index) |>
    summarize(
      n_entry              = n()
    , final_score_raw      = mean(final_score)
    , final_score_hex      = mean(final_score_hex)
    , final_score_combined = mean(final_score_combined)
    , S_pos                = mean(S_pos)
    , S_pos_hex            = mean(S_pos_hex)
    , S_neg_penalty        = mean(S_neg_penalty)
    , S_neg_penalty_hex    = mean(S_neg_penalty_hex)
    , weightval_raw        = mean(weightval_raw)
    , weightval_hex        = mean(weightval_hex)
    , gamma                = mean(gamma)
    )

  list(
    all_sets           = all_dials
  , summarized_indices = all_dials.s
  )

}


#' Choose a single set of weighting dials based on a given objective
#'
#' @title chose_weight_set

#' @param full_set All calculated scores across dial_hyperspace from calc_dial_best_set
#' @param summarized_sets Summarized dial values for each winning index from calc_dial_best_set
#' @param objective one of "balanced"; "minimize false positive"; or "maximize seasonal"
#' @return Tibble of single set of weighting parameter values for the rest of tuning
#' @author Morgan Kain
#' @export

chose_weight_set <- function(full_set, summarized_sets, objective) {

  if (objective == "balanced") {
    chosen_set <- summarized_sets |> filter(n_entry == max(n_entry))
  } else if (objective == "minimize false positive") {
    chosen_set <- summarized_sets |> filter(S_neg_penalty == min(S_neg_penalty))
  } else if (objective == "maximize seasonal") {
    chosen_set <- summarized_sets |> filter(final_score_hex == max(final_score_hex))
  } else {
    stop("Choose a supported option for objective")
  }

  dial_cols <- if ("delta" %in% names(full_set)) {
    c("weightval_raw", "weightval_hex", "gamma", "delta")
  } else {
    c("weightval_raw", "weightval_hex", "gamma")
  }

  ## Pick the evaluated draw closest to this winning index's own centroid
  full_set |>
    dplyr::filter(index == chosen_set$index) |>
    select_centroid_draw(full_set = full_set, cols = dial_cols)

}


#' Pick the evaluated draw closest to the centroid of a subset of dial-hyperspace draws,
#' in range-normalized distance. Used instead of averaging so the returned weighting values are
#' from a real, previously-evaluated point 
#'
#' @title select_centroid_draw
#'
#' @param draws Tibble of candidate draws to choose among (already filtered to one winning index)
#' @param full_set Tibble used only to establish each dial's explored range for normalization --
#'   the full, unfiltered pool of draws, not just `draws`, so the normalization scale reflects
#'   the whole search space rather than shrinking to whatever this particular subset spans
#' @param cols Character vector of dial column names to match on
#' @return One-row tibble, `draws` subset to `cols`, for the single closest-to-centroid row
#' @author Morgan Kain
#' @export

select_centroid_draw <- function(draws, full_set, cols) {

  ranges   <- purrr::map_dbl(cols, ~ diff(range(full_set[[.x]], na.rm = TRUE)))
  centroid <- purrr::map_dbl(cols, ~ mean(draws[[.x]], na.rm = TRUE))

  dist_sq <- purrr::map(seq_along(cols), function(i) {
    ((draws[[cols[i]]] - centroid[i]) / ranges[i])^2
  }) |>
    purrr::reduce(`+`)

  draws[which.min(dist_sq), cols]

}
