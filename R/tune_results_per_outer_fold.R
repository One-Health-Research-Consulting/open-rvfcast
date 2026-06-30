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
#' @param hyperparam_path path to where the best hyperparameter file will be / is saved
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @param DEBUG If TRUE reduce to a small dataset for code testing
#' @param checktime_path path to save csv tracking computation time
#' @return Character vector of file paths, one per (inner_fold_id, tune_grid_index) combination
#' @author Morgan Kain
#' @export

tune_results_per_outer_fold <- function(prejoined_data, inner_ids_all, threshold
                                      , weightings, start_p, id_cols, out_dir
                                      , tuning_grid_id, hyperparam_path, overwrite, DEBUG
                                      , checktime_path) {

  ## First, check if this tuning_grid_id already has a saved best parameter set
  if (file.exists(hyperparam_path)) {
    return(hyperparam_path)
  }

  ## Extract the outer fold ID and the pre-joined covariate data for this branch.
  ## joined_data already contains inner fold indices left-joined with train_data covariates,
  ## so no join is needed inside the loop -- only cluster-based filtering per iteration.
  outer_fold_id <- prejoined_data$outer_fold_id
  joined_data   <- prejoined_data$data[[1]]

  error_safe_read_file <- possibly(readRDS, NULL)

  ## Iterate over every (inner_fold_id, tune_grid_index) combination for this outer fold.
  ## Each fit is saved to its own file so partial progress survives a restart or error.
  save_filenames <- character(nrow(inner_ids_all))

checktime_tibble <- tibble(user = numeric(0), sys = numeric(0), elapsed = numeric(0))

checktime_path.full <- paste0(checktime_path, "/outer_fold_", outer_fold_id, ".csv")

  for (i in seq_len(nrow(inner_ids_all))) {

    inner_ids   <- inner_ids_all[i, ]
    inner_id    <- inner_ids$inner_fold_id
    tuning_grid <- inner_ids |> dplyr::select(-contains("fold_id"))

    ## Set filename (identical naming convention to the previous single-branch design)
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

    ## Inner training data: exclude one spatial cluster
    inner_tbl_train <- joined_data |>
      dplyr::filter(cluster != inner_id) |>
      relocate(cluster, .after = "date") |>
      dplyr::select(-c(cluster, cases)) |>
      mutate(outbreak = as.factor(outbreak)) |>
      mutate(forecast_interval = as.factor(forecast_interval))

    ## Class imbalance handled via scale_pos_weight in engine, not case weights
    spw <- calc_spw(inner_tbl_train)

    ## Inner assessment data: extract the held-out spatial cluster
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

    if (DEBUG) {
      inner_tbl_train <- inner_tbl_train[1:10000, ]
    }

    ## Create scaffold recipe + model + workflow and fit model
    rec <- make_recipe(inner_tbl_train, id_cols = id_cols)
    mod <- make_model(params = tuning_grid, start_p = start_p, spw = spw)
    wf  <- workflow() |> add_model(mod) |> add_recipe(rec)

    print("At model fitting")
    fit <- fit(wf, data = inner_tbl_train)
    print("Finished with model fitting")

    ## Free training objects before predictions to reduce peak memory within the loop
    rm(inner_tbl_train, rec, mod, wf)
    gc()

    ## Predictions: prob only
    prob1     <- predict(fit, inner_tbl_assess, type = "prob")$.pred_1
    truth     <- factor(inner_tbl_assess[["outbreak"]], levels = c("1", "0"))
    class_hat <- apply(
      threshold |> matrix()
    , 1
    , FUN = function(x) factor(ifelse(prob1 >= x, "1", "0"), levels = c("1", "0"))
    )
    all_intervals     <- inner_tbl_assess$forecast_interval
    forecast_interval <- all_intervals |> unique() |> as.character() |> as.numeric() |> sort()

    ## Free fitted model before metrics computation
    rm(fit)
    gc()

    ## Compute metrics
    metrics <- purrr:::map(forecast_interval, .f = function(this_int) {

      truth.t     <- truth[which(all_intervals == this_int)]
      prob1.t     <- prob1[which(all_intervals == this_int)]
      class_hat.t <- class_hat[which(all_intervals == this_int), ]

      compute_metrics_vec(
        truth       = truth.t
      , threshold   = threshold
      , weightings  = weightings
      , caseweights = inner_tbl_assess |> filter(forecast_interval == this_int) |> pull(weights)
      , prob1       = prob1.t
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


#' Select finalized hyperparameters by aggregating ALL inner-fold tuning results
#'
#' @title finalize_hyperparameters_from_inner

#' @param inner_folds Character vector of all file paths returned by the
#'   tuned_results_per_outer_fold target (one path per outer x inner x index branch)
#' @param metric Character; only "mix" is currently supported
#' @param weightval Numeric >= 0; penalty weight on S_neg_penalty (false-alarm log-loss) relative
#'   to S_pos (positive log-loss). Larger values suppress false alarms more aggressively at the
#'   cost of potentially missing outbreaks. Values in the range 1-5 are reasonable for rare events.
#' @param direction Character, max or min
#' @param tuning_grid_id string for this tuning grid
#' @param outpath where to save the best hyperparameter set
#' @return Single-row tibble containing final_score, S_pos, S_neg_penalty,
#'   n_pos_folds (number of folds with at least one positive case), n_total_folds,
#'   total_n_pos, metric, weightval, index, and all hyperparameter values
#'   (trees, tree_depth, learn_rate, min_n, loss_reduction, mtry)
#' @author Morgan Kain
#' @export

finalize_hyperparameters_from_inner <- function(inner_folds, metric, weightval, direction
                                                , tuning_grid_id, outpath) {

  ## First, check if this tuning_grid_id already has a saved best parameter set
  if (file.exists(outpath)) return(outpath)

  ## Make the outpath if it doesn't exist yet
  create_data_directory(directory_path = strsplit(outpath, "/best_hyperparameters")[[1]][1])

  stopifnot(metric    == "mix")
  ## For my current setup only max makes sense
  stopifnot(direction == "max")
  stopifnot(is.numeric(weightval), length(weightval) == 1, weightval >= 0)

  ## Read every per-(outer x inner x index) result file into one long tibble
  all_results <- apply(inner_folds |> matrix(), 1, FUN = readRDS) |> bind_rows()

  #### Notes about this scoring metric ---------------------------------------------
  
  ## *S_pos*: n_pos-weighted mean of -(logloss_pos), where logloss_pos is the per-fold mean
  ## log-loss computed only on true 1s. Rewards predicting outbreak probability high where
  ## outbreaks actually occur. The null model (predict prevalence ~0.005 everywhere) gets
  ## S_pos ≈ -5.3, so it cannot "hide at zero"

  ## Folds without any true 1s have logloss_pos = NA and contribute 0 to S_pos automatically.
  
  ## *S_neg_penalty*: n_all-weighted mean logloss_neg, the per-fold mean log-loss on true 0s.
  ## Folds without any true 1s contribute here, preserving the ability for this strategy to
  ## penalize high probabilities for true 0s even if they don't contribute to S_pos.
  
  ## final_score = S_pos - weightval * S_neg_penalty  (maximise)
  
  ## Larger weightval suppresses false alarms more aggressively.
  
  scores <- all_results |>
    group_by(index) |>
    summarise(
      ## score using the logloss_pos (see above) focused on estimated probabilities
       ## for true 1s
      S_pos = -sum(logloss_pos * n_pos, na.rm = TRUE) /
               pmax(sum(n_pos[!is.na(logloss_pos)], na.rm = TRUE), 1L)
      ## place where high estimated probabilities for true 0s get penalized 
    , S_neg_penalty = sum(logloss_neg * n_all, na.rm = TRUE) /
                      sum(n_all, na.rm = TRUE)
      ## Summary stuff 
    , n_pos_folds   = sum(n_pos > 0)
    , n_total_folds = n()
    , total_n_pos   = sum(n_pos)
    , .groups       = "drop"
    ) |>
    mutate(
      ## The final score is how well the model predicts true 1s 
       ## minus how badly it over-predicts outbreak probability for true 0s 
       ## multiplied by how much we want to weight this penalty
      final_score = S_pos - weightval * S_neg_penalty
    , metric      = metric
    , weightval   = weightval
    )

  ## Quick visualization of score across all parameter combinations
  # scores |> arrange(desc(final_score)) |> mutate(ii = seq(n()) |> as.factor()) |> {ggplot(_, aes(ii, final_score)) + geom_point()}

  ## Select the single index with the highest combined score
  best <- scores |>
    arrange(desc(final_score)) |>
    dplyr::slice(1)

  ## Recover the hyperparameter values for the winning index.
  ## trees / tree_depth / learn_rate / min_n / loss_reduction / mtry are constant
  ## across all rows sharing an index, so distinct() always yields exactly one row.
  best <- best |>
    left_join(
      all_results |>
        dplyr::select(index, trees, tree_depth, learn_rate, min_n, loss_reduction, mtry) |>
        distinct(), by = "index") |>
    mutate(
      tuning_grid_id = tuning_grid_id, .before = index
    )

  write.csv(best, outpath)

  outpath

}
