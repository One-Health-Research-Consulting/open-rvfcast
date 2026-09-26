#' Build a hyperparameter grid and save it for repeat use
#'
#'
#' @title build_hyperparameter_grid

#' @param tune_pars grid of parameter ranges
#' @param grid_path path to where to save parameter grid
#' @param overwrite boolean to overwrite or generate a new hyperparameter grid
#' @param folded_data_training
#' @param splitted_data
#' @param seed
#' @param min_capacity Minimum trees*learn_rate ("boosting capacity") a grid point must have
#'   to be kept; see sample_capacity_filtered_grid for why this is needed
#' @param id_cols Columns that define a unique data point (removed by the recipe); used to
#'   count the predictors the model actually sees, which sets mtry's upper bound
#' @return Tibble of search grid and other needs for model tuning
#' @author Morgan Kain
#' @export

build_hyperparameter_grid <- function(tune_pars, grid_path, folded_data_training, splitted_data
                                      , overwrite, seed, min_capacity = 20, id_cols) {

  ## Make the grid path
  create_data_directory(directory_path = grid_path)

  #### Hyperparameter search and tuning grid --------------------------------------

  set.seed(seed)
  ## Grid id from everything that shapes the grid, so changing a range, the capacity floor
   ## or the ID columns builds a new grid instead of silently reusing a saved one
  hyper_id  <- substr(digest::digest(list(tune_pars, min_capacity, seed, id_cols)), 1, 15)
  grid_path <- paste(grid_path, "/hypergrid_", hyper_id, ".Rds", sep = "")

  ## load previously saved if available for consistency
  ## Check if saved file exists and not overwrite
  if (file.exists(grid_path) && !overwrite) {

    par_grid <- readRDS(grid_path)

  } else {

    ## Candidate hyperparameter sets with trees*learn_rate below min_capacity have been shown
     ## empirically to never escape a constant, input-independent prediction on this severely
     ## imbalanced dataset -- they get rejected and resampled here rather than wasting tuning
     ## compute on guaranteed-degenerate fits. See sample_capacity_filtered_grid for why this
     ## can't be done with simple independent trees_min/learn_rate_min floors instead.
    par_grid <- with(tune_pars
         , sample_capacity_filtered_grid(
             trees_range   = c(tree_min, tree_max)
           , depth_range   = c(tree_dep_min, tree_dep_max)
           , lr_range      = c(learn_rate_min, learn_rate_max)
           , minn_range    = c(minn_min, minn_max)
           , lossred_range = c(loss_red_min, loss_red_max)
           , mtry_range_lo = mtry_min
           ## Every predictor the model sees after the recipe (dummy columns included)
           , mtry_range_hi = count_model_predictors(folded_data_training, splitted_data, id_cols)
           ## Total number of combinations of hyperparameters
           , size          = size
           , min_capacity  = min_capacity
           , seed          = seed
           )) |>
      mutate(index = seq_len(n()), .before = 1)

    saveRDS(par_grid, grid_path)

  }

  ## return
  tibble(
    par_grid = par_grid |> list()
  , grid_id  = hyper_id
  )

}


#' Build a local refinement hyperparameter grid centered on the top-k sets from global tuning
#'
#' Reads the saved per-(outer x inner x index) tuning result files produced by
#' tune_results_per_outer_fold, scores them with the same S_pos / S_neg_penalty
#' formula used in finalize_hyperparameters_from_inner, then builds a new
#' space-filling grid confined to the neighborhood of the top-k parameter sets.
#' Indices in the new grid start above max(global_grid$par_grid[[1]]$index) so
#' that local and global indices never collide when pooled in
#' finalize_hyperparameters_from_inner.
#'
#' @title build_local_hyperparameter_grid
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
#' @param delta Numeric weight on final_score_index (country-level index-case performance) in the
#'   blend; see score_hexrelative_results. 0 reduces to the pre-existing hex + gamma*raw blend
#' @param expansion Fraction of the top-k range to extend on each side (e.g. 0.5 = +/-50%)
#' @param grid_path Directory in which to save the local grid Rds
#' @param folded_data_training Folded training data (needed to finalise mtry upper bound)
#' @param splitted_data Split data object (needed to finalise mtry upper bound)
#' @param seed Random seed for reproducibility
#' @param min_capacity Minimum trees*learn_rate ("boosting capacity") a grid point must have
#'   to be kept; see sample_capacity_filtered_grid for why this is needed
#' @param spw_mult_range Range on the scaling of the class-imbalance weighting ratio; NULL
#'   (default) omits spw_multiplier from this grid entirely. In testing, spw_multiplier's effect 
#'   (which is purely on absolute prediction confidence) is resolved quite poorly at this stage,
#'   so NULL is the current plan (and optimizing as part of post-correction calibration). 
#'   Leaving it here for now though. 
#' @param id_cols Columns that define a unique data point (removed by the recipe); used to
#'   count the predictors the model actually sees, which sets mtry's upper bound
#' @return Single-row tibble with columns par_grid (list), grid_id (character, prefixed "localhex_"),
#'   weightval_raw, weightval_hex, gamma, delta
#' @author Morgan Kain
#' @export

build_local_hyperparameter_grid <- function(
    inner_fold_paths
    , global_grid
    , tune_pars
    , top_k
    , size
    , weightval_raw
    , weightval_hex
    , gamma
    , delta
    , expansion
    , grid_path
    , hyperparam_path
    , folded_data_training
    , splitted_data
    , seed
    , min_capacity   = 20
    , spw_mult_range = NULL
    , id_cols
) {

  create_data_directory(directory_path = grid_path)

  all_results <- purrr::map(inner_fold_paths, .f = function(x) {
    tload <- try(readRDS(x) |> dplyr::select(-recall_index), silent = TRUE)
    if (class(tload)[1] != "try-error") {
      return(tload)
    } else {
      return(NULL)
    }
  }) |> bind_rows()

  ## Do the scoring. Detailed info on the scoring inside this function
  scores <- score_hexrelative_results(all_results, weightval_raw, weightval_hex, gamma, delta)

  ## Extract out the top few indices
  top_indices <- scores |>
    arrange(desc(final_score_combined)) |>
    dplyr::slice(seq_len(top_k)) |>
    pull(index)

  ## Extract out the top few parameter sets
  top_params <- all_results |>
    dplyr::filter(index %in% top_indices) |>
    dplyr::select(index, trees, tree_depth, learn_rate, min_n, loss_reduction, mtry) |>
    distinct()

  ## Compute local bounds for each hyperparameter, hard-capped at the ORIGINAL global
  ## tune_pars bounds -- the local grid is a refinement and should never be allowed to
  ## search outside where the global grid already looked.
  ## learn_rate and loss_reduction are sampled on log10 scale by dials, so convert.
  trees_range   <- expand_range(top_params$trees, lo_hard = tune_pars$tree_min, hi_hard = tune_pars$tree_max, expansion = expansion, min_half_width = 50)
  depth_range   <- expand_range(top_params$tree_depth, lo_hard = tune_pars$tree_dep_min, hi_hard = tune_pars$tree_dep_max, expansion = expansion, min_half_width = 1)
  lr_range      <- expand_range(log10(top_params$learn_rate), lo_hard = tune_pars$learn_rate_min, hi_hard = tune_pars$learn_rate_max, expansion = expansion, min_half_width = 0.2)
  ## min_n is searched on log10 scale (see sample_capacity_filtered_grid), so widen it there
   ## too and convert back; min_half_width 0.3 is roughly a factor of 2 either way
  minn_range    <- 10^expand_range(log10(top_params$min_n), lo_hard = log10(tune_pars$minn_min), hi_hard = log10(tune_pars$minn_max), expansion = expansion, min_half_width = 0.3)
  lossred_range <- expand_range(log10(top_params$loss_reduction + .Machine$double.eps), lo_hard = tune_pars$loss_red_min, hi_hard = tune_pars$loss_red_max, expansion = expansion, min_half_width = 0.5)
  ## Keep mtry anchored within reach of the top-k observed values, but never below the global floor
  mtry_range_lo <- max(tune_pars$mtry_min, min(top_params$mtry) - 3L)

  ## Hash every parameter that determines this grid's content into its id, so a change in any of
   ## them produces a new file (forcing a rebuild) instead of silently reusing a stale one -- see
   ## the note above the function.
  param_sig <- digest::digest(list(weightval_raw, weightval_hex, gamma, delta, top_k, expansion, size, seed, min_capacity, spw_mult_range,
                                   ## The sets the grid is centred on, the global bounds and grid, and
                                    ## the ID columns (which set mtry's upper bound) also shape it
                                   top_params, tune_pars, global_grid$grid_id, id_cols))
  hyper_id  <- paste0("localhex_", param_sig)
  save_path <- paste0(grid_path, "/hypergrid_", hyper_id, ".Rds")

  if (file.exists(save_path)) {

    par_grid <- readRDS(save_path)

  } else {

    idx_offset <- max(global_grid$par_grid[[1]]$index)

    ## Same capacity-floor rejection/resampling as build_hyperparameter_grid -- the top-k sets
     ## this local grid is centred on are already known-good, but the +/- expansion can still
     ## push some candidates back into the degenerate trees*learn_rate zone, so guard here too.
    par_grid <- sample_capacity_filtered_grid(
        trees_range   = trees_range
      , depth_range   = depth_range
      , lr_range      = lr_range
      , minn_range    = minn_range
      , lossred_range = lossred_range
      , mtry_range_lo = mtry_range_lo
      ## Every predictor the model sees after the recipe (dummy columns included)
      , mtry_range_hi = count_model_predictors(folded_data_training, splitted_data, id_cols)
      , size          = size
      , min_capacity  = min_capacity
      , seed          = seed
        ## Pulled in as a new parameter -- not part of the global grid
      , spw_mult_range = spw_mult_range
      ) |>
      mutate(index = idx_offset + seq_len(n()), .before = 1)

    saveRDS(par_grid, save_path)

  }

  ## Save the intermediate best set as a tracking method to indicate the global
  ## tuning is finished
  write.csv(top_params, hyperparam_path)

  tibble(
    par_grid      = par_grid |> list()
  , grid_id       = hyper_id
  , weightval_raw = weightval_raw
  , weightval_hex = weightval_hex
  , gamma         = gamma
  , delta         = delta
  )

}

## Helper: extend the observed range by expansion on each side, clamped to hard limits.
## min_half_width prevents collapse when all top-k sets share the same value.
expand_range <- function(vals, lo_hard, hi_hard, expansion, min_half_width = 0) {
  lo_k <- min(vals, na.rm = TRUE)
  hi_k <- max(vals, na.rm = TRUE)
  pad  <- max((hi_k - lo_k) * expansion, min_half_width)
  c(max(lo_hard, lo_k - pad), min(hi_hard, hi_k + pad))
}

## Helper: build a space-filling grid, rejecting and resampling any point whose
## trees*learn_rate ("boosting capacity") falls below min_capacity. Confirmed empirically
## (see notes above score_hexrelative_results) that on this severely imbalanced dataset,
## hyperparameter sets below this capacity never escape a constant, input-independent
## prediction -- the ensemble never accumulates enough boosting rounds to move off its
## initial base-score guess, regardless of the other hyperparameters. The degenerate zone
## is bounded by the hyperbola trees*learn_rate = min_capacity, so a plain
## trees_min/learn_rate_min floor can't exclude it without also cutting off 
## "many trees, slow learn_rate" combinations.
## Update (Sep 2026): that collapse was caused by the outcome factor levels being ordered 0,1
## during tuning (so scale_pos_weight and base_score applied to the wrong class). With the
## levels fixed, a capacity of 2 still gives a varied, well-ranked prediction, so the
## pipeline now passes a much lower min_capacity.
##
## @param trees_range,depth_range,lr_range,minn_range,lossred_range Ranges passed straight
##   through to the matching dials::* range args (lr_range/lossred_range on log10 scale;
##   minn_range on the natural scale, sampled on a log10 scale here)
## @param mtry_range_lo,mtry_range_hi Lower and upper bounds on mtry (a count of predictors)
## @param size Desired number of grid points after capacity filtering
## @param min_capacity Minimum trees*learn_rate required to keep a candidate point
## @param seed Random seed
## @param max_attempts Safety cap on oversampling retries before giving up
## @return Tibble of up to `size` rows (fewer, with a warning, if min_capacity proves
##   unreachable within max_attempts), with no `index` column assigned yet
sample_capacity_filtered_grid <- function(
    trees_range, depth_range, lr_range, minn_range, lossred_range
  , mtry_range_lo, mtry_range_hi, size, min_capacity, seed, max_attempts = 6
  , spw_mult_range = NULL
) {

  oversample_mult <- 2
  attempt         <- 0
  kept            <- NULL

  ## Only the local refinement grid passes spw_mult_range (see
   ## build_local_hyperparameter_grid) -- the global grid's call site never does,
   ## so this extra dimension is fully optional/backward compatible
  spw_param <- if (!is.null(spw_mult_range)) {
    list(dials::new_quant_param(
      type = "double", range = spw_mult_range, inclusive = c(TRUE, TRUE)
    , label = c(spw_multiplier = "spw multiplier")
    ))
  } else list()

  repeat {

    attempt      <- attempt + 1
    request_size <- size * oversample_mult

    ## Vary the seed by attempt so a retry actually draws a fresh design rather than
    ## regenerating the same (still-insufficient) set of points
    set.seed(seed + attempt)
    candidate <- do.call(grid_space_filling, c(
        list(
          trees(range          = as.integer(trees_range))
        , tree_depth(range     = as.integer(depth_range))
        , learn_rate(range     = lr_range)
        ## min_n is drawn as a continuous log10 value so small values (1, 2, 5) get as much of
         ## the search as large ones; dials' integer min_n with a log transform does not spread
         ## evenly on the log scale in a multi-parameter design. Converted back below
        , dials::new_quant_param(
            type = "double", range = log10(minn_range), inclusive = c(TRUE, TRUE)
          , label = c(min_n_log10 = "log10 minimal node size"))
        , loss_reduction(range = lossred_range)
        , mtry(range           = as.integer(c(mtry_range_lo, mtry_range_hi)))
        )
      , spw_param
      , list(size = request_size)
      ))

    ## Back to whole-number min_n, in the column position the rest of the pipeline expects
    candidate <- candidate |>
      mutate(min_n = as.integer(round(10^min_n_log10)), .before = min_n_log10) |>
      dplyr::select(-min_n_log10)

    kept <- candidate |> dplyr::filter(trees * learn_rate >= min_capacity)

    if (nrow(kept) >= size || attempt >= max_attempts) break

    oversample_mult <- oversample_mult * 2

  }

  if (nrow(kept) < size) {
    warning(
      "sample_capacity_filtered_grid: only found ", nrow(kept), " of ", size
    , " requested points with trees*learn_rate >= ", min_capacity, " after ", attempt
    , " attempts; returning what was found. Consider widening trees/learn_rate ranges "
    , "or lowering min_capacity."
    )
    return(kept)
  }

  ## Random subset rather than the first rows: grid_space_filling returns its draws sorted by
   ## trees, so keeping the first rows would drop the upper half of the trees range
  kept |> dplyr::slice_sample(n = size)

}

## Helper: number of predictors the model sees after the recipe (ID columns removed, dummy
## columns added), used as mtry's upper bound. parsnip converts mtry to a share of these
## predictors for xgboost, so the bound must count them rather than raw data columns.
## Uses the same inner-fold training slice the grid has always used to size mtry.
count_model_predictors <- function(folded_data_training, splitted_data, id_cols) {

  ## Same column handling as the training tables in tune_results_per_outer_fold
  template <- folded_data_training$inner_folds[[10]] |>
    left_join(splitted_data$train_data[[1]], by = "index") |>
    filter(cluster != 1) |>
    dplyr::select(-dplyr::any_of(c("cluster", "cases", "country_index_outbreak"))) |>
    mutate(outbreak = factor(outbreak, levels = c(1, 0))) |>
    mutate(forecast_interval = as.factor(forecast_interval))

  length(get_predictor_names(make_recipe(template, id_cols = id_cols), template))

}
