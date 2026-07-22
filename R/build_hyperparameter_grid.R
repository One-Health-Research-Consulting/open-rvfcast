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
#' @return Tibble of search grid and other needs for model tuning
#' @author Morgan Kain
#' @export

build_hyperparameter_grid <- function(tune_pars, grid_path, folded_data_training, splitted_data
                                      , overwrite, seed) {

  ## Make the grid path
  create_data_directory(directory_path = grid_path)

  #### Hyperparameter search and tuning grid --------------------------------------

  set.seed(seed)
  hyper_id  <- stringi::stri_rand_strings(1, length = 15, pattern = "[A-Za-z0-9]")
  grid_path <- paste(grid_path, "/hypergrid_", hyper_id, ".Rds", sep = "")

  ## load previously saved if available for consistency
  ## Check if saved file exists and not overwrite
  if (file.exists(grid_path) && !overwrite) {

    par_grid <- readRDS(grid_path)

  } else {

    par_grid <- with(tune_pars
         ## Number of alternative grid options available, but space_filling efficient
         ## NOTE: Could possibly do a bit better to save some computation time by
         ## cutting out some of the parameter space where the combination of has some
         ## combination of hyperparameters that don't make a lot of sense
         , grid_space_filling(
             trees(range          = c(tree_min, tree_max))
           , tree_depth(range     = c(tree_dep_min, tree_dep_max))
           , learn_rate(range     = c(learn_rate_min, learn_rate_max))
           , min_n(range          = c(minn_min, minn_max))
           , loss_reduction(range = c(loss_red_min, loss_red_max))
           ## Arbitrary choice here in which train_inner, doesn't matter which
           , finalize(mtry(range = c(mtry_min, unknown())), folded_data_training$inner_folds[[10]] |>
                        left_join(
                          splitted_data$train_data[[1]], by = "index") |>
                        filter(cluster != 1))
           ## Total number of combinations of hyperparameters
           , size = size)) |>
      mutate(index = seq_len(n()), .before = 1)

    saveRDS(par_grid, grid_path)

  }

  ## return
  tibble(
    par_grid = par_grid |> list()
  , grid_id  = hyper_id
  )

}


#' Build a local refinement hyperparameter grid centred on the top-k sets from global tuning
#'
#' Reads the saved per-(outer x inner x index) tuning result files produced by
#' tune_results_per_outer_fold, scores them with the same S_pos / S_neg_penalty
#' formula used in finalize_hyperparameters_from_inner, then builds a new
#' space-filling grid confined to the neighbourhood of the top-k parameter sets.
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
#' @param expansion Fraction of the top-k range to extend on each side (e.g. 0.5 = +/-50%)
#' @param grid_path Directory in which to save the local grid Rds
#' @param folded_data_training Folded training data (needed to finalise mtry upper bound)
#' @param splitted_data Split data object (needed to finalise mtry upper bound)
#' @param seed Random seed for reproducibility
#' @return Single-row tibble with columns par_grid (list), grid_id (character, prefixed "localhex_"),
#'   weightval_raw, weightval_hex, gamma
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
  
  ## Do the scoring. Detaailed info on the scoring inside this function
  scores <- score_hexrelative_results(all_results, weightval_raw, weightval_hex, gamma)
  
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
  minn_range    <- expand_range(top_params$min_n, lo_hard = tune_pars$minn_min, hi_hard = tune_pars$minn_max, expansion = expansion, min_half_width = 5)
  lossred_range <- expand_range(log10(top_params$loss_reduction + .Machine$double.eps), lo_hard = tune_pars$loss_red_min, hi_hard = tune_pars$loss_red_max, expansion = expansion, min_half_width = 0.5)
  ## Keep mtry anchored within reach of the top-k observed values, but never below the global floor
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

## Helper: extend the observed range by expansion on each side, clamped to hard limits.
## min_half_width prevents collapse when all top-k sets share the same value.
expand_range <- function(vals, lo_hard, hi_hard, expansion, min_half_width = 0) {
  lo_k <- min(vals, na.rm = TRUE)
  hi_k <- max(vals, na.rm = TRUE)
  pad  <- max((hi_k - lo_k) * expansion, min_half_width)
  c(max(lo_hard, lo_k - pad), min(hi_hard, hi_k + pad))
}
