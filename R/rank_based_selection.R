#' Rank hyperparameter sets on three fold-aware, level-independent components 
#'
#' Hyperparameter sets are ranked using 3 metrics:
#' A) pauc: standardized partial ROC AUC over false-alarm rates 0 to 5%, ranked within each
#'  (outer fold x inner fold x forecast window) cell with outbreaks. Ranking quality where
#'  alarms are actually raised (full ROC AUC is dominated by the mass of easy non-events)
#' B) within_hex: within-hex AUC, ranked within the same cells. Outbreak days ranked above
#'  non-outbreak days inside the same hex, so a model that only knows WHICH hexes are risky
#'  (no timing skill) scores 0.5 however good its spatial ranking
#' C) quiet: a penalty for false alarms in held-out clusters with no outbreaks, ranked within
#'  each (outer fold x forecast window). It is capped from below at quiet_cap, so it
#'  only penalizes sets whose quiet regions are not below the at-risk background
#'
#' @title score_rank_based
#'
#' @param all_results Combined tibble of per-(outer x inner x interval x index) tuning result
#'   rows, as produced by tune_results_per_outer_fold
#' @param weights Named weights for the three components: pauc, within_hex, quiet.
#' @param quiet_cap Lower cap on quiet elevation (natural-log ratio); -1 means quiet regions must
#'   sit at least about 2.7 times below the at-risk background to avoid any penalty
#' @param n_boot Number of outer-fold bootstrap resamples used to measure how often each set
#'   ranks first (0 skips the bootstrap)
#' @param seed Random seed for the bootstrap
#' @return Tibble, one row per index, best first: component ranks (mean rescaled cell ranks,
#'   0 = best), selection_score (their weighted mean, lower is better), share_first (bootstrap
#'   share ranking first; NA if n_boot = 0), pooled diagnostics (roc_auc, pauc_std,
#'   recall_top1, recall_top5, within_hex_auc, median quiet elevation, share of quiet cells
#'   above the cap, old log-loss terms) and the hyperparameter values
#' @author Morgan Kain
#' @export

score_rank_based <- function(all_results, weights = c(pauc = 0.4, within_hex = 0.4, quiet = 0.2)
                             , quiet_cap = -1, n_boot = 200, seed = 1) {

  stopifnot(all(c("pauc", "within_hex", "quiet") %in% names(weights)), all(weights >= 0), sum(weights) > 0)
  weights <- weights[c("pauc", "within_hex", "quiet")] / sum(weights)

  ## Results written before partial AUC was recorded fall back to full ROC AUC
  pauc_col <- if ("pauc_std" %in% names(all_results)) "pauc_std" else {
    warning("No pauc_std column in these tuning results; ranking on full ROC AUC instead")
    "roc_auc"
  }

  ## AUC components: rank sets within each outer x inner x window cell that has outbreaks
  auc_cells <- all_results |>
    dplyr::filter(n_pos > 0) |>
    dplyr::group_by(outer_fold_id, inner_fold_id, interval) |>
    dplyr::mutate(
      rank_pauc       = rescaled_rank(.data[[pauc_col]])
    , rank_within_hex = rescaled_rank(within_hex_auc)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(outer_fold_id, index, rank_pauc, rank_within_hex)

  ## Quiet component: rank sets within each outer x window on capped quiet elevation
  quiet_cells <- quiet_elevation_by_cell(all_results) |>
    dplyr::mutate(capped = pmax(quiet_elevation, quiet_cap)) |>
    dplyr::group_by(outer_fold_id, interval) |>
    ## Lower capped elevation is better, so rank its negative
    dplyr::mutate(rank_quiet = rescaled_rank(-capped)) |>
    dplyr::ungroup()

  scores <- component_means(auc_cells, quiet_cells) |>
    dplyr::mutate(selection_score = combine_components(rank_pauc, rank_within_hex, rank_quiet, weights)) |>
    dplyr::left_join(
      quiet_cells |>
        dplyr::group_by(index) |>
        ## Share first: summarise() evaluates in order, so it must see the per-cell values
         ## before quiet_elevation is replaced by its median
        dplyr::summarise(
          share_quiet_above_cap  = mean(quiet_elevation > quiet_cap)
        , quiet_elevation        = stats::median(quiet_elevation)
        , .groups = "drop")
    , by = "index")

  ## How often each set ranks first when outer folds are resampled
  scores$share_first <- if (n_boot > 0) {
    as.numeric(bootstrap_share_first(auc_cells, quiet_cells, weights, n_boot, seed)[as.character(scores$index)])
  } else NA_real_
  scores$share_first[is.na(scores$share_first) & n_boot > 0] <- 0

  scores |>
    dplyr::left_join(pooled_diagnostics(all_results), by = "index") |>
    dplyr::left_join(
      all_results |>
        dplyr::select(index, trees, tree_depth, learn_rate, min_n, loss_reduction, mtry, dplyr::any_of("spw_multiplier")) |>
        dplyr::distinct(index, .keep_all = TRUE)
    , by = "index") |>
    dplyr::arrange(selection_score)

}


#' Choose the final set: the top-ranked set, unless near-ties call for a more regularized one
#'
#' Among the top-ranked set (every set that ranks first in at least near_tie_share of the
#' outer-fold bootstrap resamples), pick the most regularized: highest min_n, then the fewest
#' trees x tree_depth. With score gaps at the level of fold noise, robustness against memorizing
#' hexes is a better tie-breaker than the noise itself. near_tie_share = 1 always returns the
#' top-ranked set.
#'
#' @title choose_near_tie_set
#'
#' @param scores Output of score_rank_based() (best first, with share_first)
#' @param near_tie_share Minimum bootstrap share of ranking first for a set to count as a near-tie
#' @return One-row tibble from scores for the chosen set, with selection_rule and
#'   top_ranked_index added
#' @author Morgan Kain
#' @export

choose_near_tie_set <- function(scores, near_tie_share = 0.1) {

  top <- scores$index[1]

  ## The top-ranked set is always a candidate, even if the bootstrap never puts it first
  candidates <- scores |>
    dplyr::filter(index == top | (!is.na(share_first) & share_first >= near_tie_share))

  chosen <- candidates |>
    dplyr::arrange(dplyr::desc(min_n), trees * tree_depth) |>
    dplyr::slice(1)

  chosen |>
    dplyr::mutate(
      top_ranked_index = top
    , selection_rule   = if (chosen$index == top) "top ranked" else
        paste0("most regularized of near-ties (", paste(candidates$index, collapse = ", "), ")")
    , .before = 1)

}


## Quiet elevation per set x outer fold x window, for outer fold x windows that have both
 ## quiet (no-outbreak) and outbreak held-out clusters
quiet_elevation_by_cell <- function(all_results) {
  
  all_results |>
    dplyr::mutate(quiet = n_pos == 0, n_neg = n_all - n_pos) |>
    dplyr::group_by(outer_fold_id, interval, index) |>
    dplyr::filter(any(quiet), any(!quiet)) |>
    dplyr::summarise(
      quiet_elevation = log(stats::weighted.mean(logloss_neg[quiet], n_neg[quiet])) -
                        log(stats::weighted.mean(logloss_neg[!quiet], n_neg[!quiet]))
    , .groups = "drop")
  
}

## Mean component ranks per set
component_means <- function(auc_cells, quiet_cells) {
  
  auc_cells |>
    dplyr::group_by(index) |>
    dplyr::summarise(
      n_cells         = dplyr::n()
    , rank_pauc       = mean(rank_pauc, na.rm = TRUE)
    , rank_within_hex = mean(rank_within_hex, na.rm = TRUE)
    , .groups = "drop") |>
    dplyr::full_join(
      quiet_cells |>
        dplyr::group_by(index) |>
        dplyr::summarise(n_quiet_cells = dplyr::n(), rank_quiet = mean(rank_quiet, na.rm = TRUE), .groups = "drop")
    , by = "index")
  
}

## Weighted combination of component ranks; a set missing a component counts as worst (1) on it
combine_components <- function(rank_pauc, rank_within_hex, rank_quiet, weights) {
  
  weights[["pauc"]]       * dplyr::coalesce(rank_pauc, 1) +
  weights[["within_hex"]] * dplyr::coalesce(rank_within_hex, 1) +
  weights[["quiet"]]      * dplyr::coalesce(rank_quiet, 1)
  
}

## Rank within a cell rescaled to 0 (best) to 1 (worst), so cells with different numbers of
## sets compare; NA stays NA and does not count toward the cell size
rescaled_rank <- function(x) {
  
  n <- sum(!is.na(x))
  if (n <= 1) return(ifelse(is.na(x), NA_real_, 0))
  (rank(-x, ties.method = "average", na.last = "keep") - 1) / (n - 1)
  
}

## Share of outer-fold bootstrap resamples in which each set has the best selection score
bootstrap_share_first <- function(auc_cells, quiet_cells, weights, n_boot, seed) {

  set.seed(seed)
  folds     <- unique(c(auc_cells$outer_fold_id, quiet_cells$outer_fold_id))
  auc_split <- split(auc_cells, auc_cells$outer_fold_id)
  qt_split  <- split(quiet_cells, quiet_cells$outer_fold_id)

  firsts <- vapply(seq_len(n_boot), function(b) {
    pick <- as.character(sample(folds, replace = TRUE))
    component_means(dplyr::bind_rows(auc_split[pick]), dplyr::bind_rows(qt_split[pick])) |>
      dplyr::mutate(score = combine_components(rank_pauc, rank_within_hex, rank_quiet, weights)) |>
      dplyr::slice_min(score, n = 1, with_ties = FALSE) |>
      dplyr::pull(index)
  }, numeric(1))

  prop.table(table(factor(firsts, levels = sort(unique(auc_cells$index)))))

}

## Pooled ranking measures and the old log-loss score terms per set, for reporting only
pooled_diagnostics <- function(all_results) {
  
  ## Measures only present in newer tuning results
  optional <- function(col) if (col %in% names(all_results)) all_results[[col]] else NA_real_
  all_results |>
    dplyr::mutate(
      .pauc = optional("pauc_std")
    , .top1 = optional("recall_top1")
    , .top5 = optional("recall_top5")) |>
    dplyr::group_by(index) |>
    dplyr::summarise(
      roc_auc           = stats::weighted.mean(roc_auc, n_pos, na.rm = TRUE)
    , pauc_std          = stats::weighted.mean(.pauc, n_pos, na.rm = TRUE)
    , recall_top1       = stats::weighted.mean(.top1, n_pos, na.rm = TRUE)
    , recall_top5       = stats::weighted.mean(.top5, n_pos, na.rm = TRUE)
    , within_hex_auc    = stats::weighted.mean(within_hex_auc, within_hex_auc_n_pairs, na.rm = TRUE)
    , S_pos             = -sum(logloss_pos * n_pos, na.rm = TRUE) / pmax(sum(n_pos[!is.na(logloss_pos)]), 1L)
    , S_neg_penalty     = sum(logloss_neg * n_all, na.rm = TRUE) / sum(n_all, na.rm = TRUE)
    , S_pos_hex         = -sum(logloss_pos_hex * n_pos, na.rm = TRUE) / pmax(sum(n_pos[!is.na(logloss_pos_hex)]), 1L)
    , S_neg_penalty_hex = sum(logloss_neg_hex * n_neg_eventful, na.rm = TRUE) /
                            pmax(sum(n_neg_eventful[!is.na(logloss_neg_hex)]), 1L)
    , .groups = "drop")
  
}
