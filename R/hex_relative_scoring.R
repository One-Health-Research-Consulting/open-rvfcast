#####
## Helpers for tune_results_per_outer_fold, build_local_hyperparameter_grid, and
## finalize_hyperparameters_from_inner
#####

#' Internal function to calclate the score using hex-relative and "global" components.
#' For detailed notes see comments inside the function
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
#' @param index_flag Optional 0/1 vector (same row order/length as truth); current default 
#'   being to use country-level index cases. Passed to compute_metrics_vecto weight hyperparameter
#'   selection toward those cases (see get_rvf_response/lag_join_aggregate in rvf_data_processing_targets.R)
#' @return Tibble: all compute_metrics_vec columns plus n_hex, logloss_pos_hex, logloss_neg_hex,
#'   n_neg_eventful, within_hex_auc, within_hex_auc_n_pairs
#' @author Morgan Kain
#' @export

compute_metrics_vec_hexrelative <- function(truth, threshold, weightings, caseweights, prob1, hex_id
                                            , class_hat, event_level = "first", index_flag = NULL) {

  ## Compute the "global" calibration score component
  base_metrics <- compute_metrics_vec(
    truth       = truth
  , threshold   = threshold
  , weightings  = weightings
  , caseweights = caseweights
  , prob1       = prob1
  , class_hat   = class_hat
  , event_level = event_level
  , index_flag  = index_flag)

  ## Score Information ------------------------------------------------------------
  
  ## S_pos
  ## The larger this value (closer to 0), the higher the predicted probability is on
   ## true 1 days, in absolute terms, pooled across all hexes.
  
  ## S_neg_penalty
  ## The larger this value, the higher overall predicted probabilities are for true 
   ## 0s (across all hexes, pooled together, in absolute terms).
  ## ** larger weightval_raw_for_scoring makes the cost of over-predicting true 0s higher. 
  
  ## S_pos_hex
  ## The larger this value (closer to 0), the higher predicted probability rises 
   ## specifically on the days a real outbreak actually occurred, relative to that hex's
   ## own background level (computed from that hex's non-event days). That is, it measures
   ## how confidently elevated the model is exactly on the days that mattered, relative 
   ## to that hex's normal level.

  ## S_neg_penalty_hex
  ## The larger this value, the more predicted probability rises on non-event ("true 0") 
   ## days within hexes that have had at least one real event, relative to that same 
   ## hex's own background level — i.e., how badly the model fails to suppress risk on 
   ## the "wrong" (off-season) days, specifically in places that do have real risk. 
  ## ** larger weightval_hex_for_scoring makes the cost of failing to suppress off-season 
   ## risk within an at-risk hex higher. it pushes hyperparameter selection toward models 
   ## that only raise probability narrowly around real events, rather than staying elevated 
   ## broadly across an at-risk hex's whole year.

  ## within_hex_auc
   ## The larger this value, the better the model ranks true-1 days above true-0 days when 
   ## compared only to other days in that same hex (0.5 = no better than random guessing at 
   ## telling event days from non-event days in that hex; 1.0 = perfect). 
   ## NOTE: this is diagnostic-only — it doesn't feed into final_score_combined, it is 
   ## calculated as a sanity-check against which to compare other numbers.

  ## final_score
  ## Combination of S_pos and S_neg_penalty with the weighting value 
   ## weightval_raw_for_scoring

  ## final_score_hex
  ## Combination of S_pos_hex and S_neg_penalty_hex with the weighting value 
   ## weightval_hex_for_scoring

  ## final_score_combined
  ## Combination of final_score and final_score_hex with the weighting value gamma
  ## ** larger gamma puts more weight on the entire raw final_score. That is, a larger 
   ## gamma pulls the blended score towards “global” calibration: both better absolute 
   ## positive-day confidence and better absolute false-alarm control together
  
  ## logloss_neg_hex is restricted to negative rows belonging to hexes that have at least one event
  ## THIS call. Chronic hexes' own demeaned negative rows sit at a near-fixed ~-log(0.5) regardless 
  ## of that hex's absolute calibration (demeaning removes a hex's level exactly, whether the underlying
  ## prediction was well- or badly-calibrated), so including them would only dilute the one signal
  ## this term can actually measure (within-hex timing precision among hexes where timing is
  ## measurable) with a large mass of rows that cannot inform it either way. Absolute miscalibration
  ## is penalized through the raw (non-hex) S_neg_penalty term via gamma.
  
  
  ## Continue with the score calculation -------------------------------------------
  
  ## Compute the hex-relative score component. See notes above ^^
  prob1_hex <- compute_hex_relative_prob(prob1 = prob1, hex_id = hex_id, truth = truth)

  n_pos          <- length(which(truth == "1"))
  eventful_hexes <- unique(hex_id[truth == "1"])
  is_eventful    <- hex_id %in% eventful_hexes
  n_neg_eventful <- sum(truth == "0" & is_eventful)

  ## Clean/add some details
  hex_metrics <- tibble(
    n_hex           = n_distinct(hex_id)
  , logloss_pos_hex = if (n_pos == 0) NA_real_ else mean(-log(pmax(prob1_hex[truth == "1"], 1e-15)))
  , logloss_neg_hex = if (n_pos == 0) NA_real_ else
                      mean(-log(pmax(1 - prob1_hex[truth == "0" & is_eventful], 1e-15)))
  , n_neg_eventful  = n_neg_eventful)

  ## return
  bind_cols(
    base_metrics
  , hex_metrics
  , within_hex_auc_vec(truth = truth, prob1 = prob1, hex_id = hex_id))

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
       ## Note: Used for debugging of a sort, does not directly enter into the score
       ## calculation
    , auc   = {
        r <- rank(prob1)
        if (sum(truth == "1") > 0 && sum(truth == "0") > 0) {
          (sum(r[truth == "1"]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
        } else {
          NA_real_
        }
      }, .groups = "drop") |>
    filter(n_pos > 0, n_neg > 0)

  if (nrow(by_hex) == 0) return(tibble(within_hex_auc = NA_real_, within_hex_auc_n_pairs = 0))

  n_pairs <- by_hex$n_pos * by_hex$n_neg

  tibble(
    within_hex_auc         = sum(by_hex$auc * n_pairs) / sum(n_pairs)
  , within_hex_auc_n_pairs = sum(n_pairs))

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
#' @param delta Numeric weight on final_score_index (country-level index-case
#'   performance, see get_rvf_response/lag_join_aggregate) when blending it into
#'   final_score_combined; delta = 0 reduces to the pre-existing hex + gamma*raw blend.
#'   Since an index case is already one of the positives counted in S_pos/S_pos_hex,
#'   delta > 0 makes index-case performance count *again*, on top of that, so it has
#'   more influence on which hyperparameters get picked than an equally-well/badly
#'   predicted non-index positive
#' @return Tibble with one row per tuning-grid index: S_pos, S_neg_penalty, final_score (raw),
#'   S_pos_hex, S_neg_penalty_hex, final_score_hex, S_pos_index, final_score_index,
#'   final_score_rank, final_score_hex_rank, final_score_index_rank, final_score_combined,
#'   within_hex_auc, n_pos_folds, n_total_folds, total_n_pos, total_n_pos_index,
#'   weightval_raw, weightval_hex, gamma, delta
#' @author Morgan Kain
#' @export

score_hexrelative_results <- function(all_results, weightval_raw, weightval_hex, gamma, delta = 0) {

  ## ** NOTE: See commenting in compute_metrics_vec_hexrelative for more details

  if ("logloss_index" %in% names(all_results)) {
  
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
      ## Same pooling logic as S_pos, restricted to country-level index-case rows (see
       ## compute_metrics_vec); NA (contributes nothing) when a fold/interval/index has none
    , S_pos_index       = -sum(logloss_index * n_pos_index, na.rm = TRUE) /
                            pmax(sum(n_pos_index[!is.na(logloss_index)], na.rm = TRUE), 1L)
    , within_hex_auc    = sum(within_hex_auc * within_hex_auc_n_pairs, na.rm = TRUE) /
                            pmax(sum(within_hex_auc_n_pairs[!is.na(within_hex_auc)], na.rm = TRUE), 1L)
    , n_pos_folds       = sum(n_pos > 0)
    , n_total_folds     = n()
    , total_n_pos       = sum(n_pos)
    , total_n_pos_index = sum(n_pos_index, na.rm = TRUE)
    , .groups           = "drop"
    ) |>
    mutate(
      final_score             = S_pos - weightval_raw * S_neg_penalty
    , final_score_hex         = S_pos_hex - weightval_hex * S_neg_penalty_hex
      ## No separate negative-penalty term here: an index case is only ever a positive
       ## row, so there is no natural "false index alarm" to penalize symmetrically
    , final_score_index       = S_pos_index
      ## Percentile-rank each component across this pool of candidate indices before blending,
       ## so gamma/delta act as genuine relative weights regardless of the raw run-dependent 
       ## scale of S_pos/S_neg_penalty. Rank-normalizing solves a problem that was arising
       ## where a few very poor hypersets can cause gamma to simply strip these outliers out 
       ## and fail to do anything further with the fine-grained differences among the few 
       ## somewhat similar top sets. Rank-normalization strips out how much better certain 
       ## indices are then others, which could impact diagnostics/improvement, so do need to 
       ## be careful with this, though because our goal is to find the best set, 
       ## rank-normalization should be fine
    , final_score_rank        = rank_normalize(final_score)
    , final_score_hex_rank    = rank_normalize(final_score_hex)
    , final_score_index_rank  = rank_normalize(final_score_index)
    , final_score_combined    = final_score_hex_rank + gamma * final_score_rank + delta * final_score_index_rank
    , weightval_raw           = weightval_raw
    , weightval_hex           = weightval_hex
    , gamma                   = gamma
    , delta                   = delta
    )
    
  } else {
    
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
          ## See notes in the logloss_index branch above for why this is rank-normalized
           ## before blending rather than combined on the raw score
        , final_score_rank     = rank_normalize(final_score)
        , final_score_hex_rank = rank_normalize(final_score_hex)
        , final_score_combined = final_score_hex_rank + gamma * final_score_rank
        , weightval_raw        = weightval_raw
        , weightval_hex        = weightval_hex
        , gamma                = gamma
      )

  }

}


#' Convert a numeric vector to a percentile rank in [0,1] (0 = worst, 1 = best), so values
#' from differently-scaled score components can be blended as relative weights instead of raw, 
#' run-dependent magnitudes. Ties are averaged. A single non-NA value (n=1)
#' returns 1 for that value, since there is nothing to rank it against.
#'
#' @title rank_normalize
#'
#' @param x Numeric vector to rank-normalize
#' @return Numeric vector, same length/order as x, of percentile ranks in [0,1] (NA stays NA)
#' @author Morgan Kain
#' @export

rank_normalize <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1) return(ifelse(is.na(x), NA_real_, 1))
  r <- rank(x, ties.method = "average", na.last = "keep")
  (r - 1) / (n - 1)
}
