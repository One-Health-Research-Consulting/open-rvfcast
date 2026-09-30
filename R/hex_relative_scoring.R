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
  ## Hyperparameter selection now uses fold-aware ranks of pooled ROC AUC and within_hex_auc
   ## (see score_rank_based). The log-loss terms below are still computed and reported, but
   ## only as diagnostics: they depend on the probability level, which calibration handles

  ## S_pos
  ## The larger this value (closer to 0), the higher the predicted probability is on
   ## true 1 days, in absolute terms, pooled across all hexes.
  
  ## S_neg_penalty
  ## The larger this value, the higher overall predicted probabilities are for true 
   ## 0s (across all hexes, pooled together, in absolute terms).
  
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
  ## NOTE: demeaning makes each hex's non-event rows average zero on the log-odds scale, so
   ## this term measures the SPREAD of non-event predictions and is lowest for a flat model.
   ## It therefore penalizes any within-hex timing signal and is not used for selection.

  ## within_hex_auc
   ## The larger this value, the better the model ranks true-1 days above true-0 days when 
   ## compared only to other days in that same hex (0.5 = no better than random guessing at 
   ## telling event days from non-event days in that hex; 1.0 = perfect). 
   ## Used for hyperparameter selection (see score_rank_based).

  ## logloss_neg_hex is restricted to negative rows belonging to hexes that have at least one event
  ## THIS call. Chronic hexes' own demeaned negative rows sit at a near-fixed ~-log(0.5) regardless 
  ## of that hex's absolute calibration (demeaning removes a hex's level exactly, whether the underlying
  ## prediction was well- or badly-calibrated), so including them would only dilute the one signal
  ## this term can actually measure (within-hex timing precision among hexes where timing is
  ## measurable) with a large mass of rows that cannot inform it either way.

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

