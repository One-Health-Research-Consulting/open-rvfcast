#' Forecast-focused evaluation of hex-level predictions
#'
#' Scores every prediction row (base date -by- hex -by- forecast window): predicted-probability
#' summaries by date and outcome, confusion counts at probability thresholds, probability
#' densities, maps of predictions, and rank-based alert tables (top share of hexes 
#' flagged per base date and window).
#'
#' @title evaluate_hex_forecasts
#' @param hex_preds Tibble with eval_set, date, shapeName, forecast_interval, prob_pred,
#'   true_out and prob_rank_pct
#' @param p_thresh Probability thresholds for the confusion counts
#' @param alert_fractions Alert fractions for the rank-based alert table
#' @param min_events_for_auc Minimum events before AUCs are computed for a group
#' @return List of tibbles: scores (by eval_set and window), alert_table, threshold_table,
#'   summary_probs
#' @author Morgan Kain
#' @export
evaluate_hex_forecasts <- function(
    hex_preds
  , p_thresh           = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
  , alert_fractions    = c(0.01, 0.05, 0.10)
  , min_events_for_auc = 5
) {

  ## Overall scores by evaluation set and window (AUCs, log loss, Brier skill,
   ## reliability/resolution) plus observed / expected events
  scores <- hex_preds |>
    dplyr::group_by(eval_set, forecast_interval) |>
    dplyr::group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc) |>
                          dplyr::mutate(expected_events = sum(.x$prob_pred), obs_over_exp = sum(.x$true_out) / sum(.x$prob_pred))) |>
    dplyr::ungroup()

  ## Rank-based alerts: recall, precision and "lift" when the top share of hexes is flagged
  alert_table <- tidyr::expand_grid(alert_fraction = alert_fractions) |>
    dplyr::cross_join(hex_preds |> dplyr::distinct(eval_set, forecast_interval)) |>
    purrr::pmap_dfr(function(alert_fraction, eval_set, forecast_interval) {
      d <- hex_preds |> dplyr::filter(.data$eval_set == .env$eval_set, .data$forecast_interval == .env$forecast_interval)
      flagged <- d$prob_rank_pct >= 1 - alert_fraction
      tibble::tibble(
        eval_set      = eval_set, forecast_interval = forecast_interval, alert_fraction = alert_fraction
      , share_flagged = mean(flagged)
      , recall        = sum(d$true_out[flagged]) / sum(d$true_out)
      , precision     = mean(d$true_out[flagged])
      , lift          = mean(d$true_out[flagged]) / mean(d$true_out))
    })

  ## Confusion counts at fixed probability thresholds for what probability defines a 
   ## predicted outbreak
  threshold_table <- purrr::map_dfr(p_thresh, function(th) {
    hex_preds |>
      dplyr::mutate(outbreak_assigned = as.numeric(prob_pred > th)) |>
      dplyr::count(eval_set, forecast_interval, true_out, outbreak_assigned, name = "n_rows") |>
      dplyr::mutate(positive_threshold = th, .before = 1)
  })

  ## Spread of predicted probabilities by base date, window and outcome 
  summary_probs <- hex_preds |>
    dplyr::summarize(
      lwr   = stats::quantile(prob_pred, 0.025)
    , lwr_n = stats::quantile(prob_pred, 0.200)
    , mid   = stats::quantile(prob_pred, 0.500)
    , upr_n = stats::quantile(prob_pred, 0.800)
    , upr   = stats::quantile(prob_pred, 0.975)
    , .by = c(eval_set, date, forecast_interval, true_out))

  list(
    scores          = scores
  , alert_table     = alert_table
  , threshold_table = threshold_table
  , summary_probs   = summary_probs
  )

}


#' Figures for evaluate_hex_forecasts() output
#'
#' @title plot_hex_forecasts
#' @param hex_scores Output of evaluate_hex_forecasts()
#' @param hex_preds The hex_preds passed to evaluate_hex_forecasts()
#' @param eval_set_to_plot Evaluation set drawn in the single-set figures
#' @return Named list of ggplots: scores_by_horizon, alert_recall, summary_probs,
#'   prob_density (maps are made by plot_hex_masked_forecasts())
#' @author Morgan Kain
#' @export
plot_hex_forecasts <- function(hex_scores, hex_preds, eval_set_to_plot) {

  plots <- list()

  ## Scores across windows, one line per evaluation set
  plots$scores_by_horizon <- hex_scores$scores |>
    dplyr::select(eval_set, forecast_interval, roc_auc, pr_auc, logloss, brier_skill, obs_over_exp) |>
    tidyr::pivot_longer(c(roc_auc, pr_auc, logloss, brier_skill, obs_over_exp), names_to = "metric", values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(forecast_interval, value, colour = eval_set)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::labs(x = "Forecast interval (days)", y = NULL, colour = NULL
                  , title = "Hex-level scores by forecast window"
                  , subtitle = "brier_skill: 0 = no better than climatology; obs_over_exp: 1 = right overall level") +
    ggplot2::theme_bw()

  ## Recall when the top share of hexes is flagged
  plots$alert_recall <- hex_scores$alert_table |>
    ggplot2::ggplot(ggplot2::aes(forecast_interval, recall, colour = factor(alert_fraction))) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::facet_wrap(~eval_set) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Forecast interval (days)", y = "Share of outbreaks `caught`"
                  , colour = "Alert fraction", title = "Recall of rank-based alerts") +
    ggplot2::theme_bw()

  ## Predicted-probability spread by date for outbreak and non-outbreak rows
  plots$summary_probs <- hex_scores$summary_probs |>
    dplyr::filter(eval_set == eval_set_to_plot) |>
    dplyr::mutate(forecast_interval = factor(forecast_interval), true_out = factor(true_out, labels = c("No outbreak", "Outbreak"))) |>
    ggplot2::ggplot(ggplot2::aes(date, mid, colour = forecast_interval)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr_n, ymax = upr_n), linewidth = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr, ymax = upr), linewidth = 0.2) +
    ggplot2::geom_point() +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~true_out, ncol = 1) +
    ggplot2::labs(x = "Base date", y = "Predicted probability", colour = "Forecast\ninterval") +
    ggplot2::theme_bw()

  ## Densities of predicted probability (logit scale) for outbreak and non-outbreak rows
  plots$prob_density <- hex_preds |>
    dplyr::filter(eval_set == eval_set_to_plot) |>
    dplyr::mutate(true_out = factor(true_out), logit_p = stats::qlogis(pmin(pmax(prob_pred, 1e-12), 1 - 1e-12))) |>
    ggplot2::ggplot(ggplot2::aes(logit_p)) +
    ggplot2::geom_density(ggplot2::aes(fill = true_out), alpha = 0.5, colour = NA) +
    ggplot2::facet_wrap(~forecast_interval, scales = "free") +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::labs(x = "Predicted probability (logit scale)", y = "Density", fill = "Outbreak") +
    ggplot2::theme_bw()

  plots

}


#' Historical reference forecast: each hex's training-period outbreak rate by calendar month
#'
#' Potentially (maybe) baseline a "forecaster" could make without a model: for every hex, calendar month of
#' the base date and forecast window, the share of training rows with an outbreak. Hex-months
#' never seen in training get the hex's overall rate, then the overall training rate.
#'
#' @title build_reference_rates
#' @param train_data Training data with shapeName, date, forecast_interval and outbreak
#' @return Tibble with shapeName, month, forecast_interval and ref_prob
#' @author Morgan Kain
#' @export
build_reference_rates <- function(train_data) {

  train_data |>
    dplyr::transmute(
      shapeName
    , month             = lubridate::month(date)
    , forecast_interval = as.numeric(as.character(forecast_interval))
    , outbreak          = as.numeric(as.character(outbreak))) |>
    dplyr::summarize(ref_prob = mean(outbreak), .by = c(shapeName, month, forecast_interval))

}


#' Per-hex context for interpreting forecast quality and masking hex maps
#'
#' One row per hex with what a user needs to judge its forecast quality; also the context used
#' by plot_hex_masked_forecasts() (the hex-level counterpart of build_region_context()):
#'  - mask skill: mean log-loss gain of prob_pred over a flat forecast (the eval set's hex
#'    outbreak rate for that window), pooled across windows, taken from the outer folds where
#'    the hex has outer-fold rows and from the test period otherwise; ranked (skill_rank) and
#'    classed into quality_class with the same levels as the regional maps. Test-period gain
#'    (test_logloss_gain) is kept alongside as a diagnostic
#'  - test-period summaries: typical (background) predicted probability, mean / median / max
#'    prediction, within-hex AUC, and log loss against the historical reference rate
#'    (logloss_gain_vs_ref > 0 = the model beat the hex's own historical rate)
#'  - outbreak history: labelled outbreaks (cases > 0) in the hex that started up to
#'    history_end, binned into history_class
#' Also adds per-row context to the test predictions: the ratio of each prediction to its hex's
#' background level and to the reference rate.
#'
#' @title build_hex_context
#' @param hex_preds Hex-level predictions of every evaluation set (eval_set, date, shapeName,
#'   forecast_interval, prob_pred, true_out and ref_prob)
#' @param hexed_outbreaks Tibble saved by export_hexed_outbreaks()
#' @param hex_info Tibble with shapeName, Country, lat and lon
#' @param history_end Last date counted as "prior" outbreak history (the day before the test period)
#' @param skill_sets Evaluation sets used for the mask skill, in order of preference
#' @param history_breaks Breaks for binning prior outbreak counts into history_class
#' @param history_labels Labels for the history_class bins
#' @return List with hex_context (one row per hex) and hex_preds (test predictions with context columns)
#' @author Morgan Kain
#' @export
build_hex_context <- function(
    hex_preds
  , hexed_outbreaks
  , hex_info
  , history_end
  , skill_sets     = c("outer_folds", "test")
  , history_breaks = c(-Inf, 0, 2, 10, Inf)
  , history_labels = c("No prior outbreaks", "1-2 prior outbreaks", "3-10 prior outbreaks", "> 10 prior outbreaks")
) {

  ## Mask skill (outer folds first), with the same score and classes as the regions
  mask_skill <- logloss_gain_by_unit(hex_preds, id_col = "shapeName", prob_col = "prob_pred") |>
    unit_mask_skill("shapeName", skill_sets)

  ## Background level: each hex's median test prediction on non-outbreak rows, per window
  test_preds <- hex_preds |> dplyr::filter(eval_set == "test")
  background <- test_preds |>
    dplyr::filter(true_out == 0) |>
    dplyr::summarize(hex_background_prob = stats::median(prob_pred), .by = c(shapeName, forecast_interval))

  ## Relative probability
  test_preds <- test_preds |>
    dplyr::left_join(background, by = c("shapeName", "forecast_interval")) |>
    dplyr::mutate(
      prob_ratio_to_background = prob_pred / hex_background_prob
    , prob_ratio_to_reference  = prob_pred / pmax(ref_prob, 1e-6))

  ## Per-hex test-period skill and "typical" probability predictions across all windows
  test_summary <- test_preds |>
    dplyr::summarize(
      n_test_rows          = dplyr::n()
    , n_test_outbreak_rows = sum(true_out)
    , mean_prob            = mean(prob_pred)
    , median_prob          = stats::median(prob_pred)
    , max_prob             = max(prob_pred)
    , mean_ref_prob        = mean(ref_prob, na.rm = TRUE)
      ## logloss as a key means of evaluating overall performance
    , test_logloss         = mean(rare_log_score_vec(prob_pred, true_out))
    , test_logloss_ref     = mean(rare_log_score_vec(pmax(ref_prob, 1e-6), true_out), na.rm = TRUE)
    , mean_prob_outbreak_rows    = if (any(true_out == 1)) mean(prob_pred[true_out == 1]) else NA_real_
    , mean_ratio_outbreak_rows   = if (any(true_out == 1)) mean(prob_ratio_to_background[true_out == 1]) else NA_real_
      ## Within-hex ranking of outbreak rows against the same hex's other rows
    , within_hex_auc = if (any(true_out == 1) && any(true_out == 0)) {
        r <- rank(prob_pred); n1 <- sum(true_out == 1); n0 <- sum(true_out == 0)
        (sum(r[true_out == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
      } else NA_real_
    , .by = shapeName) |>
    ## Positive = the model beat the hex's own historical rate on log loss
    dplyr::mutate(logloss_gain_vs_ref = test_logloss_ref - test_logloss)

  ## Labelled outbreaks are already assigned to hexes
  hex_outbreaks <- hexed_outbreaks |>
    dplyr::filter(counts_toward_label == 1) |>
    dplyr::select(shapeName, start_date, cases)

  hex_context <- hex_info |>
    dplyr::left_join(mask_skill, by = "shapeName") |>
    dplyr::left_join(test_summary, by = "shapeName") |>
    join_outbreak_history(hex_outbreaks, "shapeName", history_end, history_breaks, history_labels)

  list(hex_context = hex_context, hex_preds = test_preds)

}
