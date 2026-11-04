#' Forecasts of any outbreak within the whole forecast horizon
#'
#' Scores the probability of at least one outbreak in the full forecast horizon (days 1-149
#' from the base date: the union of the five consecutive forecast windows) for hexes and, when
#' the regional copula ran, for regions. The model forecasts each window separately, so the
#' per-window probabilities are combined two ways that bound P(any) when outbreaks in
#' consecutive windows are positively associated:
#'  - p_indep: 1 - prod(1 - p_window) (windows independent; upper bound)
#'  - p_max:   largest window probability (perfect dependence; lower bound)
#' Regional rows combine the out-of-fold copula probabilities (p_copula) of each window.
#' A base date is only scored for a hex / region with probabilities for every window.
#'
#' @title evaluate_cumulative_forecasts
#' @param hex_preds Tibble with eval_set, date, shapeName, forecast_interval, prob_pred and true_out
#' @param regional_oof Out-of-fold regional rows (evaluate_regional_copula()$oof), or NULL
#' @param forecast_windows Tibble with forecast_interval and window_lower
#' @param alert_fractions Alert fractions for the rank-based alert table
#' @param min_events_for_auc Minimum events before AUCs are computed for a group
#' @return List with tables (scores and alert_table by unit, eval_set and combination rule)
#'   and plots (reliability on the test set)
#' @author Morgan Kain
#' @export
evaluate_cumulative_forecasts <- function(
    hex_preds
  , regional_oof       = NULL
  , forecast_windows
  , alert_fractions    = c(0.01, 0.05, 0.10)
  , min_events_for_auc = 5
) {

  n_windows <- nrow(forecast_windows)

  ## One row per unit, base date and evaluation set over the whole horizon
  rows <- list(Hex = cumulative_rows(hex_preds, c("eval_set", "date", "shapeName"), "prob_pred", n_windows))
  if (!is.null(regional_oof)) {
    rows$Region <- cumulative_rows(regional_oof, c("eval_set", "date", "area_index"), "p_copula", n_windows)
  }

  ## Long format: one row per combination rule, with each row's rank among the units of its base date
  long <- purrr::imap_dfr(rows, ~ .x |> dplyr::mutate(unit = .y, .before = 1)) |>
    tidyr::pivot_longer(c(p_indep, p_max), names_to = "method", values_to = "prob_pred") |>
    dplyr::mutate(prob_rank_pct = dplyr::percent_rank(prob_pred), .by = c(unit, eval_set, method, date))

  ## Same scores as the per-window hex and regional sections, plus information gain over a flat
   ## forecast and Molchan area skill
  scores <- long |>
    dplyr::group_by(unit, eval_set, method) |>
    dplyr::group_modify(~ dplyr::bind_cols(
      score_regional_method(.x, min_events_for_auc)
    , rare_basic_scores(.x$prob_pred, .x$true_out) |> dplyr::select(info_gain_nats_per_event, prob_gain_per_event)
    , tibble::tibble(ASS_tau_0.05 = rare_area_skill_score(rare_molchan_curve(.x$prob_pred, .x$true_out), 0.05)))) |>
    dplyr::ungroup()

  ## Recall when the top share of units on each base date is flagged
  alert_table <- tidyr::expand_grid(alert_fraction = alert_fractions) |>
    dplyr::cross_join(long) |>
    dplyr::mutate(flagged = prob_rank_pct >= 1 - alert_fraction) |>
    dplyr::summarize(
      share_flagged = mean(flagged)
    , recall        = sum(true_out[flagged]) / sum(true_out)
    , precision     = mean(true_out[flagged])
    , .by = c(unit, eval_set, method, alert_fraction))

  list(
    tables = list(scores = scores, alert_table = alert_table)
  , plots  = list(reliability = plot_cumulative_reliability(long, "test"))
  )

}


## Whole-horizon rows: the outcome is any outbreak in any window, the probability is combined
## across windows; units missing a window on a base date are dropped
cumulative_rows <- function(df, keys, prob_col, n_windows) {

  df |>
    dplyr::summarize(
      n_present = dplyr::n()
    , true_out  = max(true_out)
    , p_indep   = 1 - prod(1 - .data[[prob_col]])
    , p_max     = max(.data[[prob_col]])
    , .by = dplyr::all_of(keys)) |>
    dplyr::filter(n_present == n_windows) |>
    dplyr::select(-n_present)

}

## Reliability of the whole-horizon probabilities: observed rate vs mean prediction in
## probability bins (log scales), by unit and combination rule
plot_cumulative_reliability <- function(long, eval_set_to_plot,
                                        bins = c(0, 1e-4, 3e-4, 1e-3, 3e-3, 0.01, 0.03, 0.1, 0.3, 1)) {

  long |>
    dplyr::filter(eval_set == eval_set_to_plot) |>
    dplyr::mutate(
      method = factor(method, levels = c("p_indep", "p_max"), labels = c("Independent windows (upper)", "Largest window (lower)"))
    , bin    = cut(prob_pred, bins, include.lowest = TRUE)) |>
    dplyr::summarize(n = dplyr::n(), pred = mean(prob_pred), obs = mean(true_out), n_events = sum(true_out), .by = c(unit, method, bin)) |>
    dplyr::filter(n_events > 0) |>
    ggplot2::ggplot(ggplot2::aes(pred, obs)) +
    ggplot2::geom_abline(linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(ggplot2::aes(size = n)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::facet_grid(unit ~ method) +
    ggplot2::labs(x = "Predicted P(any outbreak, days 1-149)", y = "Observed rate", size = "Rows"
                  , title = "Reliability of whole-horizon forecasts") +
    ggplot2::theme_bw()

}
