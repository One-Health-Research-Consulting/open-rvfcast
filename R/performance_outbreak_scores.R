#' Outbreak-focused (rather than forecast-focused) evaluation of hex predictions
#'
#' Forecast-focused scores treat every (base date, hex, window) row as a unit, so an outbreak
#' counts once for every base date whose window happened to contain it (1-4 rows per window,
#' depending on how the random base dates fell). Here the unit is the hex outbreak episode, 
#' and forecasts are indexed by how many days before the episode's first outbreak they were made. 
#'
#' Alerts are rank-based: a hex is "flagged" on a base date and window when its predicted
#' probability is in the top `alert_fraction` of all hexes for that base date and window.
#' The idea here is that we can manipulate the share of hex-windows under alert (the alert burden)
#' and evaluate things like which outbreaks would be missed.
#'
#' @title evaluate_outbreak_skill
#' @param linked Output of link_predictions_to_outbreaks() for one evaluation set
#' @param eval_set Label for the evaluation set
#' @param alert_fractions Alert fractions (share of hexes flagged per base date and window)
#' @param warn_leads Lead times (days) at which the share of warned episodes is summarized
#' @param lead_bin_days Width of the lead-time bins for the rank curves
#' @param n_boot Bootstrap resamples of episodes for confidence intervals
#' @param seed Random seed for the bootstrap
#' @return List of tibbles: episodes, episode_weighted_scores, lead_curve, warning_curve,
#'   warning_summary, missed_episodes, warning_by_size
#' @author Morgan Kain
#' @export
evaluate_outbreak_skill <- function(
    linked
  , eval_set
  , alert_fractions = c(0.01, 0.05, 0.10)
  , warn_leads      = c(7, 30, 60, 90)
  , lead_bin_days   = 15
  , n_boot          = 1000
  , seed            = 2026
) {

  set.seed(seed)

  ## Outbreak-forecast pairs for outbreaks that are used in evaluation
   ## can change this, but some outbreaks have 0 cases or NA cases, which make
   ## me less confident in them. "counts_toward_label" gets a 1 when cases is not NA or 0
   ## (why cases would be 0 is something to look into in the WOAH WAHIS reports)
  events <- linked$prediction_events |> dplyr::filter(counts_toward_label == 1)

  ## Episode table: first outbreak, type, size and lead-time coverage
  episodes <- summarize_episodes(events, linked$outbreaks)

  ## Forecasts targeting each episode's first outbreak, indexed by lead time
  first_events <- events |>
    dplyr::semi_join(episodes, by = c("hex_episode", "outbreak_id" = "first_outbreak_id"))

  ## Longest lead at which each episode was flagged, for every alert fraction
  warned <- purrr::map_dfr(alert_fractions, function(a) {
    first_events |>
      dplyr::summarize(
        warned_lead = suppressWarnings(max(days_before_start[prob_rank_pct >= 1 - a]))
      , .by = hex_episode) |>
      dplyr::mutate(
        warned_lead    = ifelse(is.finite(warned_lead), warned_lead, NA_real_)
      , alert_fraction = a)
  })

  ## Join first events into episodes
  episodes <- episodes |>
    dplyr::left_join(
      first_events |>
        dplyr::summarize(
          best_rank_pct   = max(prob_rank_pct)
        , best_rank_lead  = days_before_start[which.max(prob_rank_pct)]
        , median_rank_pct = stats::median(prob_rank_pct)
        , .by             = hex_episode)
    , by = "hex_episode") |>
    dplyr::left_join(
      warned |>
        tidyr::pivot_wider(names_from = alert_fraction, values_from = warned_lead, names_prefix = "warned_lead_top_")
    , by = "hex_episode") |>
    dplyr::mutate(eval_set = eval_set, .before = 1)

  ## Episodes with forecasts available at every lead time (for lead-time summaries)
  covered <- episodes |> dplyr::filter(full_lead_coverage)

  list(
    episodes                = episodes
  , episode_weighted_scores = episode_weighted_scores(linked, events, n_boot) |> dplyr::mutate(eval_set = eval_set, .before = 1)
  , lead_curve              = lead_rank_curve(first_events, covered, lead_bin_days) |> dplyr::mutate(eval_set = eval_set, .before = 1)
  , warning_curve           = warning_curves(warned, covered) |> dplyr::mutate(eval_set = eval_set, .before = 1)
  , warning_summary         = warning_summary(warned, covered, warn_leads, n_boot) |> dplyr::mutate(eval_set = eval_set, .before = 1)
  , missed_episodes         = episodes |>
      dplyr::filter(dplyr::if_all(dplyr::starts_with("warned_lead_top_"), is.na)) |>
      dplyr::arrange(dplyr::desc(n_cases))
  , warning_by_size         = warning_by_size(warned, covered) |> dplyr::mutate(eval_set = eval_set, .before = 1)
  )

}

#' Figures for evaluate_outbreak_skill() output
#'
#' @title plot_outbreak_skill
#' @param outbreak_skill Output of evaluate_outbreak_skill()
#' @return Named list of ggplots: lead_curve, lead_curve_by_type, warning_curve,
#'   warning_curve_by_type, episode_weighted_auc
#' @author Morgan Kain
#' @export
plot_outbreak_skill <- function(outbreak_skill) {

  plots <- list()

  ## Rank of outbreak hexes among all hexes as the outbreak approaches
  lead_plot <- function(d, by_type) {
    g <- d |>
      ggplot2::ggplot(ggplot2::aes(lead_bin_mid, median_rank_pct)) +
#      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = q25_rank_pct, ymax = q75_rank_pct)) +
      ggplot2::scale_x_reverse() +
      ggplot2::labs(x = "Days before outbreak start", y = "Percentile among all hexes (1 = highest risk)"
                    , title = "How outbreak hexes ranked as the outbreak approached"
                    , subtitle = "Median and IQR across episodes; dashed = an average hex") +
      ggplot2::theme_bw()
    if (by_type) g + ggplot2::facet_wrap(~episode_type) else g
  }
  plots$lead_curve         <- lead_plot(outbreak_skill$lead_curve |> dplyr::filter(episode_type == "All episodes"), FALSE)
  plots$lead_curve_by_type <- lead_plot(outbreak_skill$lead_curve |> dplyr::filter(episode_type != "All episodes"), TRUE)

  ## Share of episodes warned at least L days ahead
  warning_plot <- function(d, by_type) {
    g <- d |>
      ggplot2::ggplot(ggplot2::aes(lead_days, share_warned, colour = factor(alert_fraction))) +
      ggplot2::geom_step() +
      ggplot2::scale_x_reverse() +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(x = "Warned at least this many days before the outbreak", y = "Share of outbreak episodes"
                    , colour = "Alert fraction\n(top share of\nhexes flagged)"
                    , title = "Earliest warning of outbreak episodes") +
      ggplot2::theme_bw()
    if (by_type) g + ggplot2::facet_wrap(~episode_type) else g
  }
  
  plots$warning_curve         <- warning_plot(outbreak_skill$warning_curve |> dplyr::filter(episode_type == "All episodes"), FALSE)
  plots$warning_curve_by_type <- warning_plot(outbreak_skill$warning_curve |> dplyr::filter(episode_type != "All episodes"), TRUE)

  ## Row-weighted vs episode-weighted ROC-AUC by window
  plots$episode_weighted_auc <- outbreak_skill$episode_weighted_scores |>
    ggplot2::ggplot(ggplot2::aes(forecast_interval)) +
    ggplot2::geom_point(ggplot2::aes(y = roc_auc_rows, colour = "Each row counts once")) +
    ggplot2::geom_pointrange(ggplot2::aes(y = roc_auc_episode, ymin = roc_auc_episode_lo, ymax = roc_auc_episode_hi
                                          , colour = "Each episode counts once")) +
    ggplot2::labs(x = "Forecast interval (days)", y = "ROC-AUC", colour = NULL
                  , title = "ROC-AUC with rows vs episodes as the unit (95% CI over episodes)") +
    ggplot2::theme_bw()

  plots

}

## One row per hex episode with its first labelled outbreak, type, size and coverage
summarize_episodes <- function(events, outbreaks) {

  ## Episode type from all outbreaks in the episode (index flags mark the start of a chain
   ## or the first outbreak of a chain in a new country)
  episode_flags <- outbreaks |>
    dplyr::filter(counts_toward_label == 1) |>
    dplyr::summarize(
      first_start_date  = min(start_date)
    , first_outbreak_id = outbreak_id[which.min(start_date)]
    , n_outbreaks       = dplyr::n()
    , n_cases           = sum(cases, na.rm = TRUE)
    , country           = country_iso3c[which.min(start_date)]
    , shapeName         = shapeName[1]
    , has_chain_index   = any(index_case == 1, na.rm = TRUE)
    , has_country_index = any(country_index_case == 1, na.rm = TRUE)
    , full_lead_coverage = full_lead_coverage[which.min(start_date)]
    , .by = hex_episode) |>
    dplyr::mutate(episode_type = dplyr::case_when(
      has_chain_index   ~ "First outbreak of a chain"
    , has_country_index ~ "First outbreak in a new country"
    , TRUE              ~ "Follow-on outbreak"))

  ## Keep episodes that some forecast targeted
  episode_flags |>
    dplyr::semi_join(events, by = "hex_episode") |>
    dplyr::left_join(
      events |> dplyr::summarize(n_forecasts = dplyr::n(), .by = hex_episode)
    , by = "hex_episode")

}

## Row-weighted and episode-weighted ROC-AUC / average precision by window
episode_weighted_scores <- function(linked, events, n_boot) {

  ## Each positive row belongs to the episode of the first outbreak in its window
  row_keys <- base::intersect(c("outer_fold_id", "date", "shapeName", "forecast_interval"), names(linked$predictions))
  row_episode <- events |>
    dplyr::slice_min(start_date, n = 1, with_ties = FALSE, by = dplyr::all_of(row_keys)) |>
    dplyr::select(dplyr::all_of(row_keys), hex_episode)

  linked$predictions |>
    dplyr::left_join(row_episode, by = row_keys) |>
    dplyr::group_by(forecast_interval) |>
    dplyr::group_modify(function(d, k) {

      pos <- d |> dplyr::filter(true_out == 1, !is.na(hex_episode))
      neg <- sort(d$prob_pred[d$true_out == 0])

      ## Share of negatives each positive outranks (ties count half)
      outranks <- (findInterval(pos$prob_pred, neg, left.open = TRUE) +
                     findInterval(pos$prob_pred, neg)) / (2 * length(neg))

      ## Weight 1 / (rows of the same episode) so each episode counts once
      w <- 1 / ave(rep(1, nrow(pos)), pos$hex_episode, FUN = length)

      ## Bootstrap episodes (negatives fixed) for the episode-weighted AUC
      ep_ids   <- unique(pos$hex_episode)
      ep_rows  <- split(seq_len(nrow(pos)), pos$hex_episode)
      boot_auc <- vapply(seq_len(n_boot), function(b) {
        idx <- unlist(ep_rows[sample(ep_ids, replace = TRUE)], use.names = FALSE)
        sum(w[idx] * outranks[idx]) / sum(w[idx])
      }, numeric(1))

      tibble::tibble(
        n_positive_rows    = nrow(pos)
      , n_episodes         = length(ep_ids)
      , roc_auc_rows       = mean(outranks)
      , roc_auc_episode    = sum(w * outranks) / sum(w)
      , roc_auc_episode_lo = stats::quantile(boot_auc, 0.025)
      , roc_auc_episode_hi = stats::quantile(boot_auc, 0.975)
      , avg_precision_rows    = weighted_average_precision(pos$prob_pred, rep(1, nrow(pos)), neg)
      , avg_precision_episode = weighted_average_precision(pos$prob_pred, w, neg)
      , prevalence_rows       = mean(d$true_out)
      )

    }) |>
    dplyr::ungroup()

}

## Average precision with weighted positives and unit-weight negatives
weighted_average_precision <- function(p_pos, w_pos, neg_sorted) {

  o     <- order(p_pos, decreasing = TRUE)
  p_pos <- p_pos[o]; w_pos <- w_pos[o]
  
  ## Weighted true positives and (unweighted) false positives at each positive's threshold
  tp <- vapply(p_pos, function(p) sum(w_pos[p_pos >= p]), numeric(1))
  fp <- length(neg_sorted) - findInterval(p_pos, neg_sorted, left.open = TRUE)
  sum(w_pos * tp / (tp + fp)) / sum(w_pos)

}

## Median and IQR of outbreak-hex rank by lead-time bin, overall and by episode type
lead_rank_curve <- function(first_events, covered, lead_bin_days) {

  breaks <- seq(0, ceiling(max(first_events$days_before_start) / lead_bin_days) * lead_bin_days, by = lead_bin_days)

  ## One value per episode per bin, so each episode counts once
  per_episode <- first_events |>
    dplyr::inner_join(covered |> dplyr::select(hex_episode, episode_type), by = "hex_episode") |>
    dplyr::mutate(lead_bin = cut(days_before_start, breaks)) |>
    dplyr::summarize(rank_pct = stats::median(prob_rank_pct), .by = c(hex_episode, episode_type, lead_bin))

  dplyr::bind_rows(per_episode |> dplyr::mutate(episode_type = "All episodes"), per_episode) |>
    dplyr::summarize(
      median_rank_pct = stats::median(rank_pct)
    , q25_rank_pct    = stats::quantile(rank_pct, 0.25)
    , q75_rank_pct    = stats::quantile(rank_pct, 0.75)
    , n_episodes      = dplyr::n()
    , .by = c(episode_type, lead_bin)) |>
    dplyr::mutate(lead_bin_mid = (as.numeric(lead_bin) - 0.5) * lead_bin_days) |>
    dplyr::arrange(episode_type, lead_bin)

}

## Share of covered episodes warned at least L days ahead, for every L and alert fraction
warning_curves <- function(warned, covered) {

  w <- warned |> dplyr::inner_join(covered |> dplyr::select(hex_episode, episode_type), by = "hex_episode")
  leads <- seq_len(max(c(1, w$warned_lead), na.rm = TRUE))

  dplyr::bind_rows(w |> dplyr::mutate(episode_type = "All episodes"), w) |>
    dplyr::group_by(episode_type, alert_fraction) |>
    dplyr::group_modify(function(d, k) {
      tibble::tibble(
        lead_days    = leads
      , share_warned = vapply(leads, function(L) mean(!is.na(d$warned_lead) & d$warned_lead >= L), numeric(1))
      , n_episodes   = nrow(d))
    }) |>
    dplyr::ungroup()

}

## Share warned at key lead times with bootstrap CIs over episodes
warning_summary <- function(warned, covered, warn_leads, n_boot) {

  w <- warned |> dplyr::inner_join(covered |> dplyr::select(hex_episode, episode_type), by = "hex_episode")

  dplyr::bind_rows(w |> dplyr::mutate(episode_type = "All episodes"), w) |>
    tidyr::expand_grid(lead_days = warn_leads) |>
    dplyr::group_by(episode_type, alert_fraction, lead_days) |>
    dplyr::group_modify(function(d, k) {
      hit  <- !is.na(d$warned_lead) & d$warned_lead >= k$lead_days
      boot <- vapply(seq_len(n_boot), function(b) mean(sample(hit, replace = TRUE)), numeric(1))
      tibble::tibble(
        n_episodes   = length(hit)
      , share_warned = mean(hit)
      , lo_2.5       = stats::quantile(boot, 0.025)
      , hi_97.5      = stats::quantile(boot, 0.975))
    }) |>
    dplyr::ungroup()

}

## Share warned at least 30 days ahead by outbreak size (cases)
warning_by_size <- function(warned, covered) {

  warned |>
    dplyr::inner_join(covered |> dplyr::select(hex_episode, n_cases), by = "hex_episode") |>
    dplyr::mutate(size_class = cut(n_cases, c(0, 1, 10, 100, Inf), labels = c("1 case", "2-10 cases", "11-100 cases", "> 100 cases"))) |>
    dplyr::summarize(
      n_episodes          = dplyr::n()
    , share_warned_any    = mean(!is.na(warned_lead))
    , share_warned_30days = mean(!is.na(warned_lead) & warned_lead >= 30)
    , .by = c(alert_fraction, size_class)) |>
    dplyr::arrange(alert_fraction, size_class)

}
