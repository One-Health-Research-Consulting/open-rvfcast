#' Write the comprehensive performance report (markdown and html)
#'
#' Builds report.md from the tables and figures produced by run_comprehensive_performance(),
#' with a short explanation of what each section measures, and renders it to report.html with
#' commonmark (no pandoc needed). Figures are linked by relative path, so the report should be
#' opened from inside the output folder.
#'
#' @title write_performance_report
#' @param results The results list built inside run_comprehensive_performance()
#' @param out_dir The output folder
#' @return Path to report.md
#' @author Morgan Kain
#' @export
write_performance_report <- function(results, out_dir) {

  tb  <- results$tables
  ff  <- results$figure_files
  md  <- character(0)
  add <- function(...) md <<- c(md, ...)

  ## Embed a saved figure by its section__name key, if it exists
  fig <- function(key, caption = "") {
    path <- ff[[key]]
    if (is.null(path) || !grepl("\\.png$", path)) return(invisible(NULL))
    add(sprintf("![%s](%s)", caption, file.path("figures", basename(path))), "")
  }

  add("# RVF outbreak forecast performance", ""
    , sprintf("Run date: %s. Predictions: `%s`.", results$meta$run_date, results$meta$predictions), ""
    , "Two evaluation sets are scored when available. **test** is the held-out test period, predicted by the final model. **outer_folds** is the calibration harvest: each outer fold's refit predicting its own held-out window before 2021, calibrated with coefficients fit on the other folds. The outer-fold set adds many more outbreak seasons; the reference model is only scored on the test set because it is built from the training period.", ""
    , report_md_table(results$meta$eval_sets), "")

  ## Label linking check
  lc <- results$meta$label_checks
  n_disagree <- sum(lc$n[lc$true_out != lc$label_from_outbreaks])
  add(sprintf("Outbreak linking reproduced the pipeline's labels on all but %s prediction rows.", format(n_disagree, big.mark = ",")), "")

  if (length(results$errors)) {
    add("**Sections that failed:**", "", paste0("- ", names(results$errors), ": ", unlist(results$errors)), "")
  }

  #### Headline -------------------------------------------------------------------------
  add("## Headline (test set)", "")
  headline <- report_headline(tb)
  if (!is.null(headline)) {
    add("One row per forecast window. Information gain per event is the log-likelihood improvement over the comparison forecast divided by the number of outbreak rows, in nats; its exponential (probability gain) is how many times more probability the model put on the outbreaks that happened. Recall at 1% / 5% is the share of outbreak rows inside the top 1% / 5% of hexes. Episode AUC counts each outbreak episode once.", ""
      , report_md_table(headline), "")
  }

  #### Hex-level forecast scores -------------------------------------------------------------
  if (!is.null(tb$hex)) {
    add("## Hex-level forecast scores", ""
      , "Every base date x hex x window row is a unit. ROC-AUC and PR-AUC measure ranking (judge PR-AUC against the outbreak prevalence, not 0.5). Brier skill compares with a constant base-rate forecast. Observed / expected near 1 means the overall level of the probabilities is right.", ""
      , report_md_table(tb$hex$scores |> dplyr::select(eval_set, forecast_interval, n, n_pos, prevalence, roc_auc, pr_auc, logloss, brier_skill, obs_over_exp)), "")
    fig("hex__scores_by_horizon", "Scores by window")
    add("Rank-based alerts flag the top share of hexes on each base date and window, so the alert burden equals the alert fraction.", ""
      , report_md_table(tb$hex$alert_table), "")
    fig("hex__alert_recall", "Recall of rank-based alerts")
    fig("hex__calibration_opt", "Calibration (optimised bins)")
    fig("hex__calibration_even", "Calibration (even bins)")
    fig("hex__prob_density", "Predicted probability densities")
    fig("hex__summary_probs", "Predicted probabilities by base date")
    ## Masked hex maps: the first forecast window for each mapped base date (all windows are in figures/)
    if (!is.null(tb$context$hex_context)) {
      add("### Hex forecasts masked by forecast quality", ""
        , "Test-period hex probabilities (log colour scale). Forecast quality is judged on the outer folds where a hex has outer-fold rows, otherwise on the test period: hexes with no outbreaks in that evaluation period are faded (skill cannot be judged), hatched hexes have outbreaks but no log-loss gain over a flat forecast, and full-colour unhatched hexes show demonstrated skill. Outline width shows outbreaks before the test period; a dashed red outline marks a hex with an outbreak in the window. Maps for every window are in figures/.", ""
        , report_md_table(tb$context$hex_context |> dplyr::count(quality_class, history_class) |> tidyr::pivot_wider(names_from = history_class, values_from = n, values_fill = 0)), "")
      for (k in grep("^hex__masked_maps__.*__30$", names(ff), value = TRUE)) fig(k, k)
    }
  }

  #### Rare-event verification ---------------------------------------------------------------
  if (!is.null(tb$rare_event)) {
    re <- tb$rare_event
    add("## Rare-event verification", ""
      , "Scores designed for very rare events, comparing the model with climatology (the constant event rate) and with the reference forecast (each hex's training-period outbreak rate for that calendar month and window).", "")
    add("**Information gain.** Positive values favour the model. p_value_T and p_value_W are the Rhoades et al. (2011) T- and W-tests.", ""
      , report_md_table(re$information_gain |> dplyr::select(eval_set, forecast_interval, model, versus, IG_nats_per_event, prob_gain_per_event, p_value_T, p_value_W)), "")
    add("**Calibration in the large (N-test).** ratio_obs_exp near 1 and a z-score within about +/-2 mean the total number of outbreaks matches what the probabilities implied.", ""
      , report_md_table(re$n_test |> dplyr::select(eval_set, forecast_interval, model, observed_events, expected_events, ratio_obs_exp, z_poisson_binomial)), "")
    add("**CORP decomposition.** Mean score = miscalibration - discrimination + uncertainty. Lower miscalibration and higher discrimination are better.", ""
      , report_md_table(re$corp_decomposition |> dplyr::filter(score == "log") |> dplyr::select(eval_set, forecast_interval, model, mean_score, MCB_miscalibration, DSC_discrimination)), "")
    fig("rare_event__corp", "CORP reliability")
    add("**Molchan diagram and area skill.** ASS_tau_0.05 is the area under the hit-rate curve when up to 5% of space-time is alarmed (random = 0.025). The reference-weighted version asks whether the model ranks better than the reference.", ""
      , report_md_table(re$molchan |> dplyr::select(eval_set, forecast_interval, model, ASS_full, ASS_tau_0.05, recall_at_1pct, recall_at_5pct, ASS_full_ref_weighted)), "")
    fig("rare_event__molchan_zoom", "Molchan diagram (zoomed)")
    add("**Decision value.** Relative economic value is the share of the possible saving (vs acting on climatology) achieved across cost/loss ratios; the Murphy diagram checks whether the model is better at every decision threshold.", ""
      , report_md_table(re$economic_value |> dplyr::select(eval_set, forecast_interval, model, peak_V_envelope, alpha_at_peak, alpha_min_useful, alpha_max_useful, peak_V_face_value)), ""
      , report_md_table(re$murphy_dominance), "")
    fig("rare_event__economic_value", "Relative economic value")
    fig("rare_event__murphy", "Murphy diagram")
    if (!is.null(re$bootstrap_ci) && nrow(re$bootstrap_ci)) {
      add("**Space-time block-bootstrap 95% intervals** (5-degree tiles x calendar years).", "", report_md_table(re$bootstrap_ci), "")
    }
  }

  #### Outbreak-focused skill -----------------------------------------------------------------
  if (!is.null(tb$outbreak)) {
    ob <- tb$outbreak
    add("## Outbreak-focused skill", ""
      , "Here the unit is the hex outbreak episode (outbreak starts in the same hex less than 31 days apart), and forecasts are indexed by how many days before the episode's first outbreak they were made. A forecast flags a hex when it is in the top share of hexes for that base date and window. Lead-time summaries use episodes with forecasts available at every lead time.", "")
    add(report_md_table(ob$episodes |> dplyr::count(eval_set, episode_type, full_lead_coverage, name = "n_episodes")), "")
    add("**Rows vs episodes as the unit.**", "", report_md_table(ob$episode_weighted_scores), "")
    fig("outbreak__episode_weighted_auc", "Episode-weighted AUC")
    add("**Earliest warning.** Share of episodes flagged at least this many days before the outbreak, with bootstrap 95% intervals over episodes.", ""
      , report_md_table(ob$warning_summary |> dplyr::filter(episode_type == "All episodes") |> dplyr::select(-episode_type)), "")
    fig("outbreak__warning_curve", "Earliest warning")
    fig("outbreak__warning_curve_by_type", "Earliest warning by episode type")
    add("**First outbreaks vs follow-on outbreaks.** Follow-on outbreaks are partly predictable from nearby recent outbreaks; first outbreaks of a chain or in a new country are the main target of early warning.", ""
      , report_md_table(ob$warning_summary |> dplyr::filter(episode_type != "All episodes", lead_days == 30)), "")
    fig("outbreak__lead_curve", "Rank of outbreak hexes as the outbreak approached")
    fig("outbreak__lead_curve_by_type", "Rank by episode type")
    add("**Warning by outbreak size** (cases).", "", report_md_table(ob$warning_by_size), "")
    if (nrow(ob$missed_episodes)) {
      add("**Episodes never flagged at any alert fraction** (largest first).", ""
        , report_md_table(ob$missed_episodes |> dplyr::select(eval_set, hex_episode, country, first_start_date, n_outbreaks, n_cases, episode_type, best_rank_pct) |> utils::head(20)), "")
    }
  }

  #### Regional up-scaling --------------------------------------------------------------------
  if (!is.null(tb$regional)) {
    rg <- tb$regional
    add("## Regional up-scaling", ""
      , "Hex probabilities are combined into the probability of at least one outbreak in each region. The copula keeps every hex probability and adds one latent correlation (rho) per window, fitted on regional outcomes with leave-one-year-out cross-validation; rho = 0 is independence, rho near 1 is the largest hex probability. All scores below are out of sample.", ""
      , "**Fitted parameters (all years).**", "", report_md_table(rg$params_full), "")
    add("**Scores by method (test set).** obs_over_exp near 1 means the regional probabilities have the right overall level.", ""
      , report_md_table(rg$scores |> dplyr::filter(eval_set == "test") |> dplyr::select(method, forecast_interval, n, n_pos, roc_auc, pr_auc, logloss, obs_over_exp)), "")
    add("**Rare-event scores by method (test set).**", ""
      , report_md_table(rg$rare_event_scores |> dplyr::filter(eval_set == "test") |> dplyr::select(method, forecast_interval, info_gain_nats_per_event, prob_gain_per_event, ratio_obs_exp, ASS_tau_0.05)), "")
    add("**Observed / expected by region size (test set).**", ""
      , report_md_table(rg$scores_by_size |> dplyr::filter(eval_set == "test") |> dplyr::select(method, size_class, n_pos, expected_events, obs_over_exp)), "")
    fig("regional__reliability", "Regional reliability")
    fig("regional__obs_exp_by_size", "Observed / expected by region size")
    fig("regional__rho_by_year", "Fitted rho by held-out year")
    ## Masked regional maps: the first forecast window for each mapped base date (all windows,
     ## and the unmasked maps, are in figures/)
    if (!is.null(rg$region_context)) {
      add("### Regional forecasts masked by forecast quality", ""
        , "Out-of-fold copula probabilities of at least one outbreak per aggregated ADM2 region (log colour scale). Hatching marks regions with outbreaks but no log-loss gain over a flat forecast; stippling marks regions with no outbreaks in their evaluation period. Outline width shows outbreaks before the test period, red dots are outbreaks in the window (area proportional to count), and dashed red outlines mark the 10 regions with the highest probability.", ""
        , report_md_table(rg$region_context |> dplyr::count(quality_class, history_class) |> tidyr::pivot_wider(names_from = history_class, values_from = n, values_fill = 0)), "")
      for (k in grep("^regional__masked_maps__.*__30$", names(ff), value = TRUE)) fig(k, k)
    }
  }

  #### Any outbreak within the whole forecast horizon ------------------------------------------
  if (!is.null(tb$cumulative)) {
    add("## Any outbreak within the forecast horizon (days 1-149)", ""
      , "Probability of at least one outbreak in the whole horizon, combining the five window forecasts: independent windows (1 - prod(1 - p), an upper bound when outbreaks in consecutive windows go together) and the largest window probability (a lower bound). Regions combine their out-of-fold copula probabilities. obs_over_exp above 1 under the upper bound, or below 1 under the lower bound, means the truth lies outside the bounds.", ""
      , report_md_table(tb$cumulative$scores |> dplyr::select(unit, eval_set, method, n, n_pos, prevalence, roc_auc, pr_auc, logloss, obs_over_exp, info_gain_nats_per_event, ASS_tau_0.05)), ""
      , "**Rank-based alerts** (top share of hexes / regions on each base date).", ""
      , report_md_table(tb$cumulative$alert_table |> dplyr::filter(eval_set == "test")), "")
    fig("cumulative__reliability", "Reliability of whole-horizon forecasts")
  }

  #### Where and when ----------------------------------------------------------------------------
  if (!is.null(tb$spatial)) {
    add("## Where the model does well (test set)", ""
      , report_md_table(tb$spatial$country_perf |> dplyr::filter(n_pos > 0) |> dplyr::select(Country, country_name, n, n_pos, prevalence, roc_auc, pr_auc, logloss, brier_skill, average_p)), "")
    fig("spatial__hex_logloss", "Hex log loss")
    fig("spatial__country_ranking", "Country ranking")
  }
  if (!is.null(tb$enso) && !is.null(tb$enso$enso_phase_perf)) {
    add("## ENSO phase (test set)", "", report_md_table(tb$enso$enso_phase_perf |> dplyr::select(enso_phase, enso_rvf_sensitivity, n, n_pos, roc_auc, logloss, brier_skill)), "")
    fig("enso__enso_perf", "Performance by ENSO phase")
  }
  if (!is.null(tb$temporal)) {
    add("## Over time (test set)", "")
    if (!is.null(tb$temporal$cycle_summary) && nrow(tb$temporal$cycle_summary)) add(report_md_table(tb$temporal$cycle_summary), "")
    fig("temporal__rolling_perf", "Rolling quarterly performance")
    fig("temporal__seasonal_cycle", "Seasonal cycle")
  }
  if (!is.null(tb$model_structure)) {
    add("## What drives the predictions", "", report_md_table(tb$model_structure$shap_feature_order), "")
    fig("model_structure__shap_summary", "SHAP importance")
    fig("model_structure__shap_beeswarm", "SHAP beeswarm")
  }
  if (!is.null(tb$variable_importance)) {
    add("## Variable importance across outer-fold models", ""
      , "xgboost gain per feature, averaged over all outer-fold models (zero gain where a model never split on the feature); n_folds_top10 counts the models with the feature in their top 10.", ""
      , report_md_table(tb$variable_importance$vi_summary |> dplyr::slice_head(n = 20)), "")
    fig("variable_importance__vi_gain", "Gain importance by outer-fold model")
    fig("variable_importance__pdp", "Partial dependence by outer-fold model")
  }

  #### Hex context ------------------------------------------------------------------------------
  if (!is.null(tb$context$hex_context)) {
    hc <- tb$context$hex_context
    add("## Hex context", ""
      , "Per-hex context (tables/context__hex_context.csv), also used to mask the hex maps: mask skill and quality class (outer folds first), outbreak history before the test period, typical test-period predicted level, and test-period skill relative to the hex's own historical rate (logloss_gain_vs_ref > 0 means the model beat the reference in that hex). The test predictions with per-row context (ratio to the hex's background level and to the reference) are in data/test_predictions_with_context.qs.", ""
      , report_md_table(hc |>
          dplyr::summarize(
            n_hexes = dplyr::n(), n_with_test_outbreaks = sum(n_test_outbreak_rows > 0, na.rm = TRUE)
          , median_prob = stats::median(median_prob, na.rm = TRUE)
          , mean_logloss_gain_vs_ref = mean(logloss_gain_vs_ref, na.rm = TRUE)
          , median_within_hex_auc = stats::median(within_hex_auc, na.rm = TRUE)
          , .by = history_class) |>
          dplyr::arrange(history_class)), "")
  }

  report_md <- file.path(out_dir, "report.md")
  writeLines(md, report_md)

  ## Render to html without pandoc
  if (requireNamespace("commonmark", quietly = TRUE)) {
    body <- commonmark::markdown_html(paste(md, collapse = "\n"), extensions = TRUE)
    css  <- "<style>body{font-family:sans-serif;max-width:1100px;margin:2em auto;padding:0 1em;line-height:1.4}table{border-collapse:collapse;font-size:12px;margin:0.5em 0}th,td{border:1px solid #ccc;padding:3px 6px;text-align:right}th{background:#f3f3f3}img{max-width:100%}</style>"
    writeLines(c("<!DOCTYPE html><html><head><meta charset='utf-8'><title>RVF forecast performance</title>", css, "</head><body>", body, "</body></html>")
               , file.path(out_dir, "report.html"))
  }

  report_md

}


## Headline table for the test set, one row per window, from whichever sections ran
report_headline <- function(tb) {

  parts <- list()

  if (!is.null(tb$hex)) {
    parts$hex <- tb$hex$scores |> dplyr::filter(eval_set == "test") |>
      dplyr::select(forecast_interval, n_outbreak_rows = n_pos, roc_auc, pr_auc, prevalence, obs_over_exp)
    parts$alerts <- tb$hex$alert_table |> dplyr::filter(eval_set == "test", alert_fraction %in% c(0.01, 0.05)) |>
      dplyr::select(forecast_interval, alert_fraction, recall) |>
      tidyr::pivot_wider(names_from = alert_fraction, values_from = recall, names_prefix = "recall_top_")
  }
  if (!is.null(tb$rare_event)) {
    parts$ig <- tb$rare_event$information_gain |> dplyr::filter(eval_set == "test", model == "xgboost") |>
      dplyr::select(forecast_interval, versus, IG_nats_per_event) |>
      tidyr::pivot_wider(names_from = versus, values_from = IG_nats_per_event, names_prefix = "IG_per_event_vs_")
  }
  if (!is.null(tb$outbreak)) {
    parts$ep <- tb$outbreak$episode_weighted_scores |> dplyr::filter(eval_set == "test") |>
      dplyr::select(forecast_interval, n_episodes, episode_auc = roc_auc_episode)
  }
  if (!length(parts)) return(NULL)

  purrr::reduce(parts, dplyr::left_join, by = "forecast_interval")

}

## Markdown table with numbers rounded to 3 significant digits
report_md_table <- function(df, max_rows = 60) {

  if (is.null(df) || !nrow(df)) return("_(no rows)_")
  df <- utils::head(as.data.frame(df), max_rows)
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "", formatC(signif(x, 3), format = "fg", digits = 3, big.mark = ",")) else as.character(x)
  })
  c(
    paste0("| ", paste(names(df), collapse = " | "), " |")
  , paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  , apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  )

}
