#' Spatial, climate-phase, temporal and model-structure diagnostics of RVF predictions
#'
#' Section functions called by run_comprehensive_performance() (formerly the body of
#' run_comprehensive_fit_diagnostics()):
#'  - diagnose_spatial_performance(): hex-level and country-level performance, including the
#'    positive/negative predicted-probability comparison by country and macro-region rollups
#'  - diagnose_enso_performance(): ENSO / climate-phase coupling, using the NOAA ONI index, a
#'    data-driven country ENSO-impact ranking and a literature-based lookup table
#'  - diagnose_temporal_performance(): seasonal (day-of-year) cycles, STL decomposition with
#'    dominant cycle length, and rolling performance over time
#'  - diagnose_model_structure(): xgboost gain importance, vip, SHAP (summary, beeswarm,
#'    dependence), partial dependence, and tree-structure visuals
#'  - diagnose_variable_importance(): gain importance and partial dependence across all
#'    outer-fold models (the variable_importance target)
#' Each returns a list with `tables` (tibbles) and `plots` (ggplots or other figure objects).
#'
#' @author Morgan Kain
#' @export
diagnose_spatial_performance <- function(hex_preds, region_hexes, shape_to_country, min_events_for_auc = 5) {

  ## region_hexes target wraps the sf object in a list
  hex_fine <- if (inherits(region_hexes, "sf")) region_hexes else region_hexes[[1]]

  africa_countries <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf") |>
    dplyr::select(iso_a3, geometry) |>
    dplyr::filter(iso_a3 != "-99")

  enso_sensitivity <- enso_sensitivity_lookup()

  tables <- list()
  plots  <- list()

  ## Fine hex-level: mean log loss and mean predicted probability per hex, pooled
  ## across the whole record. Log loss used here given that with ~0.1% prevalence
  ## spread over 2479 hexes, few hexes have any outbreaks (or more than 1)
  tables$hex_perf <- hex_preds |>
    dplyr::group_by(shapeName) |>
    dplyr::summarize(
        n            = dplyr::n()
      , n_pos        = sum(true_out == 1)
      , mean_logloss = mean(-(true_out * log(pmax(prob_pred, 1e-15)) + (1 - true_out) * log(pmax(1 - prob_pred, 1e-15))))
      , mean_prob    = mean(prob_pred)
      , .groups = "drop")

  plots$hex_logloss <- hex_fine |>
    dplyr::left_join(tables$hex_perf, by = "shapeName") |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = mean_logloss), colour = NA) +
    ggplot2::scale_fill_viridis_c(name = "Mean log loss", direction = -1, na.value = "grey90") +
    ggplot2::coord_sf() + ggplot2::theme_void() +
    ggplot2::labs(title = "Per-hex mean log loss (lower = better)")

  plots$hex_outbreaks <- hex_fine |>
    dplyr::left_join(tables$hex_perf, by = "shapeName") |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = n_pos), colour = NA) +
    ggplot2::scale_fill_viridis_c(name = "True outbreaks\n(count)", trans = "sqrt", na.value = "grey90") +
    ggplot2::coord_sf() + ggplot2::theme_void() +
    ggplot2::labs(title = "Where the true outbreaks actually are")

  ## Country-level ranking: join Country onto the fine hex-level predictions
  ## and score per country
  preds_country <- hex_preds |>
    dplyr::left_join(shape_to_country, by = "shapeName") |>
    dplyr::filter(!is.na(Country))

  ## Score one country subset and attach the literature ENSO labels (country name falls
  ## back to the ISO3 code for countries not in the lookup)
  score_by_country <- function(d) {
    d |>
      dplyr::group_by(Country) |>
      dplyr::group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
      dplyr::ungroup() |>
      dplyr::left_join(enso_sensitivity, by = "Country") |>
      dplyr::mutate(country_name = dplyr::coalesce(country_name, Country)) |>
      dplyr::arrange(dplyr::desc(n_pos))
  }

  tables$country_perf <- score_by_country(preds_country)

  plots$country_avgp_vs_logloss <- ggplot2::ggplot(tables$country_perf, ggplot2::aes(average_p, logloss, label = country_name)) +
    ggplot2::geom_point(ggplot2::aes(size = prevalence)) +
    ggplot2::xlab("Average Predicted Probability") +
    ggplot2::ylab("Log Loss") +
    ggplot2::geom_text(vjust = -0.7, size = 3) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_log10() +
    ggplot2::theme_bw()

  plots$country_prevalence_vs_logloss <- ggplot2::ggplot(tables$country_perf, ggplot2::aes(prevalence, logloss, label = country_name)) +
    ggplot2::geom_point() +
    ggplot2::xlab("Observed Outbreak Prevalence") +
    ggplot2::ylab("Log Loss") +
    ggplot2::geom_text(vjust = -0.7, size = 3) +
    ggplot2::scale_x_log10() +
    ggplot2::theme_bw()

  plots$country_map <- africa_countries |>
    dplyr::left_join(tables$country_perf, by = c("iso_a3" = "Country")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = logloss), colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(name = "Mean log loss", direction = -1, na.value = "grey90") +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Country-level mean log loss")

  plots$country_ranking <- tables$country_perf |>
    dplyr::filter(n_pos >= min_events_for_auc) |>
    ggplot2::ggplot(ggplot2::aes(x = roc_auc, y = stats::reorder(Country, roc_auc), colour = enso_rvf_sensitivity)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = "ROC-AUC", y = NULL, colour = "Literature ENSO-RVF\nsensitivity (Anyamba 2009)"
         , title = paste0("Country ranking by ROC-AUC (n_pos >= ", min_events_for_auc, " only)")) +
    ggplot2::theme_bw()

  ## Macro-region rollup: is the model systematically better/worse in the
  ## literature-flagged high-ENSO-sensitivity Horn-of-Africa/East-Africa countries?
  tables$macro_region_perf <- preds_country |>
    dplyr::left_join(enso_sensitivity, by = "Country") |>
    dplyr::filter(!is.na(macro_region)) |>
    dplyr::group_by(macro_region, enso_rvf_sensitivity) |>
    dplyr::group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(n_pos))

  ## Positive vs. negative predicted-probability country comparison
  ## scores each country separately on just its true-outbreak rows and just its
  ## true-non-outbreak rows, then compares the average predicted probability between
  ## the two
  tables$country_perf_pos <- score_by_country(preds_country |> dplyr::filter(true_out == 1))
  tables$country_perf_neg <- score_by_country(preds_country |> dplyr::filter(true_out == 0))

  tables$pos_neg_ratio <- tables$country_perf_pos |>
    dplyr::select(country_name, Country, p_pos = average_p) |>
    dplyr::left_join(
      tables$country_perf_neg |> dplyr::select(country_name, Country, p_neg = average_p)
    , by = c("country_name", "Country")) |>
    dplyr::mutate(pos_neg_ratio = p_pos / p_neg) |>
    dplyr::arrange(dplyr::desc(pos_neg_ratio))

  plots$country_pos_neg_ratio_map <- africa_countries |>
    dplyr::left_join(tables$pos_neg_ratio, by = c("iso_a3" = "Country")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = pos_neg_ratio), colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(name = "Pos/Neg Ratio", direction = -1, na.value = "grey90") +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Ratio of average predicted P: true positives vs. true negatives, by country")

  plots$country_avg_p_pos_map <- africa_countries |>
    dplyr::left_join(tables$country_perf_pos, by = c("iso_a3" = "Country")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = average_p), colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(name = "average_p", direction = -1, na.value = "grey90") +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Average predicted P on true-positive rows, by country")

  plots$country_avg_p_neg_map <- africa_countries |>
    dplyr::left_join(tables$country_perf_neg, by = c("iso_a3" = "Country")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = average_p), colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(name = "average_p", direction = -1, na.value = "grey90") +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Average predicted P on true-negative rows, by country")

  list(tables = tables, plots = plots)

}


#' @rdname diagnose_spatial_performance
#' @export
diagnose_enso_performance <- function(hex_preds, test_data, shape_to_country, country_perf
                                      , enso_data_dir, correlation_method = "spearman", min_events_for_auc = 5) {

  enso_sensitivity <- enso_sensitivity_lookup()

  tables <- list()
  plots  <- list()

  ## Data-driven country ENSO-impact ranking (temperature/precipitation anomaly
  ## correlation with ONI); complement to the literature-based enso_sensitivity_lookup()
  ## table above.
  enso_impact_result <- compute_enso_country_impact_ranking(enso_data_dir, correlation_method)

  if (!is.null(enso_impact_result)) {

    tables$enso_climate_impact_ranking <- enso_impact_result$ranking
    plots$enso_impact_ranking          <- enso_impact_result$gg_ranking
    plots$enso_impact_map              <- enso_impact_result$gg_map

    enso_impact_by_country <- enso_impact_result$ranking |>
      dplyr::rename(Country = country) |>
      dplyr::select(ENSO_impact_rank = rank, Country, combined_impact)

    plots$country_logloss_vs_enso_impact <- country_perf |>
      dplyr::left_join(enso_impact_by_country, by = "Country") |>
      ggplot2::ggplot(ggplot2::aes(x = combined_impact, y = logloss, label = country_name)) +
      ggplot2::geom_point() +
      ggplot2::scale_y_log10() +
      ggplot2::xlab("ENSO Impact") +
      ggplot2::ylab("LogLoss") +
      ggplot2::geom_text(vjust = -0.7, size = 3) +
      ggplot2::theme_bw()

    plots$country_logloss_rank_vs_enso_impact_rank <- country_perf |>
      dplyr::left_join(enso_impact_by_country, by = "Country") |>
      dplyr::arrange(dplyr::desc(logloss)) |>
      dplyr::mutate(LogLoss_rank = dplyr::row_number()) |>
      ggplot2::ggplot(ggplot2::aes(x = ENSO_impact_rank, y = LogLoss_rank, label = country_name)) +
      ggplot2::geom_point() +
      ggplot2::xlab("ENSO Impact Rank") +
      ggplot2::ylab("LogLoss Rank") +
      ggplot2::geom_text(vjust = -0.7, size = 3) +
      ggplot2::theme_bw()

  } else {

    message("Skipping the ENSO country-impact comparison plots -- see ",
            "compute_enso_country_impact_ranking() for the required data/ENSO/ files.")

  }

  oni_tbl <- fetch_oni()

  if (!is.null(oni_tbl)) {

    preds_with_enso <- hex_preds |>
      dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) |>
      dplyr::left_join(oni_tbl, by = c("year", "month")) |>
      dplyr::left_join(shape_to_country, by = "shapeName") |>
      dplyr::left_join(enso_sensitivity, by = "Country") |>
      dplyr::filter(!is.na(enso_phase))

    ## Does performance differ by ENSO phase, and does that difference concentrate in
    ## the literature-flagged high-sensitivity countries (the interaction is the
    ## actually-interesting test here, not the marginal phase effect alone)
    tables$enso_phase_perf <- preds_with_enso |>
      dplyr::group_by(enso_phase, enso_rvf_sensitivity) |>
      dplyr::group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
      dplyr::ungroup()

    plots$enso_perf <- tables$enso_phase_perf |>
      dplyr::filter(!is.na(enso_rvf_sensitivity)) |>
      ggplot2::ggplot(ggplot2::aes(enso_phase, logloss, fill = enso_phase)) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(~enso_rvf_sensitivity) +
      ggplot2::labs(x = NULL, y = "Mean log loss"
           , title = "Performance by ENSO phase x literature ENSO-RVF sensitivity"
           , subtitle = "ENSO phase from NOAA ONI (single-season >=0.5/<=-0.5 threshold, a simplification -- see fetch_oni())") +
      ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")

    ## Do the covariates that actually matter to the model shift systematically with
    ## ENSO phase? Sampled for plotting speed.
    if (!is.null(test_data)) {

      covariate_by_enso <- test_data |>
        dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) |>
        dplyr::left_join(oni_tbl, by = c("year", "month")) |>
        dplyr::filter(!is.na(enso_phase))
      covariate_by_enso <- covariate_by_enso |> dplyr::slice_sample(n = min(50000, nrow(covariate_by_enso)))

      key_covariates <- base::intersect(
        c("anomaly_scaled_precipitation_90", "anomaly_forecast_scaled_relative_humidity", "Mean_Temperature_of_Coldest_Quarter")
      , names(covariate_by_enso))

      plots$covariate_by_enso <- covariate_by_enso |>
        dplyr::select(enso_phase, dplyr::all_of(key_covariates)) |>
        tidyr::pivot_longer(-enso_phase, names_to = "covariate", values_to = "value") |>
        ggplot2::ggplot(ggplot2::aes(enso_phase, value, fill = enso_phase)) +
        ggplot2::geom_boxplot(outlier.alpha = 0.1) +
        ggplot2::facet_wrap(~covariate, scales = "free_y") +
        ggplot2::labs(x = NULL, y = NULL, title = "Key covariates by ENSO phase") +
        ggplot2::theme_bw() + ggplot2::theme(legend.position = "none")

    }

  } else {
    message("ONI-phase diagnostics skipped (no ONI data available -- see message above).")
  }

  list(tables = tables, plots = plots)

}


#' @rdname diagnose_spatial_performance
#' @export
diagnose_temporal_performance <- function(hex_preds, shape_to_country, country_perf, min_events_for_auc = 5) {

  tables <- list()
  plots  <- list()

  ## Day-of-year seasonal cycle by year, for the countries with the most recorded outbreaks
  top_countries_by_outbreaks <- country_perf |> dplyr::slice_max(n_pos, n = 6) |> dplyr::pull(Country)
  first_window <- min(hex_preds$forecast_interval)

  seasonal_data <- hex_preds |>
    dplyr::filter(forecast_interval == first_window) |>
    dplyr::left_join(shape_to_country, by = "shapeName") |>
    dplyr::filter(Country %in% top_countries_by_outbreaks) |>
    dplyr::mutate(doy = lubridate::yday(date), year = factor(lubridate::year(date)))

  plots$seasonal_cycle <- seasonal_data |>
    dplyr::group_by(Country, doy, year) |>
    dplyr::summarize(prob_pred = mean(prob_pred), true_out = max(true_out), .groups = "drop") |>
    ggplot2::ggplot(ggplot2::aes(doy, prob_pred, colour = year)) +
    ggplot2::geom_line(ggplot2::aes(group = year), alpha = 0.6) +
    ggplot2::geom_vline(data = ~ dplyr::filter(.x, true_out == 1), ggplot2::aes(xintercept = doy, colour = year), linetype = "dashed") +
    ggplot2::facet_wrap(~Country, scales = "free_y") +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::labs(x = "Day of year", y = "Mean predicted probability"
         , title = "Seasonal cycle by country, dashed lines = true outbreak days") +
    ggplot2::theme_bw()

  ## STL decomposition + dominant-cycle-length per top country: splits each country's
  ## monthly mean predicted-probability series into trend / yearly-seasonal / remainder,
  ## and the periodogram's peak period says whether that country's dynamics read as
  ## "strongly annual" (peak ~12 months), "longer-cycle" (peak well above 12), or
  ## "chaotic/no dominant cycle" (flat, broadband spectrum, no clear peak)
  monthly_by_country <- seasonal_data |>
    dplyr::mutate(ym = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(Country, ym) |>
    dplyr::summarize(prob_pred = mean(prob_pred), .groups = "drop") |>
    dplyr::arrange(Country, ym)

  decomp_results <- purrr::map(top_countries_by_outbreaks, function(cty) {

    cty_tbl <- monthly_by_country |> dplyr::filter(Country == cty)
    if (nrow(cty_tbl) < 24) return(NULL)
    d <- decompose_series(cty_tbl, "prob_pred", lubridate::year(min(cty_tbl$ym)), lubridate::month(min(cty_tbl$ym)))
    if (is.null(d)) return(NULL)
    list(country = cty, decomp = d)

  }) |> purrr::compact()

  tables$cycle_summary <- purrr::map_dfr(decomp_results, function(x) {

    tibble::tibble(
        Country            = x$country
      , peak_period_months = x$decomp$peak_period_months
      , dynamics_read_as   = dplyr::case_when(
          x$decomp$peak_period_months >= 10 & x$decomp$peak_period_months <= 14 ~ "strong yearly cycle"
        , x$decomp$peak_period_months > 14                                       ~ "longer-than-yearly cycle"
        , TRUE                                                                    ~ "short/chaotic-looking dominant frequency"
      )
    )

  })

  if (length(decomp_results) > 0) {

    stl_plots <- purrr::map(decomp_results[seq_len(min(2, length(decomp_results)))], function(x) {
      tibble::as_tibble(x$decomp$stl$time.series) |>
        dplyr::mutate(ym = monthly_by_country |> dplyr::filter(Country == x$country) |> dplyr::pull(ym)) |>
        tidyr::pivot_longer(c(seasonal, trend, remainder), names_to = "component", values_to = "value") |>
        ggplot2::ggplot(ggplot2::aes(ym, value)) + ggplot2::geom_line() + ggplot2::facet_wrap(~component, ncol = 1, scales = "free_y") +
        ggplot2::labs(x = NULL, y = NULL, title = paste0(x$country, " -- STL decomposition (peak cycle ~"
                                                , round(x$decomp$peak_period_months, 1), " months)")) +
        ggplot2::theme_bw()
    })
    names(stl_plots) <- paste0("stl_", purrr::map_chr(decomp_results[seq_len(length(stl_plots))], "country"))
    plots <- c(plots, stl_plots)

  }

  ## Rolling quarterly performance over time
  tables$rolling_perf <- hex_preds |>
    dplyr::mutate(window = lubridate::floor_date(date, "3 months")) |>
    dplyr::group_by(window) |>
    dplyr::group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    dplyr::ungroup()

  plots$rolling_perf <- tables$rolling_perf |>
    dplyr::filter(n_pos >= min_events_for_auc) |>
    ggplot2::ggplot(ggplot2::aes(window, logloss)) +
    ggplot2::geom_line() + ggplot2::geom_point(ggplot2::aes(size = n_pos)) +
    ggplot2::labs(x = "Quarter", y = "Mean log loss", size = "True outbreaks\nin window"
         , title = "Rolling quarterly performance over time") +
    ggplot2::theme_bw()

  list(tables = tables, plots = plots)

}


#' @rdname diagnose_spatial_performance
#' @export
diagnose_model_structure <- function(fitted_model, test_data, chosen_outer_fold = 1, chosen_purpose_tag = "for_test_data"
                                     , shap_sample_n = 3000, pdp_grid_res = 25) {

  tables <- list()
  plots  <- list()

  ## This fold's fitted model and recipe; fall back to any saved purpose tag if the chosen
  ## one is not among the fitted models
  fold_files <- locate_fold_files(fitted_model, chosen_outer_fold, chosen_purpose_tag)
  if (!length(fold_files$parsnip_path)) {
    other_tag  <- sub(paste0(".*_", chosen_outer_fold, "_(for_[a-z_]+?)_tune.*"), "\\1", basename(fitted_model[grepl("parsnip_fit", fitted_model)][1]))
    message("No fitted model for purpose tag '", chosen_purpose_tag, "'; using '", other_tag, "' instead.")
    fold_files <- locate_fold_files(fitted_model, chosen_outer_fold, other_tag)
  }

  parsnip_fit <- readRDS(fold_files$parsnip_path[1])
  fold_recipe <- readRDS(fold_files$recipe_path[1])
  booster     <- parsnip_fit$fit
  x_names     <- parsnip_fit$preproc$x_names

  ## Gain-based importance and vip's version of the same on the parsnip fit
  tables$xgb_importance <- tibble::as_tibble(xgboost::xgb.importance(model = booster, feature_names = x_names))
  tables$vip_importance <- vip::vi(parsnip_fit)

  plots$importance <- tables$xgb_importance |>
    dplyr::slice_max(Gain, n = 25) |>
    ggplot2::ggplot(ggplot2::aes(Gain, stats::reorder(Feature, Gain))) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(x = "Gain", y = NULL, title = "Top 25 features by xgboost Gain") +
    ggplot2::theme_bw()

  ## SHAP: bake a sample of test_data through this fold's recipe, then use
  ## xgboost::predcontrib
  shap_sample <- test_data |> dplyr::slice_sample(n = min(shap_sample_n, nrow(test_data)))
  baked_shap  <- recipes::bake(fold_recipe, new_data = shap_sample) |>
    dplyr::select(dplyr::all_of(x_names))

  shap_long <- compute_shap_long(booster, baked_shap)

  tables$shap_feature_order <- shap_long |>
    dplyr::group_by(feature) |>
    dplyr::summarize(mean_abs_shap = mean(abs(shap_value)), .groups = "drop") |>
    dplyr::slice_max(mean_abs_shap, n = 20)

  plots$shap_summary <- tables$shap_feature_order |>
    ggplot2::ggplot(ggplot2::aes(mean_abs_shap, stats::reorder(feature, mean_abs_shap))) +
    ggplot2::geom_col(fill = "darkorange") +
    ggplot2::labs(x = "Mean |SHAP value|", y = NULL, title = "Global SHAP importance (top 20)") +
    ggplot2::theme_bw()

  ## Beeswarm-style SHAP distribution
  ## one row per observation, x = SHAP value, y = feature ranked by importance,
  ## colour = the observation's rescaled feature value
  plots$shap_beeswarm <- shap_long |>
    dplyr::filter(feature %in% tables$shap_feature_order$feature) |>
    dplyr::left_join(tables$shap_feature_order, by = "feature") |>
    dplyr::group_by(feature) |>
    dplyr::mutate(feature_value_scaled = scales::rescale(feature_value, to = c(0, 1))) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(shap_value, forcats::fct_reorder(feature, mean_abs_shap))) +
    ggplot2::geom_jitter(ggplot2::aes(colour = feature_value_scaled), height = 0.3, alpha = 0.5, size = 0.8) +
    ggplot2::scale_colour_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 0.5
                           , name = "Feature value\n(low -> high)") +
    ggplot2::labs(x = "SHAP value", y = NULL, title = "SHAP value distribution (top 20 features)") +
    ggplot2::theme_bw()

  ## SHAP dependence plot for the single top feature (shape of effect)
  top_feature <- tables$shap_feature_order$feature[1]
  plots$shap_dependence <- shap_long |>
    dplyr::filter(feature == top_feature) |>
    ggplot2::ggplot(ggplot2::aes(feature_value, shap_value)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_smooth(se = FALSE, colour = "firebrick3") +
    ggplot2::labs(x = top_feature, y = "SHAP value"
         , title = paste0("SHAP dependence: ", top_feature, " (top feature by mean |SHAP|)")) +
    ggplot2::theme_bw()

  ## Simple partial dependence for the same top feature, using the pdp package
  pdp_pred_fun <- function(object, newdata) stats::predict(object, new_data = newdata, type = "prob")$.pred_1

  pdp_result <- tryCatch(
    pdp::partial(
        object = parsnip_fit, pred.var = top_feature, pred.fun = pdp_pred_fun
      , train = baked_shap, grid.resolution = pdp_grid_res
    )
    , error = function(e) { message("PDP failed: ", conditionMessage(e)); NULL }
  )

  if (!is.null(pdp_result)) {
    plots$pdp <- pdp_result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(top_feature))) |>
      dplyr::summarize(yhat = mean(yhat), .groups = "drop") |>
      ggplot2::ggplot(ggplot2::aes(.data[[top_feature]], yhat)) + ggplot2::geom_line() +
      ggplot2::labs(y = "Mean predicted P(outbreak)", title = paste0("Partial dependence: ", top_feature)) +
      ggplot2::theme_bw()
  }

  ## lorax::as.party() needs the outcome column included; re bake with this column
  baked_for_tree      <- recipes::bake(fold_recipe, new_data = shap_sample) |> as.data.frame()
  plots$tree_diagram  <- render_one_tree(booster, tree_number = 1, data_for_party = baked_for_tree)

  ## Aggregate tree-shape stats across a sample of trees
  n_trees_to_sample <- 50
  ## All trees, then the first n_trees_to_sample (tree indexing differs across xgboost versions)
  tree_stats <- xgboost::xgb.model.dt.tree(model = booster) |>
    tibble::as_tibble()
  tree_stats <- tree_stats |>
    dplyr::filter(Tree %in% utils::head(sort(unique(tree_stats$Tree)), n_trees_to_sample))

  tables$tree_shape_summary <- tree_stats |>
    dplyr::group_by(Tree) |>
    dplyr::summarize(
        n_nodes   = dplyr::n()
      , n_leaves  = sum(Feature == "Leaf")
      , max_depth = max(stringr::str_count(ID, "-"))
      , .groups = "drop"
    )

  ## Which features get picked as the very first split most often
  tables$first_split_features <- tree_stats |>
    dplyr::filter(Node == 0) |>
    dplyr::count(Feature, sort = TRUE)

  plots$first_splits <- tables$first_split_features |>
    dplyr::slice_max(n, n = 15) |>
    ggplot2::ggplot(ggplot2::aes(n, stats::reorder(Feature, n))) +
    ggplot2::geom_col(fill = "seagreen") +
    ggplot2::labs(x = paste0("Times chosen as root split (of first ", n_trees_to_sample, " trees)"), y = NULL
         , title = "Most common root-node (first-split) features") +
    ggplot2::theme_bw()

  list(tables = tables, plots = plots)

}

## Variable importance across all outer-fold models, from the variable_importance target
 ## (calculate_variable_importance(), one row per fitted model): gain importance per fold, its
 ## summary across folds, partial dependence of each fold's top features, the compare_vi() gain plot
diagnose_variable_importance <- function(variable_importance) {

  ## Failed branches (error = "null") are dropped
  variable_importance <- variable_importance |> dplyr::filter(!purrr::map_lgl(top_variables_vip, is.null))
  if (!nrow(variable_importance)) stop("No variable importance results")

  tables <- list()
  plots  <- list()

  ## Gain importance of every feature in every fold model (vip::vi() on the xgboost fit)
  tables$vi_by_fold <- variable_importance$top_variables_vip |>
    purrr::map_dfr(tibble::as_tibble) |>
    dplyr::rename(feature = Variable, gain = Importance)

  ## Across folds; a feature a fold model never split on counts as zero gain in that fold
  n_folds <- dplyr::n_distinct(tables$vi_by_fold$outer_fold_id)
  tables$vi_summary <- tables$vi_by_fold |>
    tidyr::complete(feature, outer_fold_id, fill = list(gain = 0)) |>
    dplyr::mutate(fold_rank = rank(-gain, ties.method = "min"), .by = outer_fold_id) |>
    dplyr::summarize(
      mean_gain       = mean(gain)
    , sd_gain         = stats::sd(gain)
    , min_gain        = min(gain)
    , max_gain        = max(gain)
    , median_rank     = stats::median(fold_rank)
    , n_folds_top10   = sum(fold_rank <= 10)
    , n_folds_used    = sum(gain > 0)
    , .by = feature) |>
    dplyr::mutate(n_folds = n_folds) |>
    dplyr::arrange(dplyr::desc(mean_gain))

  ## Partial dependence (predicted probability) of each fold's top features
  tables$pdp <- variable_importance$pdp_df |> dplyr::bind_rows()

  ## Gain per fold (one point per fold model)
  compared <- compare_vi(variable_importance)
  plots$vi_gain <- compared$vi_gg[[1]] +
    ggplot2::labs(x = "Gain (share of total loss reduction)", y = NULL
                , title = paste0("xgboost gain importance across ", n_folds, " outer-fold models (top 30 per model)"))

  ## Partial dependence restricted to the features with the highest mean gain, each on its own x scale
  top_features <- utils::head(tables$vi_summary$feature, 12)
  plots$pdp <- tables$pdp |>
    dplyr::filter(variable %in% top_features) |>
    ggplot2::ggplot(ggplot2::aes(value, yhat)) +
    ggplot2::geom_line(ggplot2::aes(group = outer_fold_id), alpha = 0.6) +
    ggplot2::facet_wrap(~ factor(variable, levels = top_features), scales = "free_x"
                      , labeller = ggplot2::label_wrap_gen(28)) +
    ggplot2::labs(x = "Feature value (after the recipe's preprocessing)", y = "Predicted probability"
                , title = "Partial dependence of the 12 features with the highest mean gain (one line per outer-fold model in whose top 10 the feature was)")

  list(tables = tables, plots = plots)

}


#### Helper functions --------------------------------------------------------------

## Locate this fold's parsnip_fit/recipe files as used in R/calculate_variable_importance.R and
## R/calculate_shap_by_forecast_interval.R (with the addition of purpose tag)
locate_fold_files <- function(fitted_model, outer_fold_id, purpose_tag) {
  
  fold_match <- paste0("_", outer_fold_id, "_", purpose_tag, "_")
  
  list(
    parsnip_path = fitted_model[grepl("parsnip_fit", fitted_model) & grepl(fold_match, fitted_model)]
    , recipe_path  = fitted_model[grepl("recipe_", fitted_model) & grepl(fold_match, fitted_model)]
  )
  
}


## Overall + grouped scoring: ROC-AUC, PR-AUC, log loss, Brier score, and a Brier
## Skill Score (BSS) relative to a climatological (base-rate) forecast.
score_group <- function(df, min_events_for_auc) {
  
  truth_fct <- factor(df$true_out, levels = c("1", "0"))
  clim_p    <- mean(df$true_out)
  bs_model  <- mean((df$prob_pred - df$true_out)^2)
  bs_clim   <- mean((clim_p - df$true_out)^2)
  decomp    <- reliability_decomposition(df$prob_pred, df$true_out)
  
  tibble(
    n            = nrow(df)
    , n_pos        = sum(df$true_out == 1)
    , prevalence   = clim_p
    , roc_auc      = if (sum(df$true_out == 1) >= min_events_for_auc) tryCatch(yardstick::roc_auc_vec(truth_fct, df$prob_pred, event_level = "first"), error = function(e) NA_real_) else NA_real_
    , pr_auc       = if (sum(df$true_out == 1) >= min_events_for_auc) tryCatch(yardstick::pr_auc_vec(truth_fct, df$prob_pred, event_level = "first"), error = function(e) NA_real_) else NA_real_
    , logloss      = tryCatch(yardstick::mn_log_loss_vec(truth_fct, df$prob_pred, event_level = "first"), error = function(e) NA_real_)
    , brier        = bs_model
    , brier_skill  = if (bs_clim > 0) 1 - (bs_model / bs_clim) else NA_real_
    ## Murphy (1973) decomposition: brier = reliability - resolution + uncertainty.
    ## A negative brier_skill on its own can't tell you whether that's because the
    ## model doesn't discriminate (low resolution) or because it discriminates fine
    ## but the magnitudes are off (high reliability + miscalibration)
    ## These columns seek to figure this out apart.
    , reliability  = decomp$reliability
    , resolution   = decomp$resolution
    , average_p    = mean(df$prob_pred)
  )
  
}


## Murphy (1973) three-term Brier decomposition (brier = reliability - resolution +
## uncertainty) using probability bins selected and sized for the huge class imbalance
## we have here (i.e., finer near 0, where most predictions lie, coarser above 0.1).
reliability_decomposition <- function(prob_pred, true_out, breaks = c(0, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 1)) {
  
  clim_p <- mean(true_out)
  bin    <- cut(prob_pred, breaks, include.lowest = TRUE)
  tab    <- tibble(prob_pred, true_out, bin) |>
    group_by(bin) |>
    summarize(n = n(), pbar = mean(prob_pred), ybar = mean(true_out), .groups = "drop")
  
  list(
    reliability = sum(tab$n * (tab$pbar - tab$ybar)^2) / length(true_out)
    , resolution  = sum(tab$n * (tab$ybar - clim_p)^2) / length(true_out)
  )
  
}


## Fetch the NOAA CPC Oceanic Nino Index (ONI) table
## This is the standard rolling 3-month-mean SST-anomaly series used to define 
## El Nino / La Nina / Neutral phases. Format is "SEAS YR TOTAL ANOM" with SEAS 
## a 3-letter rolling-season code (e.g. "DJF"); the season's middle month is used 
## as its representative month for joining against prediction dates. 
fetch_oni <- function() {
  
  season_mid_month <- c(
    DJF = 1, JFM = 2, FMA = 3, MAM = 4, AMJ = 5, MJJ = 6
    , JJA = 7, JAS = 8, ASO = 9, SON = 10, OND = 11, NDJ = 12
  )
  
  tryCatch({
    oni_raw <- read.table(
      "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
      , header = TRUE, stringsAsFactors = FALSE
    )
    oni_raw |>
      as_tibble() |>
      mutate(
        month = season_mid_month[SEAS]
        , year  = YR
        , oni   = ANOM
        ## standard convention: ONI >= 0.5 El Nino, <= -0.5 La Nina, else Neutral.
        ## Real ONI phase designation additionally requires 5 consecutive
        ## overlapping seasons past threshold; this is a simplified single-season
        ## classification, noted here rather than silently implying the full
        ## NOAA-official designation
        , enso_phase = case_when(
          oni >=  0.5 ~ "El Nino"
          , oni <= -0.5 ~ "La Nina"
          , TRUE         ~ "Neutral"
        )
      ) |>
      dplyr::select(year, month, oni, enso_phase)
  }, error = function(e) {
    
    message("Could not fetch ONI index (needs network access): ", conditionMessage(e))
    message("Skipping ONI-phase diagnostics in Section C -- see fetch_oni() for the source URL.")
    NULL
    
  })
  
}


## Literature-informed (Anyamba et al. 2009, PNAS) label for how strongly a 
## country's RVF outbreak history has been tied to ENSO.
## Many countries not found (I can look harder if we actually want to go
## somewhere with this), labeled for now as "other"
enso_sensitivity_lookup <- function() {
  
  tibble::tribble(
    ~Country, ~country_name,               ~macro_region,               ~enso_rvf_sensitivity,
    "DJI",    "Djibouti",                  "Horn of Africa",            "high",
    "ERI",    "Eritrea",                   "Horn of Africa",            "high",
    "ETH",    "Ethiopia",                  "Horn of Africa",            "high",
    "KEN",    "Kenya",                     "Horn of Africa",            "high",
    "SDN",    "Sudan",                     "Horn of Africa",            "high",
    "SOM",    "Somalia",                   "Horn of Africa",            "high",
    "SSD",    "South Sudan",               "Horn of Africa",            "high",
    "TZA",    "Tanzania",                  "East Africa",               "high",
    "BDI",    "Burundi",                   "East Africa/Great Lakes",   "moderate",
    "RWA",    "Rwanda",                    "East Africa/Great Lakes",   "moderate",
    "UGA",    "Uganda",                    "East Africa/Great Lakes",   "moderate",
    "COM",    "Comoros",                   "Indian Ocean Islands",      "moderate",
    "MDG",    "Madagascar",                "Indian Ocean Islands",      "moderate",
    "MYT",    "Mayotte",                   "Indian Ocean Islands",      "moderate",
    "MOZ",    "Mozambique",                "Southern Africa",           "moderate",
    "ZWE",    "Zimbabwe",                  "Southern Africa",           "moderate",
    "AGO",    "Angola",                    "Southern Africa",           "other",
    "BWA",    "Botswana",                  "Southern Africa",           "other",
    "LSO",    "Lesotho",                   "Southern Africa",           "other",
    "MWI",    "Malawi",                    "Southern Africa",           "other",
    "NAM",    "Namibia",                   "Southern Africa",           "other",
    "SWZ",    "Eswatini",                  "Southern Africa",           "other",
    "ZAF",    "South Africa",              "Southern Africa",           "other",
    "ZMB",    "Zambia",                    "Southern Africa",           "other",
    "BEN",    "Benin",                     "West Africa",               "other",
    "BFA",    "Burkina Faso",              "West Africa/Sahel",         "other",
    "CIV",    "Cote d'Ivoire",             "West Africa",               "other",
    "GHA",    "Ghana",                     "West Africa",               "other",
    "GIN",    "Guinea",                    "West Africa",               "other",
    "GMB",    "Gambia",                    "West Africa",               "other",
    "GNB",    "Guinea-Bissau",             "West Africa",               "other",
    "LBR",    "Liberia",                   "West Africa",               "other",
    "MLI",    "Mali",                      "West Africa/Sahel",         "other",
    "MRT",    "Mauritania",                "West Africa/Sahel",         "other",
    "NER",    "Niger",                     "West Africa/Sahel",         "other",
    "NGA",    "Nigeria",                   "West Africa",               "other",
    "SEN",    "Senegal",                   "West Africa/Sahel",         "other",
    "SLE",    "Sierra Leone",              "West Africa",               "other",
    "TGO",    "Togo",                      "West Africa",               "other",
    "CAF",    "Central African Republic",  "Central Africa",            "other",
    "CMR",    "Cameroon",                  "Central Africa",            "other",
    "COD",    "DR Congo",                  "Central Africa",            "other",
    "COG",    "Congo",                     "Central Africa",            "other",
    "GAB",    "Gabon",                     "Central Africa",            "other",
    "GNQ",    "Equatorial Guinea",         "Central Africa",            "other",
    "TCD",    "Chad",                      "Central Africa/Sahel",      "other",
    "DZA",    "Algeria",                   "North Africa",              "other",
    "EGY",    "Egypt",                     "North Africa",              "other",
    "LBY",    "Libya",                     "North Africa",              "other",
    "MAR",    "Morocco",                   "North Africa",              "other",
    "TUN",    "Tunisia",                   "North Africa",              "other"
  )
  
}


## Country-level ENSO climate-impact ranking
## Correlates each African country's temperature/precipitation anomalies against 
## the ONI index
#' @param enso_data_dir Directory containing global_countries_temp.json,
#'   global_countries_precip.json, and viz2.csv (ONI time series).
#' @param correlation_method Passed to cor(); spearman (rank-based) default
#' @return A list with ranking (tibble), gg_ranking, and gg_map (ggplot objects)
compute_enso_country_impact_ranking <- function(enso_data_dir, correlation_method) {
  
  temp_path   <- file.path(enso_data_dir, "global_countries_temp.json")
  precip_path <- file.path(enso_data_dir, "global_countries_precip.json")
  oni_path    <- file.path(enso_data_dir, "viz2.csv")
  
  if (!all(file.exists(temp_path, precip_path, oni_path))) {
    message("compute_enso_country_impact_ranking(): one or more required files not found ",
            "under ", enso_data_dir, " (global_countries_temp.json, global_countries_precip.json, ",
            "viz2.csv) -- skipping the ENSO country-impact ranking.")
    return(NULL)
  }
  
  tryCatch({
    temp_tbl   <- load_country_climate_json(temp_path, "temp")
    precip_tbl <- load_country_climate_json(precip_path, "precip")
    
    climate_tbl <- temp_tbl |>
      full_join(precip_tbl, by = c("country", "date")) |>
      add_anomaly("temp") |>
      add_anomaly("precip")
    
    oni_local <- readr::read_csv(oni_path, show_col_types = FALSE) |>
      transmute(
        date       = as.Date(paste0(Date, "-01"), format = "%Y-%b-%d")
        , oni        = `ONI value`
        , enso_phase = `ONI Type`
      )
    
    ## Restrict to African countries -- rnaturalearth's iso_a3 is the same
    ## ISO3-country-code convention already used throughout the pipeline
    africa_iso3 <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf") |>
      filter(iso_a3 != "-99") |>
      pull(iso_a3)
    
    africa_climate_oni <- climate_tbl |>
      filter(country %in% africa_iso3) |>
      inner_join(oni_local, by = "date")
    
    enso_climate_impact_ranking <- africa_climate_oni |>
      group_by(country) |>
      summarize(
        n          = n()
        , cor_temp   = suppressWarnings(cor(temp_anom, oni, method = correlation_method, use = "pairwise.complete.obs"))
        , cor_precip = suppressWarnings(cor(precip_anom, oni, method = correlation_method, use = "pairwise.complete.obs"))
        , .groups = "drop"
      ) |>
      ## Combined impact magnitude: root-mean-square of the two |correlation|s, so a
      ## country strongly affected on either axis ranks highly
      mutate(combined_impact = sqrt((cor_temp^2 + cor_precip^2) / 2)) |>
      arrange(desc(combined_impact)) |>
      mutate(rank = row_number(), .before = 1)
    
    gg_enso_impact_ranking <- enso_climate_impact_ranking |>
      slice_head(n = 20) |>
      pivot_longer(c(cor_temp, cor_precip), names_to = "variable", values_to = "correlation") |>
      mutate(variable = recode(variable, cor_temp = "Temperature", cor_precip = "Precipitation")) |>
      ggplot(aes(correlation, reorder(country, combined_impact), fill = variable)) +
      geom_col(position = "dodge") +
      geom_vline(xintercept = 0, colour = "grey40") +
      labs(x = paste0(str_to_title(correlation_method), " correlation with ONI"), y = NULL
           , fill = NULL, title = "Top 20 African countries by ENSO climate impact"
           , subtitle = "Correlation of monthly temperature/precipitation ANOMALIES (seasonal cycle removed) with ONI") +
      theme_bw()
    
    gg_enso_impact_map <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf") |>
      left_join(enso_climate_impact_ranking, by = c("iso_a3" = "country")) |>
      ggplot() +
      geom_sf(aes(fill = combined_impact), colour = "white", linewidth = 0.1) +
      scale_fill_viridis_c(name = "ENSO impact\n(combined)", na.value = "grey90") +
      theme_void() +
      labs(title = "African countries by ENSO climate impact")
    
    list(ranking = enso_climate_impact_ranking, gg_ranking = gg_enso_impact_ranking, gg_map = gg_enso_impact_map)
  }, error = function(e) {
    message("compute_enso_country_impact_ranking() failed: ", conditionMessage(e))
    NULL
  })
  
}


## Convert one variable's global_countries_*.json (data$<ISO3>$"<YYYY-MM>" = value)
## into a tidy (country, date, value) tibble
load_country_climate_json <- function(path, value_name) {
  
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  purrr::imap_dfr(raw$data, function(country_series, iso3) {
    unlisted <- unlist(country_series)
    tibble(
      country = iso3
      , date    = lubridate::ym(names(unlisted))
      , value   = as.numeric(unlisted)
    )
  }) |>
    rename(!!value_name := value)
  
}


## Country-calendar-month anomaly: value minus that country's own long-term mean
## for that same calendar month 
add_anomaly <- function(df, value_col) {
  
  df |>
    mutate(cal_month = lubridate::month(date)) |>
    group_by(country, cal_month) |>
    mutate("{value_col}_anom" := .data[[value_col]] - mean(.data[[value_col]], na.rm = TRUE)) |>
    ungroup() |>
    select(-cal_month)
  
}


## STL decomposition + dominant-cycle-length (via periodogram) for one regularly
## spaced monthly series. Returns NULL if there isn't enough history for a 
## full 2-period STL window.
decompose_series <- function(monthly_tbl, value_col, start_year, start_month) {
  
  if (nrow(monthly_tbl) < 24) return(NULL)
  ts_obj <- ts(monthly_tbl[[value_col]], start = c(start_year, start_month), frequency = 12)
  stl_fit <- stl(ts_obj, s.window = "periodic")
  
  ## spec.pgram's frequency axis is cycles per sampling unit (per month here);
  ## convert the strongest non-zero-frequency peak to a period in months so
  ## "close to 12" reads directly as "a yearly cycle"
  spec <- spec.pgram(ts_obj, plot = FALSE, taper = 0.1)
  
  peak_idx    <- which.max(spec$spec)
  peak_period <- 1 / spec$freq[peak_idx]
  
  list(stl = stl_fit, spec = spec, peak_period_months = peak_period)
  
}


## Long-format SHAP contributions for a sample of rows, using xgboosts predcontrib 
compute_shap_long <- function(booster, baked_x) {
  
  shap_mat <- predict(booster, xgb_model_matrix(baked_x), predcontrib = TRUE)
  shap_df  <- as_tibble(shap_mat) |> mutate(.row = row_number())
  feat_df  <- baked_x |> mutate(.row = row_number())
  
  shap_long <- shap_df |>
    pivot_longer(-.row, names_to = "feature", values_to = "shap_value") |>
    filter(feature != "(Intercept)")
  
  feat_long <- feat_df |>
    pivot_longer(-.row, names_to = "feature", values_to = "feature_value")
  
  shap_long |> left_join(feat_long, by = c(".row", "feature"))
  
}


## One rendered decision tree. Tries lorax first but falls back to a visNetwork
## hierarchical layout using xgb.model.dt.tree() if lorax isn't installed or isn't
## working correctly
render_one_tree <- function(booster, tree_number = 1, data_for_party = NULL) {
  
  if (requireNamespace("lorax", quietly = TRUE) && !is.null(data_for_party)) {
    
    lorax_plot <- tryCatch(
      
      lorax::as.party(booster, tree = tree_number, data = data_for_party)
      
      , error = function(e) {
        message("lorax rendering failed (", conditionMessage(e), ") -- falling back to a ",
                "visNetwork tree diagram.")
        NULL
      }
    )
    
    if (!is.null(lorax_plot)) return(lorax_plot)
    
  } else if (requireNamespace("lorax", quietly = TRUE)) {
    
    message("lorax is installed but no data_for_party was supplied -- as.party.xgb.Booster() ",
            "needs the original data (with the outcome column included) since xgboost doesn't ",
            "store either -- falling back to a visNetwork tree diagram.")
    
  } else {
    
    message("lorax is not installed -- falling back to a visNetwork tree diagram. ",
            "Install lorax for nicer rendering if desired.")
    
  }
  
  dt <- xgboost::xgb.model.dt.tree(model = booster, trees = tree_number)
  
  nodes <- dt |>
    mutate(
      label = if_else(
        Feature == "Leaf"
        , paste0("leaf\n", round(Gain, 3))
        , paste0(Feature, "\n< ", signif(Split, 3))
      )
      , level = str_count(ID, "-")
    ) |>
    transmute(id = ID, label, level, shape = if_else(Feature == "Leaf", "box", "ellipse")
              , color = if_else(Feature == "Leaf", "#f2f2f2", "#a6cee3"))
  
  edges <- bind_rows(
    dt |> filter(!is.na(Yes)) |> transmute(from = ID, to = Yes, label = "yes")
    , dt |> filter(!is.na(No))  |> transmute(from = ID, to = No,  label = "no")
  )
  
  visNetwork::visNetwork(nodes, edges, main = paste("Tree", tree_number - 1)) |>
    visNetwork::visHierarchicalLayout(direction = "UD", sortMethod = "directed") |>
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visNodes(font = list(size = 14))
  
}

