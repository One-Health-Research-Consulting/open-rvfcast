#' Run a prediction diagnostics suite
#'
#' Exploration of the quality of RVF predictions including scoring/calibration, 
#' spatial performance, ENSO coupling, temporal dynamics, variable importance/SHAP, 
#' and tree-structure visuals.
#' 
#' Sections:
#' Overall scoring, calibration curves, and comparison to published forecasting
#'    benchmarks (CDC FluSight, dengue/malaria/RVF early-warning literature)
#' Spatial diagnostics: hex-level, region-level, country-level, and macro-region
#'    performance, including the positive/negative predicted-probability 
#'    by country 
#' ENSO / climate-phase coupling: does performance or key covariates shift with
#'    El Nino / La Nina / Neutral phases (uses NOAA ONI index); both a 
#'    data-driven country ENSO-impact ranking (temperature/precipitation anomaly
#'    correlation with ONI) and a literature-based look up table
#' Temporal dynamics: seasonal (day-of-year) cycles, STL trend/seasonal/remainder
#'    decomposition, dominant-cycle-length via periodogram, and rolling performance
#'    over time
#' What's driving predictions: xgboost gain-based importance, vip, and SHAP
#'    (summary, bee swarm, and dependence plots)
#' Tree structure visuals: a real rendered tree (via lorax if installed, else a
#'    visNetwork hierarchical diagram built from xgb.model.dt.tree(), no extra
#'    dependency required either way) plus aggregate tree-shape statistics
#' 
#' Outputs are placed into a single results list, which is saved to disk and
#' uploaded to the S3 bucket (see separate target) 
#'
#' @param predictions_path Path to a saved .qs of held-out predictions; the
#'   ex_fits.all_probs_raw target.
#' @param test_data Predictor data; the test_data target
#' @param fitted_model Character vector of fitted-model file paths; the fitted_model target 
#' @param region_hexes The region_hexes target 
#' @param performance_hexes The performance_hexes target 
#' @param out_dir Directory to write this function's outputs 
#' @param chosen_outer_fold Which saved fold's fitted model to use for the model-object
#'   diagnostics (variable importance, SHAP, tree structure). Default 1.
#' @param chosen_purpose_tag Which of that fold's fits to use; distinguishes a fold's
#'   "for_test_data" fit from "for_forecasting". Default "for_test_data".
#' @param shap_sample_n Sample size for the SHAP section, which requires baking rows
#'   through a large recipe and is worth keeping modest for speed. Default 3000.
#' @param pdp_grid_res Grid resolution for the partial dependence plot. Default 25.
#' @param min_events_for_auc Minimum true-positive count required before trusting an
#'   AUC/PR-AUC computed on a spatial or temporal subgroup. Default 5.
#' @param enso_data_dir Directory holding the local ENSO country-impact inputs
#'   (global_countries_temp.json, global_countries_precip.json, viz2.csv; see
#'   compute_enso_country_impact_ranking()). Default here::here("data/ENSO").
#' @param correlation_method Correlation method for the ENSO country-impact ranking. 
#'   Default "spearman" (rank-based to be robust to skewed precipitation).
#'
#' @return Character path to the saved comprehensive_diagnostics_results.qs file
#' @author Morgan Kain
#' @export
run_comprehensive_fit_diagnostics <- function(
    predictions_path
    , test_data
    , fitted_model
    , region_hexes
    , performance_hexes
    , out_dir
    , chosen_outer_fold  = 1
    , chosen_purpose_tag = "for_test_data"
    , shap_sample_n      = 3000
    , pdp_grid_res       = 25
    , min_events_for_auc = 5
    , enso_data_dir      = here::here("data/ENSO")
) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  #### 1) Load core data ------------------------------------------------------------------
  
  all_preds   <- qs::qread(predictions_path)
  fold_files  <- locate_fold_files(fitted_model, chosen_outer_fold, chosen_purpose_tag)
  parsnip_fit <- readRDS(fold_files$parsnip_path[1])
  fold_recipe <- readRDS(fold_files$recipe_path[1])
  booster     <- parsnip_fit$fit
  x_names     <- parsnip_fit$preproc$x_names
  
  ## region_hexes/performance_hexes are passed in as the raw targets 
  hex_fine    <- region_hexes[[1]]        ## shapeName-resolution hexes
  hex_coarse  <- performance_hexes[[1]]   ## region_norm-resolution hexes
  
  ## Guarantee one row per shapeName (take the modal Country per shapeName
  ## the same way region_to_country does), otherwise downstream joins go
  ## many-to-many and silently duplicate rows
  shape_to_country <- test_data |>
    count(shapeName, Country) |>
    slice_max(n, n = 1, by = shapeName) |>
    dplyr::select(shapeName, Country)
  
  ## region_norm -> Country: a region_norm hex can span more than one country near a
  ## border, so this takes the modal (most common) Country among its child shapeNames
  ## as a labeling convenience 
  region_to_country <- shape_to_country |>
    mutate(region_norm = h3jsr::get_parent(shapeName, 2)) |>
    count(region_norm, Country) |>
    slice_max(n, n = 1, by = region_norm) |>
    dplyr::select(region_norm, Country)
  
  africa_countries <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf") |>
    dplyr::select(iso_a3, geometry) |>
    filter(iso_a3 != "-99")
  
  enso_sensitivity <- enso_sensitivity_lookup()
  
  ## Create the list to save everything 
  results <- list()   
  
  
  #### 2) Overall scoring, calibration, and benchmark comparison ---------------
  
  ## Background prevalence; many metrics rely on this 
  overall_prevalence <- mean(all_preds$true_out)
  
  scores_by_agg_interval <- all_preds |>
    group_by(aggregation, forecast_interval) |>
    group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    ungroup()
  
  results$scores_by_agg_interval <- scores_by_agg_interval
  
  gg_scores_over_horizon <- scores_by_agg_interval |>
    filter(aggregation %in% c("No aggregation", "Spatial aggregation")) |>
    dplyr::select(aggregation, forecast_interval, roc_auc, pr_auc, logloss, brier_skill) |>
    pivot_longer(c(roc_auc, pr_auc, logloss, brier_skill), names_to = "metric", values_to = "value") |>
    ggplot(aes(forecast_interval, value, colour = aggregation)) +
    geom_line() + geom_point() +
    facet_wrap(~metric, scales = "free_y") +
    labs(x = "Forecast interval (days)", y = NULL
         , title = "Scoring metrics by forecast horizon"
         , subtitle = "brier_skill: 0 = no better than base-rate climatology, >0 = genuine skill") +
    theme_bw()
  
  results$gg_scores_over_horizon <- gg_scores_over_horizon
  
  ## Calibration curves via R/generate_calibration_curve.R
  calib_input <- all_preds |> filter(aggregation == "No aggregation")
  
  ## Somewhat inefficient to redo this here as it is done elsewhere in the pipeline,
  ## but w/e not terrible
  calcurves   <- generate_calibration_curve(
    preds    = calib_input, test_data = NULL, predname = "prob_pred", truename = "true_out"
    , splitgrp = "forecast_interval")
  
  plotted_calibration <- plot_calibration(
    caltib      = calcurves, xg = NULL
    , yg          = "forecast_interval"
    , forcastvals = c(30, 90, 150))
  
  results$calibration_opt  <- plotted_calibration$calplot.opt[[1]]
  results$calibration_even <- plotted_calibration$calplot.even[[1]]
  
  ## Benchmark comparison table. Some literature-sourced reference points.
  ## Not really directly comparable as this projects extreme class imbalance is
  ## somewhat different
  benchmark_table <- tibble::tribble(
    ~system,                                  ~metric,                ~value, ~caveat,
    "This model (No aggregation, overall)",   "ROC-AUC",              scores_by_agg_interval |> filter(aggregation == "No aggregation", is.na(forecast_interval) == FALSE) |> summarize(v = weighted.mean(roc_auc, n, na.rm = TRUE)) |> pull(v), "computed here",
    "This model (No aggregation, overall)",   "Brier Skill Score",    scores_by_agg_interval |> filter(aggregation == "No aggregation", is.na(forecast_interval) == FALSE) |> summarize(v = weighted.mean(brier_skill, n, na.rm = TRUE)) |> pull(v), "computed here; 0=no skill vs climatology",
    "RVF outbreaks, Kenya, XGBoost",          "ROC-AUC",              0.8908, "Mulwa et al. 2024 BDCC 8(11):148 -- accuracy/precision/recall also reported near 99.7-100%, a signature of a much less imbalanced label split than ours; verify before quoting precisely",
    "Malaria outbreaks, Gambia, XGBoost",     "ROC-AUC",              0.97,   "Khan et al. 2024 PLOS ONE -- ~13% outbreak base rate, far less imbalanced than our ~0.1%",
    "Malaria epidemic alerts, East Africa",   "Sensitivity/PPV",      NA_real_, "Githeko et al. 2014 Malar J 13:329 -- 75-100% sensitivity depending on valley ecosystem type, no single AUC reported",
    "Dengue EWARS-csd alarms, Colombia",      "Sensitivity/Specificity", NA_real_, "Schlesinger et al. 2024 Front Public Health -- median sens 0.97, spec 0.94 across 11 municipalities",
    "Dengue forecasting challenge (ensemble)", "Log score vs null",   NA_real_, "Johansson et al. 2019 PNAS 116(48) -- ensemble was the only entry to beat a null model on every target; individual models often did not",
    "CDC FluSight (best model / ensemble)",   "Relative WIS vs baseline", 0.77, "Mathis et al. 2024 Nat Commun 15:6289 -- ensemble 0.77-0.82 across two seasons; only 6/23-12/18 individual models beat the naive baseline at all",
    "NEON aquatics forecasting challenge",    "CRPS skill vs climatology", NA_real_, "Olsson et al. 2025 Ecol Appl 35(1) -- most entrants (incl. most non-XGBoost models) never beat a day-of-year climatology baseline",
    "RVF early warning (climate/NDVI)",       "Lead time",            NA_real_, "Anyamba et al. 2009 PNAS 106(3) -- qualitative 2-6 week lead time ahead of the 2006-07 Horn of Africa outbreak, predates routine AUC-style reporting",
    "AUC interpretation convention",          "reference scale",      NA_real_, "Mandrekar 2010 J Thorac Oncol -- 0.7-0.8 acceptable/0.8-0.9 excellent/>0.9 outstanding, but calibrated for roughly-balanced diagnostic tests, NOT rare-event classifiers",
    "PR-AUC no-skill floor",                  "reference",            overall_prevalence, "Saito & Rehmsmeier 2015 PLOS ONE -- a random classifier's PR-AUC equals the positive-class prevalence, not 0.5; judge our PR-AUC against THIS floor, not 0.5"
  )
  results$benchmark_table <- benchmark_table
  
  ## That being said, compare AUC-style numbers
  gg_benchmark <- benchmark_table |>
    filter(metric == "ROC-AUC") |>
    mutate(is_ours = str_detect(system, "^This model")) |>
    ggplot(aes(x = value, y = reorder(system, value), colour = is_ours)) +
    geom_point(size = 3) +
    scale_colour_manual(values = c(`TRUE` = "firebrick3", `FALSE` = "grey30"), guide = "none") +
    labs(x = "ROC-AUC", y = NULL
         , title = "ROC-AUC vs. published forecasting systems"
         , subtitle = "Different diseases/base rates -- directional context only, see benchmark_table for caveats") +
    theme_bw()
  results$gg_benchmark <- gg_benchmark
  
  
  #### 3) Spatial diagnostics (hex / region / country / macro-region) ----------
  
  ## Fine hex-level: mean log loss and mean predicted probability per hex, pooled
  ## across the whole record. Log loss used here given that with ~0.1% prevalence 
  ## spread over 2479 hexes, few hexes have any outbreaks (or more than 1)
  hex_perf <- all_preds |>
    filter(aggregation == "No aggregation") |>
    group_by(shapeName) |>
    summarize(
      n           = n()
      , n_pos       = sum(true_out == 1)
      , mean_logloss = mean(-(true_out * log(pmax(prob_pred, 1e-15)) + (1 - true_out) * log(pmax(1 - prob_pred, 1e-15))))
      , mean_prob   = mean(prob_pred)
      , .groups = "drop")
  results$hex_perf <- hex_perf
  
  gg_hex_logloss <- hex_fine |>
    left_join(hex_perf, by = "shapeName") |>
    ggplot() +
    geom_sf(aes(fill = mean_logloss), colour = NA) +
    scale_fill_viridis_c(name = "Mean log loss", direction = -1, na.value = "grey90") +
    coord_sf() + theme_void() +
    labs(title = "Per-hex mean log loss (lower = better)")
  results$gg_hex_logloss <- gg_hex_logloss
  
  gg_hex_outbreaks <- hex_fine |>
    left_join(hex_perf, by = "shapeName") |>
    ggplot() +
    geom_sf(aes(fill = n_pos), colour = NA) +
    scale_fill_viridis_c(name = "True outbreaks\n(count)", trans = "sqrt", na.value = "grey90") +
    coord_sf() + theme_void() +
    labs(title = "Where the true outbreaks actually are")
  results$gg_hex_outbreaks <- gg_hex_outbreaks
  
  ## Coarser region_norm level (using the "Double aggregation" rows, which pool 
  ## across space and time) has more observations per spatial unit
  region_perf <- all_preds |>
    filter(aggregation == "Double aggregation") |>
    group_by(region_norm) |>
    group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    ungroup() |>
    left_join(region_to_country, by = "region_norm") |>
    left_join(enso_sensitivity, by = "Country")
  results$region_perf <- region_perf
  
  gg_region_auc <- hex_coarse |>
    rename(region_norm = shapeName) |>
    left_join(region_perf, by = "region_norm") |>
    ggplot() +
    geom_sf(aes(fill = roc_auc), colour = NA) +
    scale_fill_viridis_c(name = "ROC-AUC", na.value = "grey90", limits = c(0.5, 1), oob = scales::squish) +
    coord_sf() + theme_void() +
    labs(title = paste0("Regional ROC-AUC (Double aggregation, n_pos >= ", min_events_for_auc, ")"))
  results$gg_region_auc <- gg_region_auc
  
  ## Country-level ranking: join Country onto the fine hex-level predictions 
  ## and score per country
  country_perf <- all_preds |>
    filter(aggregation == "No aggregation") |>
    left_join(shape_to_country, by = "shapeName") |>
    filter(!is.na(Country)) |>
    group_by(Country) |>
    group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    ungroup() |>
    left_join(enso_sensitivity, by = "Country") |>
    arrange(desc(n_pos))
  results$country_perf <- country_perf
  
  results$gg_country_avgp_vs_logloss <- ggplot(country_perf, aes(average_p, logloss, label = country_name)) +
    geom_point(aes(size = prevalence)) +
    xlab("Average Predicted Probability") +
    ylab("Log Loss") +
    geom_text(vjust = -0.7, size = 3) +
    scale_y_log10() +
    scale_x_log10()
  
  results$gg_country_prevalence_vs_logloss <- ggplot(country_perf, aes(prevalence, logloss, label = country_name)) +
    geom_point() +
    xlab("Average Predicted Probability") +
    ylab("Log Loss") +
    geom_text(vjust = -0.7, size = 3) +
    scale_x_log10()
  
  gg_country_map <- africa_countries |>
    left_join(country_perf, by = c("iso_a3" = "Country")) |>
    ggplot() +
    geom_sf(aes(fill = logloss), colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(name = "Mean log loss", direction = -1, na.value = "grey90") +
    theme_void() +
    labs(title = "Country-level mean log loss")
  results$gg_country_map <- gg_country_map
  
  gg_country_ranking <- country_perf |>
    filter(n_pos >= min_events_for_auc) |>
    ggplot(aes(x = roc_auc, y = reorder(Country, roc_auc), colour = enso_rvf_sensitivity)) +
    geom_point(size = 2) +
    labs(x = "ROC-AUC", y = NULL, colour = "Literature ENSO-RVF\nsensitivity (Anyamba 2009)"
         , title = paste0("Country ranking by ROC-AUC (n_pos >= ", min_events_for_auc, " only)")) +
    theme_bw()
  results$gg_country_ranking <- gg_country_ranking
  
  ## Macro-region rollup: is the model systematically better/worse in the
  ## literature-flagged high-ENSO-sensitivity Horn-of-Africa/East-Africa countries?
  macro_region_perf <- all_preds |>
    filter(aggregation == "No aggregation") |>
    left_join(shape_to_country, by = "shapeName") |>
    left_join(enso_sensitivity, by = "Country") |>
    filter(!is.na(macro_region)) |>
    group_by(macro_region, enso_rvf_sensitivity) |>
    group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    ungroup() |>
    arrange(desc(n_pos))
  results$macro_region_perf <- macro_region_perf
  
  ## Positive vs. negative predicted-probability country comparison 
  ## scores each country separately on just its true-outbreak rows and just its 
  ## true-non-outbreak rows, then compares the average predicted probability between 
  ## the two 
  country_perf_pos <- all_preds |>
    filter(aggregation == "No aggregation", true_out == 1) |>
    left_join(shape_to_country, by = "shapeName") |>
    filter(!is.na(Country)) |>
    group_by(Country) |>
    group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    ungroup() |>
    left_join(enso_sensitivity, by = "Country") |>
    arrange(desc(n_pos))
  results$country_perf_pos <- country_perf_pos
  
  country_perf_neg <- all_preds |>
    filter(aggregation == "No aggregation", true_out == 0) |>
    left_join(shape_to_country, by = "shapeName") |>
    filter(!is.na(Country)) |>
    group_by(Country) |>
    group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    ungroup() |>
    left_join(enso_sensitivity, by = "Country") |>
    arrange(desc(n_pos))
  results$country_perf_neg <- country_perf_neg
  
  pos_neg_ratio <- country_perf_pos |>
    dplyr::select(country_name, Country, average_p) |>
    rename(p_pos = average_p) |>
    left_join(
      country_perf_neg |>
        dplyr::select(country_name, Country, average_p) |>
        rename(p_neg = average_p)
      , by = c("country_name", "Country")
    ) |>
    mutate(pos_neg_ratio = p_pos / p_neg) |>
    arrange(desc(pos_neg_ratio))
  results$pos_neg_ratio <- pos_neg_ratio
  
  results$gg_country_pos_neg_ratio_map <- africa_countries |>
    left_join(pos_neg_ratio, by = c("iso_a3" = "Country")) |>
    ggplot() +
    geom_sf(aes(fill = pos_neg_ratio), colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(name = "Pos/Neg Ratio", direction = -1, na.value = "grey90") +
    theme_void() +
    labs(title = "Ratio of average predicted P: true positives vs. true negatives, by country")
  
  results$gg_country_avg_p_pos_map <- africa_countries |>
    left_join(country_perf_pos, by = c("iso_a3" = "Country")) |>
    ggplot() +
    geom_sf(aes(fill = average_p), colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(name = "average_p", direction = -1, na.value = "grey90") +
    theme_void() +
    labs(title = "Average predicted P on true-positive rows, by country")
  
  results$gg_country_avg_p_neg_map <- africa_countries |>
    left_join(country_perf_neg, by = c("iso_a3" = "Country")) |>
    ggplot() +
    geom_sf(aes(fill = average_p), colour = "white", linewidth = 0.1) +
    scale_fill_viridis_c(name = "average_p", direction = -1, na.value = "grey90") +
    theme_void() +
    labs(title = "Average predicted P on true-negative rows, by country")
  
  
  #### Check covariate stack in periods in Mauritania that have cases vs those periods
   ## that do not have cases
  all_preds |> filter(Country == "MRT") %>% 
    group_by(date) %>% summarize(mp = mean(prob_pred)) %>% 
    mutate(month = month(date)) %>% 
    arrange(desc(mp))
    ## months 5, 6, 7, 8
  MRT_shapes <- all_preds |> filter(Country == "MRT") %>%
    mutate(month = month(date)) %>% 
    filter(month %in% c(5, 6, 7, 8)) %>%
    group_by(shapeName) %>%
    summarize(mm = mean(prob_pred)) %>% 
    arrange(desc(mm)) %>%
    dplyr::slice(1:20) %>%
    pull(shapeName)
  
  MRT_data <- all_preds %>%
    mutate(month = month(date)) %>% 
    filter(
      month %in% c(5, 6, 7, 8)
    , shapeName %in% MRT_shapes
    ) %>% left_join(test_data)
  
  MRT_data %>% dplyr::select(
    outbreak, contains("anomaly_forecast")
  ) %>% mutate(outbreak = as.factor(outbreak)) %>% {
    ggpairs(., aes(colour = outbreak)) +
      scale_colour_brewer(palette = "Dark2") +
      theme(
        strip.text.x = element_text(size = 10)
      , axis.text.x  = element_text(size = 8))
  }
  
  MRT_data %>% dplyr::select(
    outbreak, contains("anomaly_scaled")
  ) %>% mutate(outbreak = as.factor(outbreak)) %>% {
    ggpairs(., aes(colour = outbreak)) +
      scale_colour_brewer(palette = "Dark2") +
      theme(
        strip.text.x = element_text(size = 10)
      , axis.text.x  = element_text(size = 8))
  }
  
  ZAF_shapes <- all_preds |> filter(Country == "ZAF") %>%
    mutate(month = month(date)) %>% 
    filter(month %in% c(10, 11, 12, 1, 2)) %>%
    group_by(shapeName) %>%
    summarize(mm = mean(prob_pred)) %>% 
    arrange(desc(mm)) %>%
    dplyr::slice(1:20) %>%
    pull(shapeName)
  
  ZAF_data <- all_preds %>%
    mutate(month = month(date)) %>% 
    filter(
      month %in% c(10, 11, 12, 1, 2)
      , shapeName %in% ZAF_shapes
    ) %>% left_join(test_data)
  
  ZAF_data %>% dplyr::select(
    outbreak, contains("anomaly_forecast")
  ) %>% mutate(outbreak = as.factor(outbreak)) %>% {
    ggpairs(., aes(colour = outbreak)) +
      scale_colour_brewer(palette = "Dark2") +
      theme(
        strip.text.x = element_text(size = 10)
      , axis.text.x  = element_text(size = 8))
  }
  
  ZAF_data %>% dplyr::select(
    outbreak, contains("anomaly_scaled")
  ) %>% mutate(outbreak = as.factor(outbreak)) %>% {
    ggpairs(., aes(colour = outbreak)) +
      scale_colour_brewer(palette = "Dark2") +
      theme(
        strip.text.x = element_text(size = 10)
      , axis.text.x  = element_text(size = 8))
  }
  
  
  #### 4) ENSO / climate-phase coupling -----------------------------------------
  
  ## Data-driven country ENSO-impact ranking (temperature/precipitation anomaly
  ## correlation with ONI); complement to the literature-based enso_sensitivity_lookup()
  ## table above. 
  enso_impact_result <- compute_enso_country_impact_ranking(enso_data_dir, correlation_method)
  
  if (!is.null(enso_impact_result)) {
    
    results$enso_climate_impact_ranking <- enso_impact_result$ranking
    results$gg_enso_impact_ranking      <- enso_impact_result$gg_ranking
    results$gg_enso_impact_map          <- enso_impact_result$gg_map
    
    enso_impact_by_country <- enso_impact_result$ranking |>
      rename(Country = country) |>
      dplyr::select(rank, Country, combined_impact) |>
      rename(ENSO_impact_rank = rank)
    
    results$gg_country_logloss_vs_enso_impact <- country_perf |>
      left_join(enso_impact_by_country, by = "Country") |>
      ggplot(aes(x = combined_impact, y = logloss, label = country_name)) +
      geom_point() +
      scale_y_log10() +
      xlab("ENSO Impact") +
      ylab("LogLoss") +
      geom_text(vjust = -0.7, size = 3)
    
    results$gg_country_logloss_rank_vs_enso_impact_rank <- country_perf |>
      left_join(enso_impact_by_country, by = "Country") |>
      arrange(desc(logloss)) |>
      mutate(LogLoss_rank = seq(n())) |>
      ggplot(aes(x = ENSO_impact_rank, y = LogLoss_rank, label = country_name)) +
      geom_point() +
      xlab("ENSO Impact Rank") +
      ylab("LogLoss Rank") +
      geom_text(vjust = -0.7, size = 3)
    
  } else {
    
    message("Skipping the ENSO country-impact comparison plots -- see ",
            "compute_enso_country_impact_ranking() for the required data/ENSO/ files.")
    
  }
  
  oni_tbl <- fetch_oni()
  
  if (!is.null(oni_tbl)) {
    
    preds_with_enso <- all_preds |>
      filter(aggregation == "No aggregation") |>
      mutate(year = year(date), month = month(date)) |>
      left_join(oni_tbl, by = c("year", "month")) |>
      left_join(shape_to_country, by = "shapeName") |>
      left_join(enso_sensitivity, by = "Country") |>
      filter(!is.na(enso_phase))
    
    ## Does performance differ by ENSO phase, and does that difference concentrate in
    ## the literature-flagged high-sensitivity countries (the interaction is the
    ## actually-interesting test here, not the marginal phase effect alone)
    enso_phase_perf <- preds_with_enso |>
      group_by(enso_phase, enso_rvf_sensitivity) |>
      group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
      ungroup()
    results$enso_phase_perf <- enso_phase_perf
    
    gg_enso_perf <- enso_phase_perf |>
      filter(!is.na(enso_rvf_sensitivity)) |>
      ggplot(aes(enso_phase, logloss, fill = enso_phase)) +
      geom_col() +
      facet_wrap(~enso_rvf_sensitivity) +
      labs(x = NULL, y = "Mean log loss"
           , title = "Performance by ENSO phase x literature ENSO-RVF sensitivity"
           , subtitle = "ENSO phase from NOAA ONI (single-season >=0.5/<=-0.5 threshold, a simplification -- see fetch_oni())") +
      theme_bw() + theme(legend.position = "none")
    results$gg_enso_perf <- gg_enso_perf
    
    ## Do the covariates that actually matter to the model (Section E's top features)
    ## shift systematically with ENSO phase? Sampled for plotting speed.
    covariate_by_enso <- test_data |>
      mutate(year = year(date), month = month(date)) |>
      left_join(oni_tbl, by = c("year", "month")) |>
      filter(!is.na(enso_phase))
    ## n() only works inside a data-masking verb (mutate/filter/summarize), not as a
    ## bare argument value -- compute the row count first, then cap the sample size
    covariate_by_enso <- covariate_by_enso |> slice_sample(n = min(50000, nrow(covariate_by_enso)))
    
    gg_covariate_by_enso <- covariate_by_enso |>
      dplyr::select(enso_phase, anomaly_scaled_precipitation_90, anomaly_forecast_scaled_relative_humidity, Mean_Temperature_of_Coldest_Quarter) |>
      pivot_longer(-enso_phase, names_to = "covariate", values_to = "value") |>
      ggplot(aes(enso_phase, value, fill = enso_phase)) +
      geom_boxplot(outlier.alpha = 0.1) +
      facet_wrap(~covariate, scales = "free_y") +
      labs(x = NULL, y = NULL, title = "Key covariates by ENSO phase") +
      theme_bw() + theme(legend.position = "none")
    results$gg_covariate_by_enso <- gg_covariate_by_enso
    
  } else {
    message("Section C ONI-phase diagnostics skipped (no ONI data available -- see message above).")
  }
  
  
  #### 5) Temporal dynamics -----------------------------------------------------
  
  ## Day-of-year seasonal cycle by year, for the countries with the most recorded outbreaks 
  top_countries_by_outbreaks <- country_perf |> slice_max(n_pos, n = 6) |> pull(Country)
  
  seasonal_data <- all_preds |>
    filter(aggregation == "No aggregation", forecast_interval == 30) |>
    left_join(shape_to_country, by = "shapeName") |>
    filter(Country %in% top_countries_by_outbreaks) |>
    mutate(doy = yday(date), year = factor(year(date)))
  
  gg_seasonal_cycle <- seasonal_data |>
    group_by(Country, doy, year) |>
    summarize(prob_pred = mean(prob_pred), true_out = max(true_out), .groups = "drop") |>
    ggplot(aes(doy, prob_pred, colour = year)) +
    geom_line(aes(group = year), alpha = 0.6) +
    geom_vline(data = ~filter(.x, true_out == 1), aes(xintercept = doy, colour = year), linetype = "dashed") +
    facet_wrap(~Country, scales = "free_y") +
    scale_colour_brewer(palette = "Dark2") +
    labs(x = "Day of year", y = "Mean predicted probability"
         , title = "Seasonal cycle by country, dashed lines = true outbreak days") +
    theme_bw()
  results$gg_seasonal_cycle <- gg_seasonal_cycle
  
  ## STL decomposition + dominant-cycle-length per top country: splits each country's
  ## monthly mean predicted-probability series into trend / yearly-seasonal / remainder,
  ## and the periodogram's peak period says whether that country's dynamics read as
  ## "strongly annual" (peak ~12 months), "longer-cycle" (peak well above 12), or
  ## "chaotic/no dominant cycle" (flat, broadband spectrum, no clear peak)
  monthly_by_country <- all_preds |>
    filter(aggregation == "No aggregation", forecast_interval == 30) |>
    left_join(shape_to_country, by = "shapeName") |>
    filter(Country %in% top_countries_by_outbreaks) |>
    mutate(ym = floor_date(date, "month")) |>
    group_by(Country, ym) |>
    summarize(prob_pred = mean(prob_pred), .groups = "drop") |>
    arrange(Country, ym)
  
  decomp_results <- map(top_countries_by_outbreaks, function(cty) {
    
    cty_tbl <- monthly_by_country |> filter(Country == cty)
    if (nrow(cty_tbl) < 24) return(NULL)
    d <- decompose_series(cty_tbl, "prob_pred", year(min(cty_tbl$ym)), month(min(cty_tbl$ym)))
    if (is.null(d)) return(NULL)
    list(country = cty, decomp = d)
    
  }) |> purrr::compact()
  
  cycle_summary <- map_dfr(decomp_results, function(x) {
    
    tibble(
      Country = x$country
      , peak_period_months = x$decomp$peak_period_months
      , dynamics_read_as = case_when(
        x$decomp$peak_period_months >= 10 & x$decomp$peak_period_months <= 14 ~ "strong yearly cycle"
        , x$decomp$peak_period_months > 14                                       ~ "longer-than-yearly cycle"
        , TRUE                                                                    ~ "short/chaotic-looking dominant frequency"
      )
    )
    
  })
  
  results$cycle_summary <- cycle_summary
  
  if (length(decomp_results) > 0) {
    
    results$gg_stl_examples <- map(decomp_results[seq_len(min(2, length(decomp_results)))], function(x) {
      as_tibble(x$decomp$stl$time.series) |>
        mutate(ym = monthly_by_country |> filter(Country == x$country) |> pull(ym)) |>
        pivot_longer(c(seasonal, trend, remainder), names_to = "component", values_to = "value") |>
        ggplot(aes(ym, value)) + geom_line() + facet_wrap(~component, ncol = 1, scales = "free_y") +
        labs(x = NULL, y = NULL, title = paste0(x$country, " -- STL decomposition (peak cycle ~"
                                                , round(x$decomp$peak_period_months, 1), " months)")) +
        theme_bw()
    })
    
  }
  
  ## Rolling quarterly performance over time 
  rolling_perf <- all_preds |>
    filter(aggregation == "No aggregation") |>
    mutate(window = floor_date(date, "3 months")) |>
    group_by(window) |>
    group_modify(~ score_group(.x, min_events_for_auc = min_events_for_auc)) |>
    ungroup()
  results$rolling_perf <- rolling_perf
  
  gg_rolling_perf <- rolling_perf |>
    filter(n_pos >= min_events_for_auc) |>
    ggplot(aes(window, logloss)) +
    geom_line() + geom_point(aes(size = n_pos)) +
    labs(x = "Quarter", y = "Mean log loss", size = "True outbreaks\nin window"
         , title = "Rolling quarterly performance over time") +
    theme_bw()
  results$gg_rolling_perf <- gg_rolling_perf
  
  
  #### 6) What's driving predictions (variable importance + SHAP) --------------
  
  ## Gain-based importance and vip's version of the same on the parsnip fit 
  xgb_importance         <- xgboost::xgb.importance(model = booster, feature_names = x_names)
  vip_importance         <- vip::vi(parsnip_fit)
  results$xgb_importance <- xgb_importance
  results$vip_importance <- vip_importance
  
  gg_importance <- xgb_importance |>
    slice_max(Gain, n = 25) |>
    ggplot(aes(Gain, reorder(Feature, Gain))) +
    geom_col(fill = "steelblue") +
    labs(x = "Gain", y = NULL, title = "Top 25 features by xgboost Gain") +
    theme_bw()
  results$gg_importance <- gg_importance
  
  ## SHAP: bake a sample of test_data through this fold's recipe, then use 
  ## xgboost::predcontrib 
  shap_sample <- test_data |> slice_sample(n = shap_sample_n)
  baked_shap  <- recipes::bake(fold_recipe, new_data = shap_sample) |>
    dplyr::select(all_of(x_names))
  
  shap_long         <- compute_shap_long(booster, baked_shap)
  results$shap_long <- shap_long
  
  shap_feature_order <- shap_long |>
    group_by(feature) |>
    summarize(mean_abs_shap = mean(abs(shap_value)), .groups = "drop") |>
    slice_max(mean_abs_shap, n = 20)
  results$shap_feature_order <- shap_feature_order
  
  gg_shap_summary <- shap_feature_order |>
    ggplot(aes(mean_abs_shap, reorder(feature, mean_abs_shap))) +
    geom_col(fill = "darkorange") +
    labs(x = "Mean |SHAP value|", y = NULL, title = "Global SHAP importance (top 20)") +
    theme_bw()
  results$gg_shap_summary <- gg_shap_summary
  
  ## Beeswarm-style SHAP distribution
  ## one row per observation, x = SHAP value, y = feature ranked by importance, 
  ## colour = the observation's rescaled feature value 
  gg_shap_beeswarm <- shap_long |>
    filter(feature %in% shap_feature_order$feature) |>
    left_join(shap_feature_order, by = "feature") |>
    group_by(feature) |>
    mutate(feature_value_scaled = scales::rescale(feature_value, to = c(0, 1))) |>
    ungroup() |>
    ggplot(aes(shap_value, forcats::fct_reorder(feature, mean_abs_shap))) +
    geom_jitter(aes(colour = feature_value_scaled), height = 0.3, alpha = 0.5, size = 0.8) +
    scale_colour_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 0.5
                           , name = "Feature value\n(low -> high)") +
    labs(x = "SHAP value", y = NULL, title = "SHAP value distribution (top 20 features)") +
    theme_bw()
  
  results$gg_shap_beeswarm <- gg_shap_beeswarm
  
  ## SHAP dependence plot for the single top feature (shape of effect)
  top_feature <- shap_feature_order$feature[1]
  gg_shap_dependence <- shap_long |>
    filter(feature == top_feature) |>
    ggplot(aes(feature_value, shap_value)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = FALSE, colour = "firebrick3") +
    labs(x = top_feature, y = "SHAP value"
         , title = paste0("SHAP dependence: ", top_feature, " (top feature by mean |SHAP|)")) +
    theme_bw()
  results$gg_shap_dependence <- gg_shap_dependence
  
  ## Simple partial dependence for the same top feature, using the pdp package
  pdp_pred_fun <- function(object, newdata) predict(object, new_data = newdata, type = "prob")$.pred_1
  
  pdp_result <- tryCatch(
    pdp::partial(
      object = parsnip_fit, pred.var = top_feature, pred.fun = pdp_pred_fun
      , train = baked_shap, grid.resolution = pdp_grid_res
    )
    , error = function(e) { message("PDP failed: ", conditionMessage(e)); NULL }
  )
  
  if (!is.null(pdp_result)) {
    results$pdp_result <- pdp_result
    results$gg_pdp <- pdp_result |>
      group_by(across(all_of(top_feature))) |>
      summarize(yhat = mean(yhat), .groups = "drop") |>
      ggplot(aes(.data[[top_feature]], yhat)) + geom_line() +
      labs(y = "Mean predicted P(outbreak)", title = paste0("Partial dependence: ", top_feature)) +
      theme_bw()
  }
  
  
  #### 7) Tree structure visuals -------------------------------------------------
  
  ## lorax::as.party() needs the outcome column included; re bake with this column
  baked_for_tree <- recipes::bake(fold_recipe, new_data = shap_sample) |> as.data.frame()
  
  results$tree_diagram <- render_one_tree(booster, tree_number = 1, data_for_party = baked_for_tree)
  
  ## Aggregate tree-shape stats across a sample of trees
  n_trees_to_sample <- 50
  tree_stats        <- xgboost::xgb.model.dt.tree(model = booster, trees = n_trees_to_sample)
  
  tree_shape_summary <- tree_stats |>
    group_by(Tree) |>
    summarize(
      n_nodes = n()
      , n_leaves = sum(Feature == "Leaf")
      , max_depth = max(str_count(ID, "-"))
      , .groups = "drop"
    )
  
  results$tree_shape_summary <- tree_shape_summary
  
  ## Which features get picked as the very first split most often
  first_split_features <- tree_stats |>
    filter(Node == 0) |>
    count(Feature, sort = TRUE)
  
  results$first_split_features <- first_split_features
  
  gg_first_splits <- first_split_features |>
    slice_max(n, n = 15) |>
    ggplot(aes(n, reorder(Feature, n))) +
    geom_col(fill = "seagreen") +
    labs(x = paste0("Times chosen as root split (of first ", n_trees_to_sample, " trees)"), y = NULL
         , title = "Most common root-node (first-split) features") +
    theme_bw()
  
  results$gg_first_splits <- gg_first_splits
  
  
  #### 8) Save everything -----------------------------------------------------------------
  
  bundle_path <- paste0(out_dir, ".qs")
  qs::qsave(results, bundle_path)
  bundle_path
  
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
  
  shap_mat <- predict(booster, as.matrix(baked_x), predcontrib = TRUE)
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

