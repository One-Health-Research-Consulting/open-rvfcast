#' Comprehensive performance evaluation of RVF outbreak forecasts
#'
#' Evaluation of both hex-level predictions and larger regions.
#' 
#' Sections include:
#'  - hex:             forecast-focused scores (evaluation from the perspective of the date from 
#'                     which forecasts are made) by window, calibration curves, rank-based
#'                     alerts, threshold tables, probability summaries, densities and maps
#'  - rare_event:      rare-event verification (information gain, CORP, N-test, Molchan,
#'                     economic value, Murphy, block-bootstrap CIs) against flat and a
#'                     historical hex -by- calendar-month reference rate
#'  - outbreak:        outbreak-focused skill (evaluation from the perspective of the oubtreak
#'                     date) with hex outbreak episodes as the unit
#'                     (episode-weighted AUC, lead-time rank curves, earliest warning, first
#'                     vs follow-on outbreaks, missed episodes)
#'  - regional:        up-scaling to regions (aggregated ADM2s) with a fitted Gaussian copula
#'                     and alternative rules, scored out of sample
#'  - cumulative:      any outbreak within the whole forecast horizon (days 1-149), for hexes
#'                     and regions, with the window probabilities combined under independence
#'                     (upper bound) and perfect dependence (lower bound)
#'  - spatial, enso, temporal, model_structure: misc extra diagnostics to assess performance
#'  - variable_importance: gain importance and partial dependence across all outer-fold models
#'                     across space and time
#'                     
#' Two evaluation sets are scored when available: "test" (the held-out test-period
#' predictions) and "outer_folds" (each outer fold's refit predicting its own held-out 
#' time period, calibrated with coefficients fit on the other folds; see harvest_outer_fold_predictions
#' and fit_probability_calibration_on_outer_folds in model_framework_targets.R). 
#' The outer-fold set is used to add many more outbreak seasons for determining the parameters
#' of the copula for up-scaling predictions to a larger spatial area.
#' 
#' A per-hex and a per-region context table (forecast skill, quality class, outbreak history)
#' mask the test-period maps, and are saved with the regional copula fit (context_dir) for
#' masking the forward-forecast maps of PURPOSE = forecast runs (run_forecast_context_maps()).
#'
#' @title run_comprehensive_performance
#' @param predictions_path Saved test-period predictions; the ex_fits.all_probs_raw target
#' @param hexed_outbreaks_path Outbreaks placed in their labelled hexes; saved by
#'   export_hexed_outbreaks() (data/rvf_outbreaks_hexed.Rds)
#' @param train_data The train_data target
#' @param test_data The test_data target 
#' @param region_hexes The region_hexes target
#' @param forecast_horizon Upper bounds (days) of the consecutive forecast windows (the
#'   forecast_horizon target); the first window starts at day 1
#' @param regions_path Regions to up-scale to, e.g.data/upscaling_regions/reduced_pan_regions_target60000km2.Rds
#' @param harvest_dir Folder of calibration-harvest files (outer-fold predictions); NULL to skip
#' @param fitted_model The fitted_model target (file paths), for the model-structure section
#' @param variable_importance The variable_importance target, for the variable-importance section
#' @param out_root Results go to out_root/<run_date>
#' @param run_date Date used to name the output folder
#' @param sections Sections to run
#' @param alert_fractions Alert fractions (top share of hexes flagged per base date and window)
#' @param p_thresh Probability thresholds for the confusion counts
#' @param min_events_for_auc Minimum events before AUCs are computed for a group
#' @param n_boot Space-time block-bootstrap replicates for the rare-event scores
#' @param n_boot_corp Resamples for the CORP reliability bands
#' @param n_boot_episodes Bootstrap resamples of outbreak episodes
#' @param copula_args Named list of extra arguments for evaluate_regional_copula()
#' @param n_map_dates Number of base dates mapped (dates with the most outbreaks + latest)
#' @param enso_data_dir Input for the ENSO country-impact ranking
#' @param correlation_method Input for the ENSO country-impact ranking
#' @param chosen_outer_fold Which fitted model the model-structure section uses
#' @param chosen_purpose_tag Which fitted model the model-structure section uses
#' @param shap_sample_n SHAP sample size
#' @param pdp_grid_res SHAP partial-dependence grid resolution
#' @param context_dir Folder the hex / region mask context and copula fit are saved to for
#'   masking forward-forecast maps (run_forecast_context_maps()); NULL to skip (PURPOSE = forecast)
#' @return Path to the output folder, which holds tables/ (csv), figures/ (png), data/
#'   (qs data objects), results.qs (all tables) and report.md / report.html
#' @author Morgan Kain
#' @export
run_comprehensive_performance <- function(
    predictions_path
  , hexed_outbreaks_path
  , train_data
  , test_data
  , region_hexes
  , forecast_horizon   = c(30, 60, 90, 120, 150)
  , regions_path       = "data/upscaling_regions/reduced_pan_regions_target60000km2.Rds"
  , harvest_dir        = "outputs/calibration_harvest"
  , fitted_model       = NULL
  , variable_importance = NULL
  , out_root           = "outputs/comprehensive_performance"
  , run_date           = Sys.Date()
  , sections           = c("hex", "rare_event", "outbreak", "regional", "cumulative", "spatial", "enso", "temporal", "model_structure", "variable_importance")
  , alert_fractions    = c(0.01, 0.05, 0.10)
  , p_thresh           = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
  , min_events_for_auc = 5
  , n_boot             = 200
  , n_boot_corp        = 50
  , n_boot_episodes    = 1000
  , copula_args        = list()
  , n_map_dates        = 4
  , enso_data_dir      = here::here("data/ENSO")
  , correlation_method = "spearman"
  , chosen_outer_fold  = 1
  , chosen_purpose_tag = "for_test_data"
  , shap_sample_n      = 3000
  , pdp_grid_res       = 25
  , context_dir        = "outputs/forecast_context"
) {

  ##########################################################################################
  #### 0) Output folders -------------------------------------------------------------------
  ##########################################################################################

  out_dir <- file.path(out_root, as.character(run_date))
  for (d in c("tables", "figures", "data")) dir.create(file.path(out_dir, d), showWarnings = FALSE, recursive = TRUE)

  ## Collected outputs: tables (saved as csv), plots (saved as png) and section errors
  results <- list(tables = list(), plots = list(), errors = list(), meta = list())
  
  ## Run one section of performance metrics, keeping its outputs, or record its error and carry on
  run_section <- function(name, expr) {
    if (!name %in% sections) return(invisible(NULL))
    message("Running section: ", name)
    out <- tryCatch(expr, error = function(e) {
      results$errors[[name]] <<- conditionMessage(e)
      message("Section '", name, "' failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(out)) {
      results$tables[[name]] <<- out$tables
      results$plots[[name]]  <<- out$plots
    }
    invisible(out)
  }

  
  ##########################################################################################
  #### 1) Inputs: predictions linked to outbreaks, reference rates, hex info ---------------
  ##########################################################################################

  forecast_windows <- make_forecast_windows(forecast_horizon)
 
  ## Load previously saved outbreak details
  hexed_outbreaks  <- readRDS(hexed_outbreaks_path)

  ## Test-period predictions linked to outbreaks
  linked <- list(test = link_predictions_to_outbreaks(predictions_path, hexed_outbreaks, forecast_intervals = c(1, forecast_horizon)))

  ## Outer-fold (calibration harvest) predictions, calibrated with coefficients fit on the
   ## other folds, linked the same way as above
   ## That is, join outbreaks in the training period with training predictions, which will
    ## be used in the calculation of the copula for upscaling
  harvest_preds <- read_harvest_predictions(harvest_dir)
  if (!is.null(harvest_preds)) {
    linked$outer_folds <- suppressWarnings(link_predictions_to_outbreaks(harvest_preds, hexed_outbreaks, forecast_intervals = c(1, forecast_horizon)))
  }

  ## Assign a country label to each hex based on which country has the largest land
   ## area in that hex, as well as get the lat lon centroid for each hex
  hex_info <- build_hex_info(train_data, test_data, region_hexes)

  ## Historical hex -by- calendar-month reference rates from the training period,
   ## hex's overall rate, and overall training rate
  reference_rates <- build_reference_rates(train_data)
  overall_rate    <- mean(as.numeric(as.character(train_data$outbreak)))
  hex_rates       <- reference_rates |> 
    dplyr::summarize(hex_rate = mean(ref_prob), .by = c(shapeName, forecast_interval))

  ## All evaluation sets joined, hex-level rows only
  hex_preds <- purrr::imap_dfr(linked, ~ .x$predictions |> dplyr::mutate(eval_set = .y, .before = 1)) |>
    dplyr::mutate(month = lubridate::month(date)) |>
    dplyr::left_join(reference_rates, by = c("shapeName", "month", "forecast_interval")) |>
    dplyr::left_join(hex_rates, by = c("shapeName", "forecast_interval")) |>
    dplyr::mutate(ref_prob = dplyr::coalesce(ref_prob, hex_rate, overall_rate)) |>
    dplyr::select(-hex_rate, -month)

  ## Just test period
  test_preds <- hex_preds |> dplyr::filter(eval_set == "test")

  ## Base dates to map: dates with the most outbreak rows in the first window, plus the latest
  map_dates <- test_preds |>
    dplyr::filter(forecast_interval == min(forecast_interval)) |>
    dplyr::summarize(n_out = sum(true_out), .by = date) |>
    dplyr::slice_max(n_out, n = n_map_dates - 1, with_ties = FALSE) |>
    dplyr::pull(date) |>
    c(max(test_preds$date)) |>
    unique() |>
    sort()

  ## Save some meta-data like attributes for this run
  results$meta <- list(
    run_date        = run_date
  , predictions     = predictions_path
  , eval_sets       = hex_preds |>
      dplyr::summarize(
        n_rows = dplyr::n(), first_base_date = min(date), last_base_date = max(date)
      , n_base_dates = dplyr::n_distinct(date), n_hexes = dplyr::n_distinct(shapeName)
      , n_outbreak_rows = sum(true_out), .by = eval_set)
  , label_checks    = purrr::imap_dfr(linked, ~ .x$label_check |> dplyr::mutate(eval_set = .y, .before = 1))
  , map_dates       = map_dates
  , alert_fractions = alert_fractions)

  ## Per-hex context (mask skill from the outer folds first, test-period summaries, outbreak
   ## history), and the test predictions with context columns
  context <- build_hex_context(hex_preds, hexed_outbreaks, hex_info, history_end = min(test_preds$date) - 1)
  
  ## Add context to overall results list
  results$tables$context <- list(hex_context = context$hex_context)
  
  ## Save context and linked events to hexes as unique .qs objects
  qs::qsave(context$hex_preds, file.path(out_dir, "data", "test_predictions_with_context.qs"))
  qs::qsave(linked, file.path(out_dir, "data", "linked_predictions.qs"))

  
  ##########################################################################################
  #### 2) Hex-level forecast-focused scores ------------------------------------------------
  ##########################################################################################

  run_section("hex", {
    
    hex_scores <- evaluate_hex_forecasts(hex_preds, p_thresh, alert_fractions, min_events_for_auc)
    hex_plots  <- plot_hex_forecasts(hex_scores, hex_preds, "test")

    ## Calibration curves (optimised and even bins) on the test set
    calcurves <- generate_calibration_curve(
      preds     = test_preds
    , test_data = NULL
    , predname  = "prob_pred"
    , truename  = "true_out"
    , splitgrp  = "forecast_interval")
    
    plotted_calibration        <- plot_calibration(caltib = calcurves, xg = NULL, yg = "forecast_interval", forcastvals = c(30, 90, 150))
    hex_plots$calibration_opt  <- plotted_calibration$calplot.opt[[1]]
    hex_plots$calibration_even <- plotted_calibration$calplot.even[[1]]

    ## Test-period hex maps masked by hex context (no regional up-scaling)
    hex_plots$masked_maps <- plot_hex_masked_forecasts(
      hex_preds, context$hex_context, region_hexes, forecast_windows, "test", map_dates)

    list(tables = hex_scores, plots = hex_plots)
    
  })

  
  ##########################################################################################
  #### 3) Rare-event verification against climatology and the reference --------------------
  ##########################################################################################

  run_section("rare_event", {
    
    ## Prep predictions for calculating rare event scores
    scoring_data <- dplyr::bind_rows(
      hex_preds  |> dplyr::mutate(model = "xgboost", predicted_probability = prob_pred)
    , test_preds |> dplyr::mutate(model = "reference", predicted_probability = ref_prob)
    ) |>
      dplyr::left_join(hex_info |> dplyr::select(shapeName, lat, lon), by = "shapeName") |>
      dplyr::transmute(eval_set, forecast_interval, model, date, location = shapeName, x = lon, y = lat
                       , predicted_probability, truth = true_out)

    ## Calculate all of the rare event scores between the xgboost fitted model, the reference
     ## and flat probabilities (build internally)
    scores <- score_rare_event_forecasts(
      scoring_data
    , reference_model = "reference"
    , n_boot          = n_boot
    , bootstrap_sets  = "test"
    , n_boot_corp     = n_boot_corp)

    ## Curves are kept for plotting but not saved as tables
    curve_names <- c("corp_curve", "corp_band", "molchan_curve")
    qs::qsave(scores[curve_names], file.path(out_dir, "data", "rare_event_curves.qs"))

    list(
      tables = scores[setdiff(names(scores), curve_names)]
    , plots  = c(
        plot_rare_event_scores(scores, "test")
      , if ("outer_folds" %in% scores$basic$eval_set) {
          rlang::set_names(plot_rare_event_scores(scores, "outer_folds")[
            c("corp", "molchan_zoom")
          ], c("corp_outer_folds", "molchan_zoom_outer_folds"))
      }
    ))
    
  })

  
  ##########################################################################################
  #### 4) Outbreak-focused skill -----------------------------------------------------------
  ##########################################################################################
  
  run_section("outbreak", {
    per_set <- purrr::imap(linked, ~ evaluate_outbreak_skill(.x, .y, alert_fractions = alert_fractions, n_boot = n_boot_episodes))
    tables  <- purrr::map(rlang::set_names(names(per_set[[1]])), function(nm) dplyr::bind_rows(purrr::map(per_set, nm)))
    
    plots   <- plot_outbreak_skill(per_set$test)
    
    if (!is.null(per_set$outer_folds)) {
      plots <- c(plots, rlang::set_names(plot_outbreak_skill(per_set$outer_folds)[c("lead_curve", "warning_curve")]
                                         , c("lead_curve_outer_folds", "warning_curve_outer_folds")))
    }
    list(tables = tables, plots = plots)
  })

  
  ##########################################################################################
  #### 5) Regional up-scaling with the copula ----------------------------------------------
  ##########################################################################################

  regional_out <- run_section("regional", {
    
    region_overlap <- build_hex_region_overlap(regions_path, region_hexes)
    
    regional       <- do.call(evaluate_regional_copula, c(list(
      hex_preds        = hex_preds
    , hexed_outbreaks  = hexed_outbreaks
    , region_overlap   = region_overlap
    , forecast_windows = forecast_windows
    ), copula_args))

    ## Regional predictions (full-data fit) and out-of-fold rows saved as data objects
    region_names <- region_overlap$regions_ea |> sf::st_drop_geometry() |> dplyr::select(dplyr::any_of(c("area_index", "country", "region")))
    qs::qsave(regional$region_rows |> dplyr::left_join(region_names, by = "area_index"), file.path(out_dir, "data", "regional_predictions.qs"))
    qs::qsave(regional$oof, file.path(out_dir, "data", "regional_oof_predictions.qs"))

    regional_plots <- plot_regional_copula(regional, "test")

    ## Per-region skill (outer folds first) and prior outbreak history, used to mask the maps
    region_context <- build_region_context(regional, history_end = min(test_preds$date) - 1)

    ## Test-period regional maps masked by region context
    regional_plots$masked_maps <- plot_regional_masked_forecasts(
      regional, region_context, forecast_windows, "test", map_dates)

    list(
      tables = c(regional[c("params_full", "params_by_fold", "scores", "scores_by_size", "rare_event_scores")]
               , list(region_context = region_context))
    , plots  = regional_plots
      ## Out-of-fold regional rows passed on to the whole-horizon section and the forecast context (not saved as a table)
    , data   = list(oof = regional$oof)
    )
  })

  
  ##########################################################################################
  #### 5b) Any outbreak within the whole forecast horizon ----------------------------------
  ##########################################################################################

  ## Hex rows always; regional rows when the regional section ran
  run_section("cumulative", evaluate_cumulative_forecasts(
    hex_preds, regional_out$data$oof, forecast_windows, alert_fractions, min_events_for_auc))

  
  ##########################################################################################
  #### 6) Spatial, climate-phase, temporal and model-structure diagnostics -----------------
  ##########################################################################################

  shape_to_country <- hex_info |> dplyr::select(shapeName, Country)

  spatial <- run_section("spatial", diagnose_spatial_performance(test_preds, region_hexes, shape_to_country, min_events_for_auc))
  country_perf <- if (!is.null(spatial)) spatial$tables$country_perf else NULL

  run_section("enso", diagnose_enso_performance(
    test_preds, test_data, shape_to_country, country_perf, enso_data_dir, correlation_method, min_events_for_auc))

  run_section("temporal", diagnose_temporal_performance(test_preds, shape_to_country, country_perf, min_events_for_auc))

  if (!is.null(fitted_model)) {
    run_section("model_structure", diagnose_model_structure(
      fitted_model, test_data, chosen_outer_fold, chosen_purpose_tag, shap_sample_n, pdp_grid_res))
  }

  ## Importance across all outer-fold models (model_structure describes one model)
  if (!is.null(variable_importance)) {
    run_section("variable_importance", diagnose_variable_importance(variable_importance))
  }

  
  ##########################################################################################
  #### 7) Save tables, figures, the results bundle and the report --------------------------
  ##########################################################################################

  results$figure_files <- save_performance_outputs(results, out_dir)
  qs::qsave(results[c("tables", "errors", "meta", "figure_files")], file.path(out_dir, "results.qs"))
  write_performance_report(results, out_dir)

  ## Mask context and copula fit kept for masking the forward-forecast maps of later forecast runs
  if (!is.null(context_dir)) {
    save_forecast_context(results, test_preds, regional_out$data$oof, context_dir, run_date, regions_path, copula_args)
  }

  out_dir

}


  ##########################################################################################
  #### 8) Helper Functions -----------------------------------------------------------------
  ##########################################################################################

## Consecutive forecast windows from their upper bounds (days): the first starts at day 1, each
## later one at the previous upper bound
make_forecast_windows <- function(forecast_horizon) {
  tibble::tibble(forecast_interval = forecast_horizon, window_lower = c(1, utils::head(forecast_horizon, -1)) + 1)
}

## Read the calibration-harvest predictions and calibrate each fold with coefficients fit
## on the other folds (cross_fit_probability_calibration)
read_harvest_predictions <- function(harvest_dir) {

  files <- if (is.null(harvest_dir)) character(0) else list.files(harvest_dir, pattern = "^outer_raw.*\\.Rds$", full.names = TRUE)
  if (!length(files)) return(NULL)

  harvest <- purrr::map_dfr(files, readRDS)
  harvest$prob_pred <- cross_fit_probability_calibration(harvest)

  harvest |>
    dplyr::filter(!is.na(prob_pred)) |>
    dplyr::transmute(
      outer_fold_id
    , date
    , shapeName         = hex_id
    , forecast_interval = as.numeric(as.character(forecast_interval))
    , prob_pred
    , true_out          = as.numeric(as.character(truth)))

}

## Largest country representation in each hex + lat lon centroid in each hex
build_hex_info <- function(train_data, test_data, region_hexes) {

  hex_sf <- if (inherits(region_hexes, "sf")) region_hexes else region_hexes[[1]]

  ## Hex centroids from the hex polygons
  centroids <- suppressWarnings(sf::st_centroid(sf::st_geometry(hex_sf)))
  coords    <- sf::st_coordinates(centroids)

  country <- dplyr::bind_rows(
    train_data |> dplyr::select(shapeName, Country)
  , if (!is.null(test_data)) test_data |> dplyr::select(shapeName, Country)
  ) |>
    dplyr::count(shapeName, Country) |>
    dplyr::slice_max(n, n = 1, by = shapeName, with_ties = FALSE) |>
    dplyr::select(shapeName, Country)

  tibble::tibble(shapeName = hex_sf$shapeName, lon = coords[, 1], lat = coords[, 2]) |>
    dplyr::left_join(country, by = "shapeName")

}

## Save every table as csv and every figure as png; returns the figure file paths
save_performance_outputs <- function(results, out_dir) {

  ## Tables: one csv per section_table
  purrr::iwalk(results$tables, function(section_tables, section) {
    purrr::iwalk(section_tables, function(tb, nm) {
      if (is.data.frame(tb) && nrow(tb) > 0) {
        tb <- tb |> dplyr::select(dplyr::where(~ !is.list(.x)))
        utils::write.csv(tb, file.path(out_dir, "tables", paste0(section, "__", nm, ".csv")), row.names = FALSE)
      }
    })
  })

  ## Figures: one file per figure; lists of figures (e.g., maps) get a suffix per element
  figure_files <- list()

  purrr::iwalk(results$plots, function(section_plots, section) {
    purrr::iwalk(section_plots, function(p, nm) {
      ## Nested lists of figures (e.g., one map per date and window) get one suffix per level
      figure_files <<- c(figure_files, save_figure_tree(p, file.path(out_dir, "figures", paste0(section, "__", nm)), paste(section, nm, sep = "__")))
    })
  })

  figure_files

}

## Save a figure, or every figure in a (possibly nested) plain list of figures; returns the
## saved paths named by key, with one "__<name>" suffix added per list level
save_figure_tree <- function(p, stub, key) {

  ## Only plain lists (e.g., one map per date) are collections of figures; figure objects
   ## such as party trees are lists too, but carry their own class
  if (identical(class(p), "list")) {
    purrr::imap(p, function(pp, sub) {
      sub <- gsub("[^A-Za-z0-9_-]", "_", sub)
      save_figure_tree(pp, paste0(stub, "__", sub), paste(key, sub, sep = "__"))
    }) |>
      unname() |>
      purrr::list_c()
  } else {
    path <- save_performance_figure(p, stub)
    if (is.null(path)) list() else rlang::set_names(list(path), key)
  }

}

## Save one figure: ggplots and party trees as png, html widgets as html; returns the path
## (NULL when the object is not a figure or saving fails)
save_performance_figure <- function(p, file_stub) {

  if (inherits(p, "ggplot")) {

    ## Maps get a square canvas
    is_map <- grepl("map", file_stub)
    path   <- paste0(file_stub, ".png")
    saved  <- tryCatch({
      ggplot2::ggsave(path, p, width = if (is_map) 10 else 11, height = if (is_map) 10 else 6, dpi = 110)
      TRUE
    }, error = function(e) {
      message("Could not save ", path, ": ", conditionMessage(e))
      FALSE
    })
    if (saved) path else NULL

  } else if (inherits(p, "party")) {

    ## Rendered decision tree (lorax / partykit) drawn with base graphics
    path <- paste0(file_stub, ".png")
    tryCatch({
      grDevices::png(path, width = 2400, height = 1400, res = 110)
      plot(p)
      grDevices::dev.off()
      path
    }, error = function(e) {
      try(grDevices::dev.off(), silent = TRUE)
      NULL
    })

  } else if (inherits(p, "htmlwidget") && requireNamespace("htmlwidgets", quietly = TRUE)) {

    ## visNetwork fallback tree
    path <- paste0(file_stub, ".html")
    tryCatch({
      htmlwidgets::saveWidget(p, file.path(normalizePath(dirname(path)), basename(path)), selfcontained = FALSE)
      path
    }, error = function(e) NULL)

  } else {
    NULL
  }

}
