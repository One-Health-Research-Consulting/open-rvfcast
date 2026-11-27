#' Context-masked maps of true forward forecasts (PURPOSE = forecast)
#'
#' Makes the hex and regional context-masked maps for a forecast run, using the forward
#' forecasts of this run and the context saved by the most recent PURPOSE = train run
#' (save_forecast_context(), called from run_comprehensive_performance()):
#'  - hex maps: the forecast hex probabilities with the saved per-hex mask context
#'  - regional maps: the forecast hex probabilities up-scaled to the saved regions with the
#'    copula parameters fitted on all evaluation data in the training run (params_full), with
#'    the saved per-region mask context
#' Colour limits are the ones saved from the training run, so forecast maps are comparable with
#' each other and with the test-period maps. Outbreaks only appear on the maps if outbreaks that
#' started inside a forecast window are already in hexed_outbreaks.
#'
#' @title run_forecast_context_maps
#' @param forecast_files Files written by examine_fits_within() in a PURPOSE = forecast run
#'   (the examined_fits_within_pan target); the Africa-wide forecast json files are used
#' @param region_hexes The region_hexes target
#' @param hexed_outbreaks_path Outbreaks placed in their labelled hexes; saved by
#'   export_hexed_outbreaks() (data/rvf_outbreaks_hexed.Rds)
#' @param context_dir Folder of saved training-run context (the most recent file is used)
#' @param out_root Maps go to out_root/<forecast date>
#' @param forecast_horizon Upper bounds (days) of the consecutive forecast windows (the
#'   forecast_horizon target)
#' @param fitted_model The fitted_model target (file paths); its hyperparameter set must match
#'   the one the saved context was built from. NULL skips the check
#' @return Path of the output folder (figures, the regional forecast rows and the context used)
#' @author Morgan Kain
#' @export
run_forecast_context_maps <- function(
    forecast_files
  , region_hexes
  , hexed_outbreaks_path
  , context_dir      = "outputs/forecast_context"
  , out_root         = "outputs/forecasts/context_maps"
  , forecast_horizon = c(30, 60, 90, 120, 150)
  , fitted_model     = NULL
) {

  forecast_windows <- make_forecast_windows(forecast_horizon)

  ## Context saved by the most recent training run
  context_path <- get_latest_forecast_context(context_dir)
  context      <- qs::qread(context_path)

  ## Stop if the forecast model and the context (skill mask, copula fit, colour limits) come
   ## from different training runs, since the forecasts would be on a different 
   ## probability scale (as calibration happens for each run so using a mismatched
   ## calibration will produce the wrong probabilities)
  check_context_model_match(context, fitted_model, context_path)

  ## Forward forecasts of this run, in the hex_preds layout used by the map functions
  hex_preds <- read_forecast_hex_predictions(forecast_files)
  map_dates <- sort(unique(hex_preds$date))

  out_dir <- file.path(out_root, as.character(max(map_dates)))
  for (d in c("figures", "data")) dir.create(file.path(out_dir, d), showWarnings = FALSE, recursive = TRUE)

  plots <- list()

  ## Hex maps with the saved hex context
  plots$hex_masked_maps <- plot_hex_masked_forecasts(
    hex_preds, context$hex_context, region_hexes, forecast_windows, "forecast", map_dates
  , fill_limits = context$fill_limits$hex)

  ## Regional maps: forecasts up-scaled with the saved copula fit, masked by the saved region context
  if (!is.null(context$region_context)) {

    region_overlap <- build_hex_region_overlap(context$regions_path, region_hexes)
    region_rows    <- predict_regional_copula(hex_preds, region_overlap, context$params_full, context$copula_settings)

    ## Any outbreaks already recorded inside the forecast windows, placed in regions as in training
    region_outbreaks <- place_outbreaks_in_regions(readRDS(hexed_outbreaks_path), region_overlap$regions_ea)

    ## Regional forward forecasts are a product in their own right, so they are saved too
    region_names <- region_overlap$regions_ea |> sf::st_drop_geometry() |> dplyr::select(dplyr::any_of(c("area_index", "country", "region")))
    qs::qsave(region_rows |> dplyr::left_join(region_names, by = "area_index"), file.path(out_dir, "data", "regional_forecasts.qs"))

    plots$regional_masked_maps <- plot_regional_masked_forecasts(
      list(oof = region_rows, region_overlap = region_overlap, region_outbreaks = region_outbreaks), context$region_context
    , forecast_windows, "forecast", map_dates, fill_limits = context$fill_limits$regional)

  }

  ## Figures saved the same way as the comprehensive performance figures
  purrr::iwalk(plots, function(p, nm) save_figure_tree(p, file.path(out_dir, "figures", nm), nm))

  ## Record of which training-run context the maps were masked with
  writeLines(c(paste("context_file:", context_path), paste("context_run_date:", context$run_date)
             , paste("forecast_dates:", paste(map_dates, collapse = ", "))), file.path(out_dir, "context_used.txt"))

  out_dir

}


## Save the context needed to mask forecast maps (called from run_comprehensive_performance()
 ## in PURPOSE = train runs); returns the saved path, or NULL when the hex context is missing
save_forecast_context <- function(results, test_preds, regional_oof, context_dir, run_date, regions_path, copula_args, model_id = NULL, country_fit = NULL) {

  hex_context <- results$tables$context$hex_context
  if (is.null(hex_context)) {
    message("No hex context; forecast context not saved")
    return(NULL)
  }

  ## Copula settings the regional fit used (evaluate_regional_copula() defaults unless overridden)
  copula_settings <- utils::modifyList(
    as.list(formals(evaluate_regional_copula)[c("latent_step", "min_coverage")])
  , copula_args[base::intersect(names(copula_args), c("latent_step", "min_coverage"))])

  ## Test-period colour limits, so forecast maps share the test-period colour scale
  hex_probs   <- test_preds$prob_pred[test_preds$prob_pred > 0]
  region_lims <- if (!is.null(regional_oof)) {
    range(regional_oof$p_copula[regional_oof$eval_set == "test" & regional_oof$p_copula > 0], na.rm = TRUE)
  }

  context <- list(
    run_date         = run_date
  , history_end      = min(test_preds$date) - 1
  , hex_context      = hex_context
  , region_context   = results$tables$regional$region_context
  , params_full      = results$tables$regional$params_full
  , copula_settings  = copula_settings
  , regions_path     = regions_path
  , fill_limits      = list(hex = range(hex_probs, na.rm = TRUE), regional = region_lims)
    ## Hyperparameter set (tuning grid id + index) of the fits behind this context, checked by forecast runs
  , model_id         = model_id
    ## Country-level copula fit (fit_country_copula()), used for the memo country table
  , country          = country_fit)

  dir.create(context_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(context_dir, paste0("forecast_context_", run_date, ".qs"))
  qs::qsave(context, path)
  path

}

## Stop when the forecast model's hyperparameter set differs from the one the saved context was
 ## built from; contexts saved before model_id was recorded only get a message
check_context_model_match <- function(context, fitted_model, context_path) {

  ## Nothing to compare without fit paths
  if (is.null(fitted_model)) return(invisible(NULL))

  forecast_id <- model_id_from_fit_paths(fitted_model)

  ## Older contexts did not record the model they came from
  if (is.null(context$model_id)) {
    message("Forecast context ", basename(context_path), " has no model_id; cannot confirm it matches the forecast model (", forecast_id, ")")
    return(invisible(NULL))
  }

  ## Any difference means the forecast and the context are on different probability scales
  if (!identical(sort(forecast_id), sort(context$model_id))) {
    stop("Forecast model (", paste(forecast_id, collapse = ", "), ") does not match the model behind forecast context "
       , basename(context_path), " (", paste(context$model_id, collapse = ", "), "). Re-run PURPOSE = train"
       , " (comprehensive_performance) for this hyperparameter set, or check finalized_hyperparameters.")
  }

  invisible(NULL)

}

## Hyperparameter set id(s) ("<tuning_grid_id>_<index>") from fitted_model file paths
 ## (e.g., parsnip_fit_1_for_forecasting_tune_grid_<grid id>_tune_index_<index>_<fit tag>.Rds)
model_id_from_fit_paths <- function(fit_paths) {

  ## Only the parsnip fits are needed (model_fit / recipe files carry the same id)
  fits <- basename(grep("parsnip_fit_", unlist(fit_paths), value = TRUE))
  if (!length(fits)) return(NULL)

  unique(sub(".*_tune_grid_(.+)_tune_index_([0-9]+).*", "\\1_\\2", fits))

}

## Most recently saved training-run context (same rule as get_latest_finalized_hyperparameters)
get_latest_forecast_context <- function(context_dir) {

  candidates <- list.files(context_dir, pattern = "^forecast_context_.*\\.qs$", full.names = TRUE)

  if (length(candidates) == 0) {
    stop("PURPOSE = forecast maps need context saved by a prior PURPOSE = train run, but none was found in '"
       , context_dir, "'. Run PURPOSE = train (with the comprehensive_performance target) at least once first.")
  }

  ## Most recently modified file == most recently completed training run
  candidates[which.max(file.mtime(candidates))]

}

## Hex forecasts from the Africa-wide forecast json files, in the hex_preds layout (no outcomes yet)
read_forecast_hex_predictions <- function(forecast_files) {

  africa_files <- grep("forecasts_for_Africa_.*\\.json$", unlist(forecast_files), value = TRUE)
  if (!length(africa_files)) stop("No forecasts_for_Africa_*.json files among forecast_files")

  purrr::map_dfr(africa_files, ~ jsonlite::read_json(.x, simplifyVector = TRUE)) |>
    ## The json repeats each hex once per ADM2 it overlaps; probabilities are per hex
    dplyr::distinct(H3, date, forecast_interval, outbreak_probability) |>
    dplyr::transmute(
      eval_set          = "forecast"
    , date              = as.Date(date)
    , shapeName         = H3
    , forecast_interval = as.numeric(forecast_interval)
    , prob_pred         = outbreak_probability
    , true_out          = NA_real_)

}

## Regional P(any outbreak) for new hex predictions with a saved copula fit: the same pieces,
 ## aggregations and copula as evaluate_regional_copula(), with rho and the recalibration taken
 ## from params_full instead of being fitted. Rows below min_coverage are dropped (unmapped)
predict_regional_copula <- function(hex_preds, region_overlap, params_full, copula_settings) {

  pieces <- build_region_pieces(hex_preds, region_overlap$overlap)
  pieces$z <- stats::qnorm(pieces$piece_q, lower.tail = FALSE)

  rows <- summarize_region_rows(pieces) |>
    dplyr::left_join(params_full |> dplyr::select(forecast_interval, rho_hat, recal_a, recal_b), by = "forecast_interval") |>
    dplyr::mutate(p_recal = -expm1(-exp(recal_a) * pmax(hazard, 1e-12)^recal_b))

  ## Same latent integration grid as evaluate_regional_copula()
  rows$p_copula <- copula_p_at_row_rho(pieces, rows, seq(-7, 7, by = copula_settings$latent_step))

  rows |>
    dplyr::filter(coverage >= copula_settings$min_coverage) |>
    dplyr::select(-row_id)

}
