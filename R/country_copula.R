#' Country polygons for up-scaling hex forecasts to whole countries
#'
#' Builds one polygon per African country (plus the Indian Ocean island units that appear in
#' the hex data) from rnaturalearth map units, coded with the ISO3 codes used in the model data,
#' and saves them in the equal-area CRS used by the regional up-scaling (build_hex_region_overlap()).
#' Somaliland is merged into Somalia; Mayotte and Reunion are kept as their own units.
#'
#' @title build_country_regions
#' @param out_path Where to save the countries (.Rds)
#' @param extra_units rnaturalearth gu_a3 codes added to the continent = "Africa" units
#' @param crs_ea Equal-area CRS of the saved polygons
#' @param overwrite Rebuild even if out_path exists
#' @return out_path (sf with columns country (ISO3), region (country name) and geometry)
#' @author Morgan Kain
#' @export
build_country_regions <- function(
    out_path    = "data/upscaling_regions/countries_rnaturalearth.Rds"
  , extra_units = c("MUS", "SYC", "MYT", "REU")
  , crs_ea      = "+proj=cea +datum=WGS84 +units=m"
  , overwrite   = FALSE
) {

  if (file.exists(out_path) && !overwrite) return(out_path)

  ## Map units split overseas territories (Mayotte, Reunion) from their sovereign country
  units <- rnaturalearth::ne_countries(type = "map_units", scale = "medium", returnclass = "sf") |>
    dplyr::filter(continent == "Africa" | gu_a3 %in% extra_units) |>
    ## Madeira is tagged Africa but is part of Portugal
    dplyr::filter(adm0_a3 != "PRT")

  countries <- units |>
    dplyr::mutate(country = country_iso3_from_units(gu_a3, adm0_a3)) |>
    sf::st_make_valid() |>
    ## One polygon per ISO3 code (merges Somaliland into Somalia and Zanzibar into Tanzania),
     ## named after the unit whose own code matches (e.g., Somalia, not Somaliland)
    dplyr::group_by(country) |>
    dplyr::summarize(region = dplyr::first(name_long[gu_a3 == country], default = dplyr::first(name_long)), .groups = "drop") |>
    sf::st_transform(crs_ea)

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(countries, out_path)

  out_path

}


#' Temporary function for transitioning to include country-level copula fits as
#' well as hex and region-level copula fits; that is, makes sure previously saved
#' forecasts (which create a "context" file; i.e., a metadata file about that forecast) 
#' holds a country-level copula
#'
#' Used for the temporary transition/development period in the construction of the 
#' monthly memo, which includes a step of creating country-wide predictions via 
#' a new fitted copula. Only needed until a new fit is run (which we have to do anyway
#' as of Nov 25, 2026) and output saved via run_comprehensive_performance(). When there
#' is no country fit, this function rebuilds the hex predictions from the 
#' linked_predictions.qs saved by the comprehensive performance run behind the context, 
#' fits the country copula with fit_country_copula(), and writes it into the context 
#' file (a copy of the original is kept in context_dir/backups).
#'
#' @title ensure_country_copula_context
#' @param context_dir Folder of saved training-run context (the most recent file is used)
#' @param perf_root Folder of comprehensive performance runs
#' @param country_regions_path Country polygons from build_country_regions()
#' @param hexed_outbreaks_path Outbreaks placed in their labelled hexes (data/rvf_outbreaks_hexed.Rds)
#' @param region_hexes The region_hexes target
#' @param forecast_horizon Upper bounds (days) of the consecutive forecast windows
#' @param copula_args Named list of extra arguments for evaluate_regional_copula()
#' @return Path of the context file (which now holds a country element)
#' @author Morgan Kain
#' @export
ensure_country_copula_context <- function(
    context_dir
  , perf_root
  , country_regions_path
  , hexed_outbreaks_path
  , region_hexes
  , forecast_horizon = c(30, 60, 90, 120, 150)
  , copula_args      = list()
) {

  ## Latest forecasts and associated metadata
  context_path <- get_latest_forecast_context(context_dir)
  context      <- qs::qread(context_path)

  ## Already fitted on these country polygons: nothing to do
  if (!is.null(context$country) && identical(context$country$regions_md5, unname(tools::md5sum(country_regions_path)))) {
    return(context_path)
  }

  ## Hex predictions (test period and outer folds) saved by the training run behind the context
  linked_path <- file.path(perf_root, as.character(context$run_date), "data", "linked_predictions.qs")
  if (!file.exists(linked_path)) {
    stop("Linked predictions for context run ", context$run_date, " not found at ", linked_path)
  }

  hex_preds <- purrr::imap_dfr(
    qs::qread(linked_path), ~ .x$predictions |> 
      dplyr::mutate(eval_set = .y, .before = 1))

  ## Fit the country copula the same way the training run would have
  context$country <- fit_country_copula(
    hex_preds            = hex_preds
  , hexed_outbreaks      = readRDS(hexed_outbreaks_path)
  , region_hexes         = region_hexes
  , country_regions_path = country_regions_path
  , forecast_windows     = make_forecast_windows(forecast_horizon)
  , history_end          = context$history_end
  , copula_args          = copula_args)

  ## Keep the original context before writing the country fit into it (backups/ is not
   ## matched by get_latest_forecast_context(), which only lists the top folder)
  backup_dir <- file.path(context_dir, "backups")
  dir.create(backup_dir, showWarnings = FALSE)
  file.copy(context_path, file.path(backup_dir, paste0("pre_country_", basename(context_path))), overwrite = FALSE)

  qs::qsave(context, context_path)

  context_path

}


#' Country-level Gaussian copula fit for up-scaling hex predictions
#'
#' Runs evaluate_regional_copula() with countries as the regions (one latent correlation rho
#' and one cloglog recalibration per forecast window, leave-one-year-out scored), and builds the
#' per-country skill / outbreak-history context. Used by run_comprehensive_performance() in
#' training runs and by ensure_country_copula_context() as a forecast-mode fallback (see
#' lengthy notes associated with the above function).
#'
#' @title fit_country_copula
#' @param hex_preds Hex predictions with eval_set, date, shapeName, forecast_interval and prob_pred
#' @param hexed_outbreaks Outbreaks placed in their labelled hexes
#' @param region_hexes The region_hexes target
#' @param country_regions_path Country polygons from build_country_regions()
#' @param forecast_windows Tibble with forecast_interval and window_lower
#' @param history_end Last date counted as prior outbreak history (day before the test period)
#' @param copula_args Named list of extra arguments for evaluate_regional_copula()
#' @return List: regions_path, regions_md5, params_full, params_by_fold, scores, scores_by_size,
#'   rare_event_scores, region_context (per country), copula_settings and fit_date
#' @author Morgan Kain
#' @export
fit_country_copula <- function(hex_preds, hexed_outbreaks, region_hexes, country_regions_path
                               , forecast_windows, history_end, copula_args = list()) {

  overlap <- build_hex_region_overlap(country_regions_path, region_hexes)

  fit <- do.call(evaluate_regional_copula, c(list(
    hex_preds        = hex_preds
  , hexed_outbreaks  = hexed_outbreaks
  , region_overlap   = overlap
  , forecast_windows = forecast_windows
  ), copula_args))

  ## Copula settings the fit used (evaluate_regional_copula() defaults unless overridden)
  copula_settings <- utils::modifyList(
    as.list(formals(evaluate_regional_copula)[c("latent_step", "min_coverage")])
  , copula_args[base::intersect(names(copula_args), c("latent_step", "min_coverage"))])

  ## Large per-row objects are dropped; forecasts are re-predicted from params_full
  list(
    regions_path      = country_regions_path
  , regions_md5       = unname(tools::md5sum(country_regions_path))
  , params_full       = fit$params_full
  , params_by_fold    = fit$params_by_fold
  , scores            = fit$scores
  , scores_by_size    = fit$scores_by_size
  , rare_event_scores = fit$rare_event_scores
  , region_context    = build_region_context(fit, history_end = history_end)
  , copula_settings   = copula_settings
  , fit_date          = Sys.Date()
  )

}


## ISO3 codes as used in the model data from rnaturalearth map-unit codes: overseas units keep
 ## their own code (gu_a3, e.g. MYT, REU), Somaliland joins Somalia, and rnaturalearth's own
 ## codes for South Sudan / Western Sahara are swapped for ISO3
country_iso3_from_units <- function(gu_a3, adm0_a3) {

  code <- ifelse(adm0_a3 == "FRA", gu_a3, adm0_a3)
  dplyr::recode(code, SOL = "SOM", SDS = "SSD", SAH = "ESH")

}
