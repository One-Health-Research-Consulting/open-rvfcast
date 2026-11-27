#' Explore what is driving the current forecasts using SHAP; for monthly memos
#'
#' Explains this run's memo-window forecasts with SHAP contributions from the
#' forecast model. Covariates are grouped into families for reading.
#'
#' Products are saved to memo_root/<forecast date>/figures and .../tables:
#'  - drivers_importance: mean |contribution| per covariate family across all hexes 
#'    (what the model is leaning on this month, continent-wide)
#'  - drivers_vs_africa: per focus region, the mean contribution per family for the 
#'    region's Medium / High hexes minus the mean across all hexes in this forecast.
#'    That is, asks why the region's risk differs from the average African location 
#'    this month
#'  - drivers_change: per focus region, change in each family's contribution from 
#'    the same time of year in earlier years (same hexes, same model) to now. 
#'  - drivers_raw: per focus region, time series of the covariates behind the 
#'    largest changes, with recorded outbreaks in the region's countries
#'
#'
#' @title build_memo_drivers
#' @param memo_risk_map Output of build_memo_risk_map() (the memo_risk_map target)
#' @param fitted_model The fitted_model target (file paths of the forecast fit)
#' @param finalized_hyperparameters Path of the hyperparameter set (holds the calib_* columns)
#' @param region_data The region_data target (covariates for every hex, date and window)
#' @param focus_regions Named list of ISO3 country code vectors, one element per focus region
#' @param hexed_outbreaks_path Outbreaks placed in their labelled hexes (data/rvf_outbreaks_hexed.Rds)
#' @param season_halfwidth_days Reference rows are earlier dates within this many days of the
#'   forecast date's day of year
#' @param n_raw_features Covariates shown per region in the raw-data figure
#' @param raw_years Years of history shown in the raw-data figure
#' @param memo_root Folder of memo outputs
#' @return List: figures (named png paths), family_importance, region_drivers (now, reference and
#'   change per region and family), feature_change (per region and covariate), focus_hexes and
#'   reference_dates
#' @author Morgan Kain
#' @export
build_memo_drivers <- function(
    memo_risk_map
  , fitted_model
  , finalized_hyperparameters
  , region_data
  , focus_regions
  , hexed_outbreaks_path  = "data/rvf_outbreaks_hexed.Rds"
  , season_halfwidth_days = 15
  , n_raw_features        = 4
  , raw_years             = 6
  , memo_root             = "reports/memos"
) {

  forecast_date <- memo_risk_map$forecast_date
  memo_window   <- memo_risk_map$memo_window

  ## Forecast model pieces: parsnip fit, prepped recipe, scale_pos_weight used and the calibration
  model <- load_memo_model(fitted_model, finalized_hyperparameters, memo_window)

  ## Hexes analysed in each focus region: its Medium / High hexes (all its hexes if it has none)
  focus_hexes <- select_focus_hexes(memo_risk_map$hex_classes, focus_regions)

  ## Covariate rows: every hex now, and the focus hexes at the same time of year in earlier years
  window_rows <- region_data |> dplyr::filter(forecast_interval == memo_window)
  now_rows    <- window_rows |> dplyr::filter(date == forecast_date)
  ref_rows    <- window_rows |>
    dplyr::filter(shapeName %in% focus_hexes$shapeName, same_season(date, forecast_date, season_halfwidth_days)
                , date < forecast_date - 300)

  ## Calibrated SHAP contributions (long: one row per row-feature), with feature families
  shap_now <- memo_shap_long(model, now_rows)
  shap_ref <- memo_shap_long(model, ref_rows)

  ## Continent-wide importance by family this month
  family_importance <- shap_now |>
    dplyr::summarize(contribution = sum(contribution), .by = c(row_id, family)) |>
    dplyr::summarize(mean_abs = mean(abs(contribution)), .by = family) |>
    dplyr::arrange(dplyr::desc(mean_abs))

  ## Region-level drivers now, in the reference season, and the change, by family and by feature
  region_drivers <- summarize_region_drivers(shap_now, shap_ref, focus_hexes, by = "family") |>
    ## Covariate values averaged across a family's covariates have no meaning; kept per feature only
    dplyr::select(-dplyr::ends_with("_value")) |>
    ## Compared with the average hex across Africa in this same forecast
    dplyr::left_join(africa_family_means(shap_now), by = "family") |>
    dplyr::mutate(vs_africa = now - africa_mean, .after = now)
  
  ## Mean contribution per region and family (or feature) now and in the reference 
   ## season, and the change
  feature_change <- summarize_region_drivers(shap_now, shap_ref, focus_hexes, by = "feature")

  ## Covariates behind the largest changes per region (time-varying ones only), for the raw-data figure
  raw_features <- feature_change |>
    dplyr::filter(family %in% time_varying_families()) |>
    dplyr::slice_max(abs(change), n = n_raw_features, by = region, with_ties = FALSE)

  ## Figures
  fig_dir <- file.path(memo_root, as.character(forecast_date), "figures")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  figures <- list(
    drivers_importance = save_memo_figure(
      plot_family_importance(family_importance)
    , file.path(fig_dir, "drivers_importance.png"), 6.5, 3.5)
  , drivers_vs_africa  = save_memo_figure(
      plot_region_drivers(region_drivers, "vs_africa")
    , file.path(fig_dir, "drivers_vs_africa.png"), 8, 6)
  , drivers_change     = save_memo_figure(
      plot_region_drivers(region_drivers, "change")
    , file.path(fig_dir, "drivers_change.png"), 8, 6)
  , drivers_raw        = save_memo_figure(
      plot_region_raw_data(window_rows, focus_hexes, raw_features
                         , readRDS(hexed_outbreaks_path), focus_regions
                         , forecast_date, raw_years, season_halfwidth_days)
    , file.path(fig_dir, "drivers_raw.png"), 9, 8.5)
  )

  ## Tables for writing the expected / unexpected sections
  tab_dir <- file.path(memo_root, as.character(forecast_date), "tables")
  dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(region_drivers, file.path(tab_dir, "drivers_by_family.csv"), row.names = FALSE)
  utils::write.csv(feature_change, file.path(tab_dir, "drivers_by_feature.csv"), row.names = FALSE)

  list(
    figures           = figures
  , family_importance = family_importance
  , region_drivers    = region_drivers
  , feature_change    = feature_change
  , focus_hexes       = focus_hexes
  , reference_dates   = sort(unique(ref_rows$date)))

}


## Forecast fit, prepped recipe, scale_pos_weight as used at fitting and calibration
load_memo_model <- function(fitted_model, finalized_hyperparameters, memo_window) {

  ## Calibration scaling from the most recent training
  calibration <- read_calibration_coefficients(utils::read.csv(finalized_hyperparameters))
  
  ## Fitted model from most recent training
  model_out   <- readRDS(grep("/model_fit_", fitted_model, value = TRUE))

  ## Load and return needed pieces
  list(
    fit       = readRDS(grep("parsnip_fit_", fitted_model, value = TRUE))
  , recipe    = readRDS(grep("recipe_", fitted_model, value = TRUE))
  , spw       = unique(model_out$preds[[1]]$.spw_used)
  , intercept = calibration$intercept[calibration$forecast_interval == as.character(memo_window)]
  , slope     = calibration$slope[calibration$forecast_interval == as.character(memo_window)])

}

## Calibrated SHAP contributions in long form: row_id, shapeName, date, feature, family,
 ## feature_value and contribution (calibrated log-odds). The calibrated forecast 
 ## for a row is baseline + the sum of its contributions
memo_shap_long <- function(model, rows) {

  ## Same factor coding as at fit time (as.factor on all five windows)
  baked   <- suppressWarnings(recipes::bake(model$recipe, new_data = rows |>
    dplyr::mutate(forecast_interval = factor(forecast_interval, levels = c(30, 60, 90, 120, 150)))))
  x_names <- model$fit$preproc$x_names

  ## The model matrix exactly as parsnip builds it for prediction
  contrib <- stats::predict(model$fit$fit, xgb_model_matrix(baked, x_names), predcontrib = TRUE)

  ## Make sure contributions sum to the fitted model's own predictions
  parsnip_margin <- stats::qlogis(stats::predict(model$fit, baked, type = "prob")$.pred_1)
  stopifnot(max(abs(rowSums(contrib) - parsnip_margin)) < 1e-3)

  ## Feature values for reading the contributions (dense copy, NA where missing)
  values <- as.matrix(baked[, x_names])

  tibble::tibble(
    row_id        = rep(seq_len(nrow(rows)), times = length(x_names))
  , shapeName     = rep(rows$shapeName, times = length(x_names))
  , date          = rep(rows$date, times = length(x_names))
  , feature       = rep(x_names, each = nrow(rows))
  , feature_value = as.vector(values)
    ## Calibration multiplies the raw log-odds by the slope
  , contribution  = as.vector(contrib[, x_names, drop = FALSE]) * model$slope) |>
  dplyr::mutate(family = feature_family(feature))

}

## Medium / High hexes in each focus region's countries (all the countries' hexes if none are)
select_focus_hexes <- function(hex_classes, focus_regions) {

  purrr::imap_dfr(focus_regions, function(countries, region) {
    
    ## Check if in focal region of interest
    in_region <- hex_classes |> dplyr::filter(Country %in% countries, !is.na(prob_pred))
    
    ## Check if hex elevated
    elevated  <- in_region |> dplyr::filter(risk_level %in% c("Medium", "High"))
    (if (nrow(elevated)) elevated else in_region) |>
      dplyr::transmute(region, shapeName, Country, risk_level, prob_pred)
  }) |>
    dplyr::mutate(region = factor(region, levels = names(focus_regions)))

}

## Dates within halfwidth days of the target date's day of year (any year); creates
 ## a bit of a window to look in across in the past
same_season <- function(dates, target, halfwidth) {
  
  d <- abs(lubridate::yday(dates) - lubridate::yday(target))
  pmin(d, 365 - d) <= halfwidth

}

## Mean contribution per region and family (or feature) now and in the reference 
 ## season, and the change. Means are over the region's focus hexes 
 ## (reference: over hexes and reference dates)
summarize_region_drivers <- function(shap_now, shap_ref, focus_hexes, by = c("family", "feature")) {

  by     <- match.arg(by)
  keys   <- if (by == "family") "family" else c("family", "feature")
  hexes  <- focus_hexes |> dplyr::select(region, shapeName)

  ## Per row sums within the grouping, then means over the region's rows
  region_mean <- function(shap, col_name) {
    shap |>
      dplyr::inner_join(hexes, by = "shapeName", relationship = "many-to-many") |>
      dplyr::summarize(contribution = sum(contribution), value = mean(feature_value), .by = c(region, row_id, dplyr::all_of(keys))) |>
      dplyr::summarize("{col_name}" := mean(contribution), "{col_name}_value" := mean(value, na.rm = TRUE), .by = c(region, dplyr::all_of(keys)))
  }

  region_mean(shap_now, "now") |>
    dplyr::left_join(region_mean(shap_ref, "reference"), by = c("region", keys)) |>
    dplyr::mutate(change = now - reference) |>
    dplyr::arrange(region, dplyr::desc(abs(change)))

}

## Define groups of covariates to define "covariate families" to aid a higher-level
 ## discussion about what is driving predictions
feature_family <- function(feature) {

  dplyr::case_when(
    grepl("^anomaly_forecast_", feature)                         ~ "Forecast weather (next 3 months)"
  , grepl("^anomaly_scaled_precipitation_", feature)             ~ "Recent rainfall"
  , grepl("^anomaly_scaled_(temperature|relative_humidity)_", feature) ~ "Recent temperature & humidity"
  , grepl("^anomaly_scaled_ndvi_", feature)                      ~ "Vegetation greenness (NDVI)"
  , grepl("^anomaly_not_sero_", feature)                         ~ "Herd immunity (predicted seroprevalence)"
  , grepl("^glw_", feature)                                      ~ "Livestock density"
  , feature %in% c("trees", "grassland", "shrubs", "cropland", "bare", "water", "wetland", "soil_drainage", "soil_texture") ~ "Land cover & soil"
  , feature %in% c("lat", "lon", "doy") | grepl("^forecast_interval", feature) ~ "Location & season"
  , TRUE                                                         ~ "Long-term climate & elevation")

}

## Families whose covariates change from month to month (the others are fixed per hex)
time_varying_families <- function() {
  
  c(
    "Forecast weather (next 3 months)", "Recent rainfall"
  , "Recent temperature & humidity", "Vegetation greenness (NDVI)"
  , "Herd immunity (predicted seroprevalence)"
  )
  
}

## Save a ggplot as png and return the path
save_memo_figure <- function(plot, path, width, height) {
  
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 200, bg = "white")
  path
  
}

## Continent-wide mean contribution per family
plot_family_importance <- function(family_importance) {

  family_importance |>
    ggplot2::ggplot(ggplot2::aes(mean_abs, stats::reorder(family, mean_abs))) +
    ggplot2::geom_col(fill = "#2b78d4", width = 0.7) +
    ggplot2::labs(x     = "Average influence on this month's forecast (|log-odds|)"
                , y     = NULL
                , title = "What the model is leaning on this month (all of Africa)") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))

}

## Per-region covariate family contributions shown as odds multipliers:
 ## now relative to the model's average -OR- 
 ## change relative to the same season in earlier years
plot_region_drivers <- function(region_drivers, what = c("vs_africa", "change")) {

  what <- match.arg(what)

  plot_data <- region_drivers |>
    dplyr::mutate(effect = .data[[what]], direction = ifelse(effect > 0, "Raises risk", "Lowers risk"))

  titles <- c(
    vs_africa = "Why risk in each focus region differs from the rest of Africa this month"
  , change    = "Why risk differs from the same time of year in earlier years")
  x_labs <- c(
    vs_africa = "Odds of an outbreak compared with the average African location in this forecast"
  , change    = "Odds of an outbreak compared with the same season in earlier years")

  ## Net effect per region for the panel strips
  totals <- plot_data |>
    dplyr::summarize(total = sum(effect), .by = region) |>
    dplyr::mutate(strip = paste0(region, "\nOverall: odds ", odds_label(total)))

  plot_data |>
    dplyr::left_join(totals |> dplyr::select(region, strip), by = "region") |>
    dplyr::mutate(strip = factor(strip, levels = totals$strip[match(levels(region), totals$region)])) |>
    ggplot2::ggplot(ggplot2::aes(effect, tidytext_reorder(family, effect, strip), fill = direction)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~strip, scales = "free_y") +
    ggplot2::scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
    ## Log-odds axis labelled as odds multipliers
    ggplot2::scale_x_continuous(labels = odds_label) +
    ggplot2::scale_fill_manual(values = c("Raises risk" = "#a8320a", "Lowers risk" = "#2b78d4"), name = NULL) +
    ggplot2::labs(x = x_labs[[what]], y = NULL, title = titles[[what]]) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    , legend.position    = "bottom"
    , plot.title         = ggplot2::element_text(face = "bold")
    , strip.text         = ggplot2::element_text(face = "bold", hjust = 0))

}

## Mean contribution per covariate family across every hex in this forecast
africa_family_means <- function(shap_now) {
  
  shap_now |>
    dplyr::summarize(contribution = sum(contribution), .by = c(row_id, family)) |>
    dplyr::summarize(africa_mean = mean(contribution), .by = family)
  
}

## Log-odds shown as an odds multiplier, e.g. 0.69 -> "x2"
odds_label <- function(log_odds) {
  
  paste0(
    "\u00d7"
  , format(signif(exp(log_odds), 2)
           , trim = TRUE
           , scientific = FALSE
           , drop0trailing = TRUE)
  )

}

## Reorder a factor within facets
tidytext_reorder <- function(x, by, within) {
  
  stats::reorder(paste(x, within, sep = "___"), by)
  
}

## Time series showing the region mean over its focus hexes of the covariates behind 
 ## the largest changes, with the reference season shaded, the current value marked 
 ## and recorded outbreaks as ticks
plot_region_raw_data <- function(
    window_rows, focus_hexes, raw_features, hexed_outbreaks
  , focus_regions, forecast_date, raw_years, season_halfwidth_days) {

  start_date <- forecast_date - round(365.25 * raw_years)

  ## Region means over time for each region's own selected covariates
  series <- purrr::map_dfr(levels(focus_hexes$region), function(r) {
    feats <- raw_features$feature[raw_features$region == r]
    window_rows |>
      dplyr::filter(shapeName %in% focus_hexes$shapeName[focus_hexes$region == r], date >= start_date) |>
      dplyr::select(date, dplyr::all_of(feats)) |>
      tidyr::pivot_longer(-date, names_to = "feature", values_to = "value") |>
      dplyr::summarize(value = mean(value, na.rm = TRUE), .by = c(date, feature)) |>
      dplyr::mutate(region = r)
  }) |>
    dplyr::mutate(
      region = factor(region, levels = levels(focus_hexes$region))
    , panel  = paste(region, readable_feature(feature), sep = "\n"))

  ## Recorded outbreaks in each region's countries over the same span
  outbreak_ticks <- purrr::imap_dfr(focus_regions, function(countries, r) {
    hexed_outbreaks |>
      dplyr::filter(country_iso3c %in% countries, start_date >= .env$start_date) |>
      dplyr::distinct(start_date) |>
      dplyr::mutate(region = r)
  })
  outbreak_ticks <- series |>
    dplyr::distinct(region, panel) |>
    dplyr::mutate(region = as.character(region)) |>
    dplyr::inner_join(outbreak_ticks, by = "region", relationship = "many-to-many")

  ## Same-season windows in each year shown
  season_bands <- tibble::tibble(year = seq(lubridate::year(start_date), lubridate::year(forecast_date))) |>
    dplyr::mutate(centre = as.Date(paste0(year, format(forecast_date, "-%m-%d"))), xmin = centre - season_halfwidth_days, xmax = centre + season_halfwidth_days)

  current <- series |> dplyr::filter(date == forecast_date)

  ggplot2::ggplot(series, ggplot2::aes(date, value)) +
    ggplot2::geom_rect(data = season_bands, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf)
                     , inherit.aes = FALSE, fill = "grey90") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(colour = "grey25", linewidth = 0.4) +
    ggplot2::geom_point(data = current, colour = "#a8320a", size = 2) +
    ggplot2::geom_rug(data = outbreak_ticks, ggplot2::aes(x = start_date), inherit.aes = FALSE, sides = "b"
                    , colour = "#a8320a", length = ggplot2::unit(0.06, "npc")) +
    ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 4) +
    ggplot2::labs(x = NULL, y = "Region average (standardised where an anomaly)"
                , title = "Raw data behind the largest changes in each focus region"
                , caption = paste("Grey bands: the same time of year as this forecast. Red point: this forecast.",
                                  "Red ticks: recorded outbreaks in the region's countries.")) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), strip.text = ggplot2::element_text(hjust = 0)
                 , panel.grid.minor = ggplot2::element_blank())

}

## Short readable labels for covariate names
readable_feature <- function(feature) {

  out <- feature |>
    sub(pattern = "^anomaly_forecast_scaled_", replacement = "Forecast ") |>
    sub(pattern = "^anomaly_scaled_", replacement = "") |>
    sub(pattern = "^anomaly_not_sero_([0-9]+)$", replacement = "Predicted seroprevalence (\\1-day mean)") |>
    gsub(pattern = "_", replacement = " ")
  ## Trailing lag in days on the anomalies (e.g., "precipitation 30" -> "precipitation anomaly, last 30 days")
  sub("([a-z]) ([0-9]+)$", "\\1 anomaly, last \\2 days", out) |>
    tools::toTitleCase() |>
    gsub(pattern = "Ndvi", replacement = "NDVI")

}
