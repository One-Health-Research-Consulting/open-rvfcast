#' Simplified low / medium / high risk map for the monthly forecast memo
#'
#' Collapses the current forecast (for one forecast window; see memo_window) into
#' three risk levels, and outlines hexes by how well the model has forecast outbreaks there
#' (over the test period).
#' Built for simplicity for external sharing of model forecasts. In contrast to those maps 
#' created in forecast_context_maps, the map here shows only:
#'  - fill: risk level. Cut-offs are quantiles of the test-period hex probabilities for the same
#'    window (from the comprehensive performance run behind the saved forecast context). This
#'    allows for a comparison of these cutoffs across months: the idea will be to produce
#'    and archive monthly memos so that it is easy to trace changes over time 
#'  - outline: forecast quality_class from the saved hex context. Thick = "Demonstrated skill",
#'    thin = "Low Forecast Skill", none = no outbreaks in the evaluation period (skill cannot be
#'    judged) 
#'
#' @title build_memo_risk_map
#' @param forecast_files Files written by examine_fits_within() in a PURPOSE = forecast run
#'   (the examined_fits target)
#' @param region_hexes The region_hexes target
#' @param context_dir Folder of saved training-run context (the most recent file is used)
#' @param perf_root Folder of comprehensive performance runs; the test-period predictions are read
#'   from perf_root/<context run date>/data/test_predictions_with_context.qs
#' @param memo_window Upper bound (days) of the forecast window shown (e.g., 90 = days 61-90)
#' @param risk_quantiles Named test-period quantiles where "medium" and "high" begin
#' @param forecast_horizon Upper bounds (days) of the consecutive forecast windows
#' @param period_label Calendar label for the window; NULL builds one from the forecast date
#' @param out_root Figures go to out_root/<forecast date>/figures
#' @return List: figure (png path), hex_classes (one row per hex with probability, risk level and
#'   quality class), thresholds (cut-offs with their test-period performance), forecast_date,
#'   memo_window and period_label
#' @author Morgan Kain
#' @export
build_memo_risk_map <- function(
    forecast_files
  , region_hexes
  , context_dir      = "outputs/forecast_context"
  , perf_root        = "outputs/comprehensive_performance"
  , memo_window      = 90
  , risk_quantiles   = c(medium = 0.90, high = 0.975)
  , forecast_horizon = c(30, 60, 90, 120, 150)
  , period_label     = NULL
  , out_root         = "reports/memos"
) {

  ## Context saved by the most recent training run
  context    <- qs::qread(get_latest_forecast_context(context_dir))
  
  ## Test-period predictions for the most recent training run
  test_preds <- read_context_test_predictions(context, perf_root)

  ## Cut-offs from the test period for the memo window
  thresholds <- memo_risk_thresholds(test_preds, memo_window, risk_quantiles)

  ## This run's forecasts for the memo window only
  hex_preds     <- read_forecast_hex_predictions(forecast_files) |> dplyr::filter(forecast_interval == memo_window)
  forecast_date <- max(hex_preds$date)

  ## Calendar label for the window (e.g., "December 2026 - January 2027") unless one is supplied
  if (is.null(period_label)) period_label <- memo_period_label(forecast_date, memo_window, forecast_horizon)

  ## One row per hex: probability, risk level and forecast quality
  hex_classes <- context$hex_context |>
    dplyr::select(shapeName, Country, quality_class, n_prior_outbreaks) |>
    dplyr::full_join(hex_preds |> dplyr::select(shapeName, prob_pred), by = "shapeName") |>
    dplyr::mutate(risk_level = classify_memo_risk(prob_pred, thresholds))

  ## Draw and save the map
  map_gg <- plot_memo_risk_map(
    hex_classes, region_hexes, period_label
  , show_performance_low_risk = FALSE)

  ## Make path and save figure
  fig_dir  <- file.path(out_root, as.character(forecast_date), "figures")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  
  fig_path <- file.path(fig_dir, "risk_map.png")
  ggplot2::ggsave(fig_path, map_gg, width = 7, height = 7.5, dpi = 200, bg = "white")

  list(
    figure        = fig_path
  , hex_classes   = hex_classes
  , thresholds    = thresholds
  , forecast_date = forecast_date
  , memo_window   = memo_window
  , period_label  = period_label)

}


#' Map of how long it has been since each hex's risk was this high. Another 
#' simplified view of current risk (at present for use in the external memo)
#'
#' For every hex rated Medium or High in memo_risk_map (no sense in comparing 
#' relative risk in areas with low risk), finds the most recent earlier base date 
#' at which the hex's predicted probability (same forecast window) was at least 
#' as high as now, and maps the months since then. 
#' 
#' Hexes never as high in the history are labelled "highest since" the start of 
#' the history. Low-risk hexes are grey (a small probability being unusually high is
#' not of interest).
#'
#' History is (1) the test-period predictions of the training run behind the saved 
#' context and (2) the hex forecasts of earlier memos made with the same model 
#' (archived by this function in memo_root/<forecast date>/data/hex_forecasts.qs), 
#' so the gap between the end of the test period and the present fills in as monthly
#' memos accumulate.
#'
#' @title build_memo_relative_map
#' @param memo_risk_map Output of build_memo_risk_map() (the memo_risk_map target)
#' @param region_hexes The region_hexes target
#' @param context_dir Folder of saved training-run context (the most recent file is used)
#' @param perf_root Folder of comprehensive performance runs (see build_memo_risk_map())
#' @param memo_root Folder of memo outputs; this run's hex forecasts are archived to
#'   memo_root/<forecast date>/data and earlier archives are read from it
#' @return List: figure (png path), hex_relative (one row per hex with last_as_high_date,
#'   months_since and never_as_high), history_start, history_end, history_sources (base dates
#'   per source) and archive (path of this run's archived hex forecasts)
#' @author Morgan Kain
#' @export
build_memo_relative_map <- function(
    memo_risk_map
  , region_hexes
  , context_dir = "outputs/forecast_context"
  , perf_root   = "outputs/comprehensive_performance"
  , memo_root   = "reports/memos"
) {

  ## Context saved by the most recent training run
  context    <- qs::qread(get_latest_forecast_context(context_dir))
  
  ## Test-period predictions for the most recent training run
  test_preds <- read_context_test_predictions(context, perf_root)

  forecast_date <- memo_risk_map$forecast_date
  memo_window   <- memo_risk_map$memo_window

  ## Archive this run's hex forecasts (with the model they came from) for later memos
  archive_path <- archive_memo_hex_forecasts(memo_risk_map, context$model_id, memo_root)

  ## History: test-period predictions plus earlier memos from the same model, before this forecast
  history <- dplyr::bind_rows(
    test_preds |>
      dplyr::filter(forecast_interval == memo_window) |>
      dplyr::transmute(source = "test period", date, shapeName, prob_pred)
  , read_memo_hex_archives(memo_root, context$model_id, memo_window, before = forecast_date))

  ## Months since each hex was last at least as high as now
  hex_relative <- months_since_as_high(memo_risk_map$hex_classes, history, forecast_date)

  ## Draw and save the map
  map_gg <- plot_memo_relative_map(
    hex_relative              = hex_relative
  , region_hexes              = region_hexes
  , period_label              = memo_risk_map$period_label
  , history_start             = min(history$date)
  , show_performance_low_risk = FALSE
  )

  ## Create path and save output
  fig_path <- file.path(memo_root, as.character(forecast_date), "figures", "relative_map.png")
  dir.create(dirname(fig_path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(fig_path, map_gg, width = 7, height = 7.5, dpi = 200, bg = "white")

  ## Even though the plot of interest is saved, return a bundle for the target
  list(
    figure          = fig_path
  , hex_relative    = hex_relative
  , history_start   = min(history$date)
  , history_end     = max(history$date)
  , history_sources = history |> dplyr::summarize(n_dates = dplyr::n_distinct(date), first = min(date), last = max(date), .by = source)
  , archive         = archive_path
  )

}


#' Country table for the external memo: probability an outbreak is recorded in each 
#' country
#'
#' Up-scales predictions for the forecast window of interest to whole countries with 
#' the country copula fit saved in the forecast context (see ensure_country_copula_context() and
#' fit_country_copula()), and ranks countries by P(at least one recorded outbreak 
#' in the window). 
#' 
#' Alongside each country the following is recorded: last memo's value (from earlier
#' memos made with the same model), the number of Medium / High hexes (and High hexes
#' with demonstrated skill), the date of the most recent recorded outbreak and the 
#' country's forecast quality class. Likely most of this will not actually enter the
#' external-facing memo, but is nice for internal evaluation
#'
#' @title build_memo_country_table
#' @param memo_risk_map Output of build_memo_risk_map() (the memo_risk_map target)
#' @param forecast_files Files written by examine_fits_within() (the examined_fits target)
#' @param region_hexes The region_hexes target
#' @param country_context_path Context file holding the country fit (the memo_country_context target)
#' @param hexed_outbreaks_path Outbreaks placed in their labelled hexes (data/rvf_outbreaks_hexed.Rds)
#' @param memo_root Folder of memo outputs; country forecasts are archived to
#'   memo_root/<forecast date>/data and the table to memo_root/<forecast date>/tables
#' @param n_top Number of countries in the memo table
#' @return List: table (top n_top countries), all_countries, csv (path of the saved table) and
#'   archive (path of this run's archived country forecasts)
#' @author Morgan Kain
#' @export
build_memo_country_table <- function(
    memo_risk_map
  , forecast_files
  , region_hexes
  , country_context_path
  , hexed_outbreaks_path = "data/rvf_outbreaks_hexed.Rds"
  , memo_root            = "reports/memos"
  , n_top                = 10
) {

  ## As with the other functions above, load "context"; i.e., the metadata about this
   ## forecast for bookkeeping and to make sure things are lining up with training
  context       <- qs::qread(country_context_path)
  country_fit   <- context$country
  forecast_date <- memo_risk_map$forecast_date

  ## Hex-country overlap and the country names / codes
   ## NOTE: Some tiny countries lost in predictions tibble due to layer used. Rebuild
   ## here to get all countries appropriately with hex overlap with each 
  overlap   <- build_hex_region_overlap(country_fit$regions_path, region_hexes)
  countries <- overlap$regions_ea |> 
    sf::st_drop_geometry() |> 
    dplyr::select(area_index, iso3 = country, country_name = region)

  ## Country P(any recorded outbreak) in the memo window from the saved copula fit
  hex_preds <- read_forecast_hex_predictions(forecast_files) |>
    dplyr::filter(forecast_interval == memo_risk_map$memo_window)
  country_probs <- predict_regional_copula(hex_preds, overlap, country_fit$params_full, country_fit$copula_settings) |>
    dplyr::select(area_index, coverage, p_outbreak = p_copula) |>
    dplyr::left_join(countries, by = "area_index")

  ## Medium / High hex counts per country (hexes assigned to their largest-share country)
  hex_counts <- memo_risk_map$hex_classes |>
    dplyr::summarize(
      n_high       = sum(risk_level == "High", na.rm = TRUE)
    , n_high_skill = sum(risk_level == "High" & quality_class == "Demonstrated skill", na.rm = TRUE)
    , n_medium     = sum(risk_level == "Medium", na.rm = TRUE)
    , .by = Country)

  ## Most recent recorded outbreak per country (all outbreaks to date, placed by coordinates)
  last_outbreak <- place_outbreaks_in_regions(readRDS(hexed_outbreaks_path), overlap$regions_ea) |>
    dplyr::summarize(last_outbreak = max(start_date), .by = area_index)

  ## Archive this run's country probabilities, then read last memo's from the same model
  archive_path <- archive_memo_country_forecasts(country_probs, forecast_date, context$model_id, memo_root)
  previous     <- read_previous_memo_countries(memo_root, context$model_id, before = forecast_date)

  all_countries <- country_probs |>
    dplyr::left_join(previous, by = "iso3") |>
    dplyr::left_join(hex_counts, by = c("iso3" = "Country")) |>
    dplyr::left_join(last_outbreak, by = "area_index") |>
    dplyr::left_join(country_fit$region_context |> dplyr::select(area_index, quality_class), by = "area_index") |>
    dplyr::mutate(dplyr::across(c(n_high, n_high_skill, n_medium), ~ tidyr::replace_na(.x, 0L))) |>
    dplyr::arrange(dplyr::desc(p_outbreak)) |>
    dplyr::mutate(rank = dplyr::row_number(), .before = 1) |>
    dplyr::select(rank, country_name, iso3, p_outbreak, p_previous, previous_date, n_high, n_high_skill, n_medium
                , last_outbreak, quality_class, coverage)

  top <- all_countries |> dplyr::slice_head(n = n_top)

  ## Saved for the memo template
  csv_path <- file.path(memo_root, as.character(forecast_date), "tables", "top_countries.csv")
  dir.create(dirname(csv_path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(top, csv_path, row.names = FALSE)

  ## Return bundle for targets even though csv is saved to disk
  list(
    table         = top
  , all_countries = all_countries
  , csv           = csv_path
  , archive       = archive_path
  )

}


## Test-period hex predictions from the comprehensive performance run that saved the context
read_context_test_predictions <- function(context, perf_root) {

  path <- file.path(perf_root, as.character(context$run_date), "data", "test_predictions_with_context.qs")

  ## The context and its performance folder must come from the same training run
  if (!file.exists(path)) stop("Test-period predictions for forecast context run ", context$run_date, " not found at ", path)

  qs::qread(path)

}

## Risk-level cut-offs: test-period quantiles of the hex probabilities for one window, with the
 ## share of test-period outbreaks at or above each cut-off and the outbreak rate relative to
 ## the overall test-period rate (for the memo footnote)
memo_risk_thresholds <- function(test_preds, memo_window, risk_quantiles) {

  window_preds <- test_preds |> dplyr::filter(forecast_interval == memo_window)
  base_rate    <- mean(window_preds$true_out)

  tibble::tibble(
    risk_level = names(risk_quantiles)
  , quantile   = unname(risk_quantiles)
  , prob       = unname(stats::quantile(window_preds$prob_pred, risk_quantiles))
  ) |>
    dplyr::mutate(
      ## Share of all test-period hex-dates at or above the cut-off
      share_of_rows         = purrr::map_dbl(prob, ~ mean(window_preds$prob_pred >= .x))
      ## Share of test-period outbreak rows at or above the cut-off
    , share_outbreaks_above = purrr::map_dbl(prob, ~ mean(window_preds$prob_pred[window_preds$true_out == 1] >= .x))
      ## Observed outbreak rate at or above the cut-off relative to the overall rate
    , lift                  = purrr::map_dbl(prob, ~ mean(window_preds$true_out[window_preds$prob_pred >= .x]) / base_rate)
    , test_first_date       = min(window_preds$date)
    , test_last_date        = max(window_preds$date)
    )

}

## Low / Medium / High from probabilities and the cut-offs (NA probability stays NA)
classify_memo_risk <- function(prob, thresholds) {

  cut_at <- thresholds$prob[match(c("medium", "high"), thresholds$risk_level)]

  factor(
    dplyr::case_when(
      is.na(prob)       ~ NA_character_
    , prob >= cut_at[2] ~ "High"
    , prob >= cut_at[1] ~ "Medium"
    , TRUE              ~ "Low")
  , levels = c("Low", "Medium", "High"))

}

## Calendar months spanned by a forecast window from the forecast date, e.g., "December 2026 -
 ## January 2027" (labelled by every month the window touches). 
## NOTE: this is temporary for dev and needs to be adjusted (need to think on this, 
 ## see comments in model_framework_targets.R) to span a longer time period as fitting
 ## for 5 forecast windows and doing all of this to report on one window is not particularly
 ## sensible. Also, this strategy isn't really right as the prediction is for one 30 day window
 ## so saying multiple months is misleading anyway. TO BE UPDATED!!
memo_period_label <- function(forecast_date, memo_window, forecast_horizon) {

  window <- make_forecast_windows(forecast_horizon) |> dplyr::filter(forecast_interval == memo_window)

  ## First and last day in the window (outbreaks counted from date + window_lower to before date + upper)
  first_day <- forecast_date + window$window_lower
  last_day  <- forecast_date + window$forecast_interval - 1

  first_lab <- format(first_day, "%B %Y")
  last_lab  <- format(last_day, "%B %Y")

  if (identical(first_lab, last_lab)) first_lab else paste(first_lab, "–", last_lab)

}

## The memo risk map: three-level fill, skill outlines, country borders for orientation
plot_memo_risk_map <- function(hex_classes, region_hexes, period_label
                             , show_performance_low_risk = TRUE) {

  ## Hexes in lon/lat with their risk level and outline class
  hexes_sf <- memo_hexes_sf(region_hexes, hex_classes)

  ## Single-hue ordinal ramp, light to dark (lightness ~92 / 67 / 39 so levels stay distinct
   ## for colour-blind readers and in greyscale print)
  risk_colours <- c(Low = "#fbe3d0", Medium = "#f08a4b", High = "#a8320a")
  
  ## Update so that low risk hexes are not outlined, regardless of model performance there
  if (!show_performance_low_risk) {
  hexes_sf <- hexes_sf |>
    mutate(
      quality_class = as.character(quality_class)
    , quality_class = ifelse(risk_level == "Low", "No outbreaks in evaluation period", quality_class)
    , quality_class = as.factor(quality_class)) 
  }

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = hexes_sf, ggplot2::aes(fill = risk_level), colour = NA) +
    memo_reference_layers(hexes_sf) +
    ggplot2::scale_fill_manual(values = risk_colours, na.value = "grey85", name = "Outbreak risk"
                               , labels = c(Low = "Low", Medium = "Medium", High = "High"), na.translate = FALSE) +
    ggplot2::guides(
      fill      = ggplot2::guide_legend(order = 1, override.aes = list(colour = "grey40", linewidth = 0.2))
    , linewidth = ggplot2::guide_legend(order = 2, override.aes = list(fill = "white"), ncol = 1)) +
    ggplot2::labs(
      title    = paste("Rift Valley fever outbreak risk,", period_label)
    , caption  = "No outline: too few past outbreaks to judge the model.") +
    memo_map_theme()

}

## Write this run's memo-window hex forecasts, tagged with the model they came from, to
 ## memo_root/<forecast date>/data/hex_forecasts.qs; returns the path
archive_memo_hex_forecasts <- function(memo_risk_map, model_id, memo_root) {

  path <- file.path(memo_root, as.character(memo_risk_map$forecast_date), "data", "hex_forecasts.qs")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  ## One row per hex with a forecast
  memo_risk_map$hex_classes |>
    dplyr::filter(!is.na(prob_pred)) |>
    dplyr::transmute(
      date              = memo_risk_map$forecast_date
    , shapeName
    , forecast_interval = memo_risk_map$memo_window
    , prob_pred
    , model_id          = paste(model_id, collapse = ";")) |>
    qs::qsave(path)

  path

}

## Archived hex forecasts of earlier memos made with the same model and window, before a date
 ## (archives from another model are on a different probability scale and are skipped)
read_memo_hex_archives <- function(memo_root, model_id, memo_window, before) {

  files <- list.files(memo_root, pattern = "^hex_forecasts\\.qs$", recursive = TRUE, full.names = TRUE)

  ## Empty history in the same layout when there is nothing to add
  empty <- tibble::tibble(source = character(), date = as.Date(character()), shapeName = character(), prob_pred = numeric())
  if (!length(files) || is.null(model_id)) return(empty)

  purrr::map_dfr(files, qs::qread) |>
    dplyr::filter(model_id == paste(.env$model_id, collapse = ";"), forecast_interval == memo_window, date < before) |>
    dplyr::transmute(source = "earlier memos", date, shapeName, prob_pred) |>
    dplyr::bind_rows(empty)

}

## Write this run's country probabilities, tagged with the model they came from, to
 ## memo_root/<forecast date>/data/country_forecasts.qs; returns the path
archive_memo_country_forecasts <- function(country_probs, forecast_date, model_id, memo_root) {

  path <- file.path(memo_root, as.character(forecast_date), "data", "country_forecasts.qs")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  country_probs |>
    dplyr::transmute(date = forecast_date, iso3, p_outbreak, model_id = paste(model_id, collapse = ";")) |>
    qs::qsave(path)

  path

}

## Country probabilities from the most recent earlier memo made with the same model (empty
 ## when there is none); columns iso3, p_previous and previous_date
read_previous_memo_countries <- function(memo_root, model_id, before) {

  empty <- tibble::tibble(iso3 = character(), p_previous = numeric(), previous_date = as.Date(character()))
  files <- list.files(memo_root, pattern = "^country_forecasts\\.qs$", recursive = TRUE, full.names = TRUE)
  if (!length(files) || is.null(model_id)) return(empty)

  archived <- purrr::map_dfr(files, qs::qread) |>
    dplyr::filter(model_id == paste(.env$model_id, collapse = ";"), date < before)
  if (!nrow(archived)) return(empty)

  ## Only the latest earlier memo
  archived |>
    dplyr::filter(date == max(date)) |>
    dplyr::transmute(iso3, p_previous = p_outbreak, previous_date = date)

}

## Per hex: most recent history date with a probability at least as high as the current one,
 ## months since then, and whether the hex was never that high in the history
months_since_as_high <- function(hex_classes, history, forecast_date) {

  ## Latest earlier date at or above the current probability
  last_high <- history |>
    dplyr::inner_join(hex_classes |> dplyr::select(shapeName, current_prob = prob_pred), by = "shapeName") |>
    dplyr::filter(prob_pred >= current_prob) |>
    dplyr::summarize(last_as_high_date = max(date), .by = shapeName)

  hex_classes |>
    dplyr::left_join(last_high, by = "shapeName") |>
    dplyr::mutate(
      ## Average month length, so 12 months is a year
      months_since  = as.numeric(forecast_date - last_as_high_date) / (365.25 / 12)
      ## Forecast hexes with history that never reached the current probability
    , never_as_high = is.na(last_as_high_date) & !is.na(prob_pred) & shapeName %in% history$shapeName)

}

## The memo relative-risk map: months since risk was this high for Medium / High hexes (darker =
 ## longer ago; never in the history = darkest), Low hexes grey, same outlines as the risk map
plot_memo_relative_map <- function(
    hex_relative, region_hexes, period_label, history_start
  , show_performance_low_risk = TRUE) {

  ## Stepped time bins (months), with "never as high in the history" as its own top step
  step_labels <- c("Within the past year", "1–2 years ago", "2–3 years ago", "3 or more years ago"
                 , paste("Highest since", format(history_start, "%b %Y")))

  ## Hexes in lon/lat; time bins only for Medium / High hexes
  hexes_sf <- memo_hexes_sf(region_hexes, hex_relative) |>
    dplyr::mutate(
      compared  = risk_level %in% c("Medium", "High")
    , time_step = factor(dplyr::case_when(
        !compared          ~ NA_character_
      , never_as_high      ~ step_labels[5]
      , months_since <= 12 ~ step_labels[1]
      , months_since <= 24 ~ step_labels[2]
      , months_since <= 36 ~ step_labels[3]
      , TRUE               ~ step_labels[4])
      , levels = step_labels))
  
  ## Update so that low risk hexes are not outlined, regardless of model performance there
  if (!show_performance_low_risk) {
    hexes_sf <- hexes_sf |>
      mutate(
        quality_class = as.character(quality_class)
        , quality_class = ifelse(risk_level == "Low", "No outbreaks in evaluation period", quality_class)
        , quality_class = as.factor(quality_class)) 
  }

  ggplot2::ggplot() +
    ## Low-risk and unpredicted hexes first, in light grey
    ggplot2::geom_sf(data = hexes_sf |> dplyr::filter(!compared), fill = "grey93", colour = NA) +
    ggplot2::geom_sf(data = hexes_sf |> dplyr::filter(compared), ggplot2::aes(fill = time_step), colour = NA) +
    memo_reference_layers(hexes_sf) +
    ## Single-hue stepped ramp (blue, so it is not confused with the risk-level map), light = recent
    ggplot2::scale_fill_manual(
      values = rlang::set_names(c("#b6d5f8", "#6aabf1", "#2b78d4", "#174f99", "#0a2148"), step_labels)
    , drop   = FALSE
    , name   = "Last time risk\nwas this high") +
    ggplot2::guides(
      fill      = ggplot2::guide_legend(order = 1, override.aes = list(colour = "grey40", linewidth = 0.2))
    , linewidth = ggplot2::guide_legend(order = 2, override.aes = list(fill = "white"), ncol = 1)) +
    ggplot2::labs(
      title   = paste0("How unusual the risk is for ", period_label)
    , caption = paste("Grey: low risk this period (not compared).",
                      "No outline: too few past outbreaks to judge the model.")) +
    memo_map_theme()

}

## Hex polygons in lon/lat joined to per-hex memo columns
memo_hexes_sf <- function(region_hexes, hex_table) {

  ## region_hexes target wraps the sf object in a list
  if (!inherits(region_hexes, "sf")) region_hexes <- region_hexes[[1]]

  region_hexes |>
    dplyr::select(shapeName) |>
    sf::st_transform(4326) |>
    dplyr::left_join(hex_table, by = "shapeName")

}

## Layers shared by the memo maps: country borders, skill outlines (thick / thin by quality_class),
 ## their linewidth scale and the map extent
memo_reference_layers <- function(hexes_sf) {

  ## Only hexes where skill could be judged get an outline
  outline_labels <- c(
    "Demonstrated skill" = "Model has anticipated past outbreaks here"
  , "Low Forecast Skill" = "Model has had mixed results here")
  outlined <- hexes_sf |>
    dplyr::filter(quality_class %in% names(outline_labels)) |>
    dplyr::mutate(track_record = factor(outline_labels[as.character(quality_class)], levels = outline_labels))

  ## Country borders for orientation (drawn under the skill outlines)
  countries <- rnaturalearth::ne_countries(continent = "Africa", scale = "medium", returnclass = "sf") |>
    sf::st_transform(4326)

  list(
    ggplot2::geom_sf(data = countries, fill = NA, colour = "grey55", linewidth = 0.2)
  , ggplot2::geom_sf(data = outlined, ggplot2::aes(linewidth = track_record), fill = NA, colour = "grey10")
  , ggplot2::scale_linewidth_manual(values = c(0.9, 0.35), name = NULL, drop = FALSE)
  , ggplot2::coord_sf(xlim = c(-18, 52), ylim = c(-36.5, 38), datum = NA, expand = FALSE)
  )

}

## Theme shared by the memo maps (legend inside the map, over the Atlantic)
memo_map_theme <- function() {

  ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA)
    , plot.title        = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 6))
    , plot.caption      = ggplot2::element_text(hjust = 0, colour = "grey30", size = 12)
    , legend.position   = "inside"
    , legend.position.inside = c(0.01, 0.01)
    , legend.justification   = c(0, 0)
    , legend.key.size   = ggplot2::unit(0.65, "cm")
    , plot.margin       = ggplot2::margin(8, 8, 8, 8))

}
