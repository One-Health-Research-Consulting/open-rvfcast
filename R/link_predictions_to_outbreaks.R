#' Link hex-level predictions to the outbreaks behind them
#'
#' Attaches the WAHIS outbreaks (with their chain and country index-case flags) to every 
#' hex-level prediction whose forecast window contains the outbreak's start date. 
#' This is used to support outbreak-centric evaluation (performance focused on 
#' the outbreaks (how well are each/all outbreaks predicted). Outbreak hexes come 
#' from export_hexed_outbreaks(), which places outbreaks the same as assigned 
#' in lag_join_aggregate(). 
#'
#' @title link_predictions_to_outbreaks
#' @param predictions Path to the saved predictions (the ex_fits.all_probs_raw target) or
#'   a tibble with at least date (the base date), shapeName, forecast_interval, prob_pred
#'   and true_out. If an `aggregation` column is present only "No aggregation" rows (the
#'   hex-level predictions) are kept
#' @param hexed_outbreaks Path to the file saved by export_hexed_outbreaks()
#' @param forecast_intervals Consecutive pairs define the windows and the upper value labels each window
#' @param episode_gap_days Outbreak starts in the same hex at most this many days apart
#'   belong to the same hex episode
#' @param max_base_date_gap An outbreak has full lead-time coverage when no gap longer than
#'   this many days separates the base dates covering its leads (base dates can have gaps,
#'   e.g., between outer folds)
#' @return A list of tibbles:
#'   - predictions: one row per hex prediction (input columns kept) plus prob_rank_pct
#'     (percentile of prob_pred among all hexes for that base date and window; 1 = highest
#'     risk), n_outbreaks, n_cases, n_index_cases, n_country_index_cases, first_start_date
#'     and days_before_first for the outbreaks in the row's window, and
#'     label_from_outbreaks (1 when those outbreaks sum to > 0 cases; should equal true_out)
#'   - prediction_events: one row per (prediction row, outbreak in its window), with the
#'     outbreak's id, hex episode, chain id, index flags, cases, country,
#'     counts_toward_label (cases > 0) and days_before_start (start_date minus base date)
#'   - outbreaks: one row per outbreak whose start falls inside the period the predictions
#'     can target, with its hex, episode, chain, index flags, counts_toward_label, whether
#'     its hex has predictions, how many forecasts targeted it, and whether forecasts exist
#'     at every lead time from the shortest to the longest window (full_lead_coverage)
#'   - label_check: counts of true_out against label_from_outbreaks by window.
#'     Off-diagonal counts mean the linking disagrees with the pipeline's labels
#' @author Morgan Kain
#' @export
link_predictions_to_outbreaks <- function(
    predictions
  , hexed_outbreaks
  , forecast_intervals = c(1, 30, 60, 90, 120, 150)
  , episode_gap_days   = 30
  , max_base_date_gap  = 50
) {

  ## Forecast windows: lower bound (inclusive) and upper bound (exclusive, also the label)
  forecast_windows <- tibble::tibble(
    forecast_interval = utils::tail(forecast_intervals, -1)
  , window_lower      = utils::head(forecast_intervals, -1)
  )

  ## Hex-level prediction rows, with each row's risk rank among all hexes that day
   ## (for each forecast_interval on a given forecast date, what is the rank for
    ## a given hex among all hexes in that forecast interval on that date)
  hex_preds <- read_hex_predictions(predictions) |>
    dplyr::mutate(
      prob_rank_pct = dplyr::percent_rank(prob_pred)
    , .by = dplyr::any_of(c("outer_fold_id", "date", "forecast_interval"))
    )

  ## Outbreaks with their labelled hex (from the rvf pipeline), grouped into hex episodes
   ## Labels in sequential order repeat outbreaks in each hex
  hex_outbreaks <- add_hex_episodes(hexed_outbreaks, episode_gap_days)

  ## Long table of every prediction row paired with each outbreak in its window
   ## Every forecast_interval that has an outbreak in it gets its own row. Longer than hex_outbreaks
    ## because a given outbreak can show up in multiple forecast intervals given the use of two
     ## random days per month
  prediction_events <- link_outbreaks_to_windows(hex_preds, hex_outbreaks, forecast_windows)

  ## Keys identifying one prediction row (outer_fold_id only when present)
  row_keys <- base::intersect(c("outer_fold_id", "date", "shapeName", "forecast_interval"), names(hex_preds))

  ## Per prediction row summaries of the outbreaks in its window
  predictions_out <- hex_preds |>
    dplyr::left_join(
      ## Calculates some summary statistics on the prediction_events to collapse the 
       ## multiple representation of given outbreaks
      prediction_events |>
        dplyr::summarize(
          n_outbreaks           = dplyr::n_distinct(outbreak_id)
        , n_cases               = sum(cases, na.rm = TRUE)
        , n_index_cases         = sum(index_case, na.rm = TRUE)
        , n_country_index_cases = sum(country_index_case, na.rm = TRUE)
        , first_start_date      = min(start_date)
        , days_before_first     = min(days_before_start)
        , .by = dplyr::all_of(row_keys))
    , by = row_keys) |>
    dplyr::mutate(
      dplyr::across(
        c(n_outbreaks, n_cases, n_index_cases, n_country_index_cases)
      , ~ tidyr::replace_na(.x, 0L))
      ## Same rule as lag_join_aggregate(): an outbreak label needs > 0 summed cases
    , label_from_outbreaks = as.numeric(n_cases > 0)
    )

  ## Quick check for agreement between the linked outbreaks and the pipeline's own labels
  label_check <- predictions_out |>
    dplyr::count(forecast_interval, true_out, label_from_outbreaks)

  ## Look for issues in matching
  n_disagree <- label_check |>
    dplyr::filter(true_out != label_from_outbreaks) |>
    dplyr::pull(n) |>
    sum()

  ## Warn the user if there is a mismatch between how many outbreaks were recorded 
   ## by forecast interval from earlier in the pipeline with this joining here
  if (n_disagree > 0) {
    warning(n_disagree, " prediction rows disagree with true_out; see label_check")
  }

  ## Return the matched/summarized outbreaks
  list(
    predictions       = predictions_out
  , prediction_events = prediction_events
  , outbreaks         = summarize_outbreak_coverage(hex_outbreaks, hex_preds, prediction_events, forecast_windows, max_base_date_gap)
  , label_check       = label_check
  )

}


## Read the saved predictions (path or tibble) and keep the hex-level rows
read_hex_predictions <- function(predictions) {

  ## Accept either the saved file path (as returned by ex_fits.all_probs_raw) or a tibble
  if (is.character(predictions)) {
    predictions <- qs::qread(predictions)
  }

  required <- c("date", "shapeName", "forecast_interval", "prob_pred", "true_out")
  missing  <- setdiff(required, names(predictions))
  if (length(missing)) {
    stop("predictions is missing column(s): ", paste(missing, collapse = ", "))
  }

  ## Only hex-level rows are linked to outbreaks; aggregated rows are dropped
  if ("aggregation" %in% names(predictions)) {
    predictions <- predictions |>
      dplyr::filter(aggregation == "No aggregation") |>
      dplyr::select(-aggregation, -dplyr::any_of("region_norm"))
  }

  predictions |>
    dplyr::mutate(
      date              = as.Date(date)
    , forecast_interval = as.numeric(as.character(forecast_interval))
    , true_out          = as.numeric(as.character(true_out))
    )

}


## Read the hexed outbreaks (path or tibble) and group starts into hex episodes
add_hex_episodes <- function(hexed_outbreaks, episode_gap_days) {

  ## Accept either the file saved by export_hexed_outbreaks() or the tibble itself
  if (is.character(hexed_outbreaks)) {
    hexed_outbreaks <- readRDS(hexed_outbreaks)
  }

  hexed_outbreaks |>
    dplyr::arrange(shapeName, start_date) |>
    ## Starts in the same hex at most episode_gap_days apart form one hex episode
    dplyr::mutate(
      hex_episode = paste(shapeName, cumsum(c(TRUE, diff(start_date) > episode_gap_days)), sep = "_")
    , .by = shapeName)

}


## Pair every prediction row with each outbreak whose start falls in its window
link_outbreaks_to_windows <- function(hex_preds, hex_outbreaks, forecast_windows) {

  hex_preds |>
    dplyr::select(dplyr::any_of("outer_fold_id"), date, shapeName, forecast_interval
                  , prob_pred, prob_rank_pct, true_out) |>
    dplyr::inner_join(hex_outbreaks, by = "shapeName", relationship = "many-to-many") |>
    dplyr::left_join(forecast_windows, by = "forecast_interval") |>
    ## Same window rule as get_rvf_response(): [date + lower, date + upper)
    dplyr::filter(
      start_date >= date + window_lower
    , start_date <  date + forecast_interval
    ) |>
    dplyr::mutate(days_before_start = as.numeric(start_date - date)) |>
    dplyr::select(-window_lower) |>
    dplyr::relocate(
      hex_episode, outbreak_id, dplyr::any_of("outbreak_chain_id"), shapeName, start_date
    , dplyr::any_of(c("index_case", "country_index_case")), cases, counts_toward_label, country_iso3c
    ) |>
    dplyr::arrange(hex_episode, start_date, date, forecast_interval)

}


## One row per outbreak the predictions could have targeted, with lead-time coverage
summarize_outbreak_coverage <- function(hex_outbreaks, hex_preds, prediction_events, forecast_windows, max_base_date_gap) {

  first_base <- min(hex_preds$date)
  last_base  <- max(hex_preds$date)
  base_dates <- sort(unique(hex_preds$date))
  max_lead   <- max(forecast_windows$forecast_interval) - 1
  min_lead   <- min(forecast_windows$window_lower)

  ## Number of forecasts that targeted each outbreak, and the range of their lead times
  targeted <- prediction_events |>
    dplyr::summarize(
      n_forecasts        = dplyr::n()
    , n_windows          = dplyr::n_distinct(forecast_interval)
    , longest_lead_days  = max(days_before_start)
    , shortest_lead_days = min(days_before_start)
    , .by = outbreak_id)

  hex_outbreaks |>
    ## Outbreaks a base date in the prediction period could reach at some lead time
    dplyr::filter(
      start_date >= first_base + min_lead
    , start_date <= last_base + max_lead
    ) |>
    dplyr::mutate(
      in_predicted_hex   = shapeName %in% hex_preds$shapeName
    ) |>
    dplyr::mutate(
      ## Base dates exist across every lead from min_lead to max_lead days before the start:
       ## no gap longer than max_base_date_gap between the lead range's ends and the base
       ## dates inside it
      full_lead_coverage = purrr::map_lgl(start_date, function(s) {
        lo <- s - max_lead
        hi <- s - min_lead
        if (lo < first_base || hi > last_base) return(FALSE)
        pts <- c(lo, base_dates[base_dates >= lo & base_dates <= hi], hi)
        max(diff(as.numeric(pts))) <= max_base_date_gap
      })
    ) |>
    dplyr::left_join(targeted, by = "outbreak_id") |>
    dplyr::mutate(n_forecasts = tidyr::replace_na(n_forecasts, 0L))

}
