#' Internal scaffold for a monthly forecast memo using Quarto to be edited down 
#' to form an external memo
#'
#' Pulls together the various memo-related targets saved across all memo-associated
#' targets, copies the Quarto template to memo_root/<forecast date>/memo_<forecast date>.qmd 
#' and renders it. 
#'
#' @title build_memo_draft
#' @param memo_risk_map Output of build_memo_risk_map()
#' @param memo_relative_map Output of build_memo_relative_map()
#' @param memo_country_table Output of build_memo_country_table()
#' @param memo_drivers Output of build_memo_drivers()
#' @param template_path Quarto template (reports/templates/rvf_monthly_memo.qmd)
#' @param context_dir Folder of saved training-run context (model id and training run date)
#' @param hexed_outbreaks_path Outbreaks placed in their labelled hexes (last outbreak date used)
#' @param forecast_horizon Upper bounds (days) of the consecutive forecast windows
#' @param headers Section headers, also used as the bold lead phrases of the summary bullets
#' @param memo_root Folder of memo outputs
#' @param render Render the memo after writing the inputs
#' @return List: qmd (memo source), inputs (rds of memo inputs) and rendered (output path, or NA
#'   when not rendered or rendering failed)
#' @author Morgan Kain
#' @export
build_memo_draft <- function(
    memo_risk_map
  , memo_relative_map
  , memo_country_table
  , memo_drivers
  , template_path        = "reports/templates/rvf_monthly_memo.qmd"
  , context_dir          = "outputs/forecast_context"
  , hexed_outbreaks_path = "data/rvf_outbreaks_hexed.Rds"
  , forecast_horizon     = c(30, 60, 90, 120, 150)
  , headers              = list(
      overall    = "Where risk is highest"
    , relative   = "How unusual the risk is"
    , countries  = "Countries to watch"
    , expected   = "Predictions we expected"
    , unexpected = "Predictions that surprised us")
  , memo_root            = "reports/memos"
  , render               = TRUE
) {

  ## Bookkeeping/metadata
  forecast_date <- memo_risk_map$forecast_date
  memo_dir      <- file.path(memo_root, as.character(forecast_date))
  context       <- qs::qread(get_latest_forecast_context(context_dir))

  ## Country names for ISO3 codes (from the country table)
  country_names <- rlang::set_names(memo_country_table$all_countries$country_name, memo_country_table$all_countries$iso3)

  ## Everything the template reads, as plain R objects
  inputs <- list(
    headers   = headers
  , meta      = memo_meta_inputs(memo_risk_map, context, hexed_outbreaks_path, forecast_horizon)
  , risk      = memo_risk_inputs(memo_risk_map, country_names)
  , relative  = memo_relative_inputs(memo_relative_map, memo_risk_map, country_names)
  , countries = memo_country_inputs(memo_country_table)
  , drivers   = list(notes = memo_driver_notes(memo_drivers)))

  inputs_path <- file.path(memo_dir, "data", "memo_inputs.rds")
  dir.create(dirname(inputs_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(inputs, inputs_path)

  ## Memo source from the template, written once (later runs only refresh inputs and figures)
  qmd_path <- file.path(memo_dir, paste0("memo_", forecast_date, ".qmd"))
  if (!file.exists(qmd_path)) write_memo_from_template(template_path, qmd_path, inputs$meta, forecast_date)

  list(
    qmd      = qmd_path
  , inputs   = inputs_path
  , rendered = if (render) render_memo(qmd_path) else NA_character_)

}


## Dates, window and model metadata
memo_meta_inputs <- function(memo_risk_map, context, hexed_outbreaks_path, forecast_horizon) {

  forecast_date <- memo_risk_map$forecast_date
  window        <- make_forecast_windows(forecast_horizon) |> dplyr::filter(forecast_interval == memo_risk_map$memo_window)

  list(
    forecast_date           = forecast_date
  , forecast_date_text      = long_date(forecast_date)
  , period_label            = memo_risk_map$period_label
  , window_days_text        = paste0(window$window_lower, "–", window$forecast_interval, " days")
  , window_dates_text       = paste(long_date(forecast_date + window$window_lower), "–", long_date(forecast_date + window$forecast_interval - 1))
  , outbreak_data_end_text  = long_date(max(readRDS(hexed_outbreaks_path)$start_date))
  , model_id                = paste(context$model_id, collapse = ", ")
  , training_run_text       = long_date(context$run_date))

}

## Risk; countries with the most high-risk hexes and the threshold footnote numbers
memo_risk_inputs <- function(memo_risk_map, country_names) {

  hexes      <- memo_risk_map$hex_classes
  thresholds <- memo_risk_map$thresholds
  high       <- thresholds[thresholds$risk_level == "high", ]
  medium     <- thresholds[thresholds$risk_level == "medium", ]

  ## Countries with the most high-risk hexes (ties broken by medium-risk hexes)
  top_countries <- hexes |>
    dplyr::summarize(n_high = sum(risk_level == "High", na.rm = TRUE), n_medium = sum(risk_level == "Medium", na.rm = TRUE), .by = Country) |>
    dplyr::filter(n_high > 0) |>
    dplyr::arrange(dplyr::desc(n_high), dplyr::desc(n_medium)) |>
    dplyr::slice_head(n = 4)

  list(
    n_high             = sum(hexes$risk_level == "High", na.rm = TRUE)
  , n_medium           = sum(hexes$risk_level == "Medium", na.rm = TRUE)
  , top_countries_text = unname(country_names[top_countries$Country])
  , high_prob          = high$prob
  , high_share         = high$share_outbreaks_above
  , high_lift          = round(high$lift)
  , high_top_pct       = paste0(100 * (1 - high$quantile), "%")
  , medium_prob        = medium$prob
  , medium_share       = medium$share_outbreaks_above
  , medium_top_pct     = paste0(100 * (1 - medium$quantile), "%")
  , test_period_text   = paste(month_year(high$test_first_date), "–", month_year(high$test_last_date)))

}

## Details on hexes at their highest since the start of the test period 
memo_relative_inputs <- function(memo_relative_map, memo_risk_map, country_names) {

  compared <- memo_relative_map$hex_relative |> dplyr::filter(risk_level %in% c("Medium", "High"))

  top_never <- compared |>
    dplyr::filter(never_as_high) |>
    dplyr::count(Country, sort = TRUE) |>
    dplyr::slice_head(n = 3)

  ## Months with no history between the last history date and this forecast
  gap_months <- as.numeric(memo_risk_map$forecast_date - memo_relative_map$history_end) / (365.25 / 12)
  gap_note   <- if (gap_months > 1.5) {
    paste0("No forecasts were made between "
          , month_year(memo_relative_map$history_end)
          , " and this forecast, so the shortest possible time since risk was this high is about "
          , round(gap_months)
          , " months. " 
          , "This gap is due to the last training run being completed on "
          , month_year(memo_relative_map$history_end)
          , " and no forecasts being run in-between this model training and this current forecast.")
  } else ""

  list(
    n_compared          = nrow(compared)
  , n_never             = sum(compared$never_as_high)
  , top_never_countries = unname(country_names[top_never$Country])
  , history_start_text  = month_year(memo_relative_map$history_start)
  , history_end_text    = month_year(memo_relative_map$history_end)
  , gap_note            = gap_note
  )

}

## Memo-ready country table 
memo_country_inputs <- function(memo_country_table) {

  top <- memo_country_table$table

  ## Plain-language track record
  track <- c("Demonstrated skill" = "Good", "Low Forecast Skill" = "Mixed", "No outbreaks in evaluation period" = "Not yet tested")

  table <- data.frame(
    Rank                  = top$rank
  , Country               = top$country_name
  , Chance                = memo_pct(top$p_outbreak)
  , `Last memo`           = ifelse(is.na(top$p_previous), "–", memo_pct(top$p_previous))
  , `High-risk locations` = paste0(top$n_high, " (", top$n_high_skill, ")")
  , `Last outbreak`       = ifelse(is.na(top$last_outbreak), "None", format(top$last_outbreak, "%b %Y"))
  , `Track record`        = unname(track[as.character(top$quality_class)])
  , check.names = FALSE
  )

  ## No earlier memo from this model yet: drop the empty column
  if (all(is.na(top$p_previous))) table$`Last memo` <- NULL

  list(
    table              = table
  , top3_text          = paste0(top$country_name[1:3], " (", memo_pct(top$p_outbreak[1:3]), ")")
  , expected_countries = sum(memo_country_table$all_countries$p_outbreak)
  )

}

## Draft notes per focus region: overall change from the same season in earlier 
 ## years and covariates behind it 
memo_driver_notes <- function(memo_drivers) {

  fam  <- memo_drivers$region_drivers
  feat <- memo_drivers$feature_change |> dplyr::filter(family %in% time_varying_families())

  purrr::map_chr(levels(fam$region), function(r) {

    f  <- fam[fam$region == r, ] |> dplyr::arrange(dplyr::desc(change))
    up <- f[f$change > 0.05, ] |> utils::head(2)
    dn <- f[f$change < -0.05, ] |> dplyr::arrange(change) |> utils::head(2)
    v  <- feat[feat$region == r, ] |> dplyr::slice_max(abs(change), n = 2)

    paste0(
      "**", r, "**: odds ", odds_label(sum(f$change)), " vs. the same season in earlier years (",
      odds_label(sum(f$vs_africa)), " vs. the average African location now). ",
      if (nrow(up)) paste0("Raised by ", and_join(paste0(up$family, " (", odds_label(up$change), ")")), ". ") else "",
      if (nrow(dn)) paste0("Lowered by ", and_join(paste0(dn$family, " (", odds_label(dn$change), ")")), ". ") else "",
      "Largest covariate changes: ", and_join(paste0(readable_feature(v$feature), " now ", signif(v$now_value, 2),
                                                     " vs. ", signif(v$reference_value, 2), " usually")), ".")

  })

}

## Fill out memo template
write_memo_from_template <- function(template_path, qmd_path, meta, forecast_date) {

  text <- readLines(template_path, warn = FALSE)

  tokens <- c(
    "{{PERIOD_LABEL}}"       = meta$period_label
  , "{{FORECAST_DATE_LONG}}" = meta$forecast_date_text
  , "{{TODAY_LONG}}"         = long_date(Sys.Date())
  , "{{FORECAST_DATE}}"      = as.character(forecast_date))

  for (tk in names(tokens)) text <- gsub(tk, tokens[[tk]], text, fixed = TRUE)

  dir.create(dirname(qmd_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(text, qmd_path)

}

## Render the memo with Quarto from its own folder; returns the output path or NA on failure
render_memo <- function(qmd_path) {

  out <- tryCatch({
    quarto::quarto_render(qmd_path, quiet = TRUE)
    sub("\\.qmd$", ".pdf", qmd_path)
  }, error = function(e) {
    warning("Memo render failed: ", conditionMessage(e))
    NA_character_
  })

  out

}

## "e.g., 4 December 2026"
long_date <- function(d) trimws(format(as.Date(d), "%e %B %Y"))

## "e.g., December 2026"
month_year <- function(d) format(as.Date(d), "%B %Y")

## Shorthand for small values Percent with one decimal, "<0.1%" for tiny values
memo_pct <- function(p) ifelse(p < 0.001, "<0.1%", paste0(formatC(100 * p, format = "f", digits = 1), "%"))

## text phrase for memo: "a, b and c"
and_join <- function(x) {
  if (length(x) <= 1) x else paste(paste(utils::head(x, -1), collapse = ", "), "and", utils::tail(x, 1))
}
