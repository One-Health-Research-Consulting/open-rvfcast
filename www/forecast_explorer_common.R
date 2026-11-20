####
## Shared code for the forecast explorer apps (forecast_explorer_hex.R and forecast_explorer_regions.R)
##
## Both apps map the latest forward forecasts (colour = predicted probability of an outbreak) and,
## when a hex / region is clicked, describe its forecast quality and outbreak history. The data
## files are built by build_forecast_app_data() (R/build_app_components.R) in the same layout
## for hexes and regions, so the two apps differ only in their data file.
####

#### Load needed packages ========================================================

library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(scales)
library(bslib)
library(shinyWidgets)
library(aws.s3)

#### Local vs deployed mode ======================================================

## SHINY_LOCAL defaults to "1" (local). Set SHINY_LOCAL=0 in the shinyapps.io
## environment variables dashboard to switch to R2 reads.
LOCAL <- identical(Sys.getenv("SHINY_LOCAL", "1"), "1")

## Downloads object_key from R2 to /tmp/ (cached for the process lifetime) and
## returns the local path. No-op if the file was already downloaded this session.
r2_path <- function(object_key) {
  ## basename() keeps the /tmp/ cache flat regardless of folder prefixes in the
  ## R2 object key (e.g. "www/data_for_app.Rds"); the full key is still passed to
  ## save_object so R2 looks in the correct bucket folder
  cached <- file.path("/tmp", basename(object_key))
  if (!file.exists(cached)) {
    aws.s3::save_object(
      object   = object_key
    , bucket   = Sys.getenv("AWS_BUCKET_ID")
    , file     = cached
    , region   = ""
    , key      = Sys.getenv("AWS_ACCESS_KEY_ID")
    , secret   = Sys.getenv("AWS_SECRET_ACCESS_KEY")
    , base_url = Sys.getenv("AWS_S3_ENDPOINT")
    )
  }
  cached
}

#### Shared setup ================================================================

## African country border outlines for the map overlay (loaded once at startup)
countries_sf <- sf::read_sf("countries.geojson") |>
  dplyr::filter(CONTINENT == "Africa") |>
  dplyr::select(NAME)

## Serve the app's own directory so the guide HTML is reachable via /guides/
addResourcePath("guides", ".")

## Plain-language meaning of each forecast-quality class
quality_descriptions <- c(
  "Demonstrated skill"                = "Back-tests here included outbreaks, and the model's probabilities scored better than a flat forecast."
, "Low Forecast Skill"                = "Back-tests here included outbreaks, but the model's probabilities did not score better than a flat forecast. Treat forecasts here with caution."
, "No outbreaks in evaluation period" = "No outbreaks happened here in the back-test period, so how well the model catches outbreaks here cannot be judged."
)

#### App builder =================================================================

## Load an app data file (local or from R2) and return the app
forecast_explorer_app <- function(data_file) {

  app_data <- readRDS(if (LOCAL) data_file else r2_path(file.path("www", data_file)))

  shinyApp(ui = forecast_explorer_ui(app_data), server = forecast_explorer_server(app_data))

}

#### User Interface ===============================================================

forecast_explorer_ui <- function(app_data) {

  meta <- app_data$meta

  fluidPage(
    theme = bs_theme(
        bg           = "#0f1117"
      , fg           = "#e8eaf0"
      , primary      = "#4a9eff"
      , base_font    = font_google("IBM Plex Sans")
      , heading_font = font_google("IBM Plex Mono")
      , bootswatch   = NULL)

    ## various html style pieces
  , tags$head(tags$style(HTML("
      body { background: #0f1117; }

      .app-header {
        padding: 18px 24px 10px;
        border-bottom: 1px solid #2a2d3a;
        margin-bottom: 16px;
      }
      .app-title {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 1.15rem;
        color: #4a9eff;
        letter-spacing: .04em;
        margin: 0;
      }
      .app-subtitle {
        font-size: .78rem;
        color: #6b7280;
        margin: 2px 0 0;
      }

      /* control bar */
      .date-bar {
        background: #16181f;
        border: 1px solid #2a2d3a;
        border-radius: 8px;
        padding: 14px 20px 8px;
        margin-bottom: 14px;
      }
      .irs--shiny .irs-bar          { background: #4a9eff; border-color: #4a9eff; }
      .irs--shiny .irs-handle       { background: #4a9eff; border-color: #fff; }
      .irs--shiny .irs-from,
      .irs--shiny .irs-to,
      .irs--shiny .irs-single       { background: #4a9eff; }
      .irs-grid-text { font-size: 10px !important; color: #9ca3af !important; }
      .irs-line { height: 6px !important; background: #2a2d3a !important; }

      /* map card */
      .map-card {
        background: #16181f;
        border: 1px solid #2a2d3a;
        border-radius: 8px;
        overflow: hidden;
      }
      .card-label {
        font-family: 'IBM Plex Mono', monospace;
        font-size: .7rem;
        color: #6b7280;
        letter-spacing: .06em;
        text-transform: uppercase;
        padding: 8px 14px 4px;
        border-bottom: 1px solid #2a2d3a;
      }

      /* right-panel cards */
      .plot-card {
        background: #16181f;
        border: 1px solid #2a2d3a;
        border-radius: 8px;
        padding: 0;
        overflow: hidden;
        margin-bottom: 12px;
      }
      .plot-inner { padding: 6px 12px 8px; font-size: .82rem; }
      .plot-inner table { width: 100%; font-size: .78rem; margin-bottom: 4px; }
      .plot-inner td, .plot-inner th { border-color: #2a2d3a !important; padding: 3px 6px !important; }
      .quality-badge { font-weight: 600; font-size: .9rem; }
      .muted { color: #9ca3af; font-size: .75rem; }
      details summary { cursor: pointer; color: #9ca3af; font-size: .75rem; }

      /* instruction nudge */
      .nudge {
        font-size: .75rem;
        color: #6b7280;
        font-style: italic;
        padding: 4px 0 6px 2px;
      }

      /* leaflet dark basemap tint */
      .leaflet-container { background: #0f1117 !important; }
      .leaflet-control .legend, .info.legend { background: #16181f; color: #e8eaf0; }
    ")))

  , tabsetPanel(

      ## MAIN TAB (forecast map with per-unit context) ==============================
      tabPanel(

        title = "Forecasts"

        ## header
      , div(class = "app-header"
          , p(class = "app-title", paste0("RVF Forecast Explorer  ·  ", tools::toTitleCase(meta$units_label)))
          , p(class = "app-subtitle", paste0(
              "Forward forecasts of an RVF outbreak in each ", meta$unit_label
            , "  ·  forecast quality and outbreak history from the training run of ", meta$context_run_date)))

        ## forecast date, window and mask controls
      , div(class = "date-bar"
          , fluidRow(
              column(3
                , selectInput("forecast_date", "Forecast base date"
                            , choices = rev(as.character(meta$forecast_dates)), selected = as.character(max(meta$forecast_dates))))
            , column(6
                , shinyWidgets::sliderTextInput(
                    inputId  = "window"
                  , label    = "Forecast window"
                  , choices  = meta$forecast_windows$window_label
                  , selected = meta$forecast_windows$window_label[1]
                  , width    = "100%"
                  , grid     = TRUE))
            , column(3
                , checkboxInput("mask", "Show forecast-quality mask", value = TRUE)
                , div(class = "muted", "Faded: skill cannot be judged. Dashed outline: low forecast skill."))))

        ## main layout
      , fluidRow(

          ## left: forecast map
          column(
            7
          , div(class = "map-card"
              , div(class = "card-label", paste0("P(outbreak) by ", meta$unit_label, "  ·  click a ", meta$unit_label, " for its context"))
              , leafletOutput("map", height = "720px")))

          ## right: context of the clicked unit
        , column(
            5
          , div(class = "nudge", uiOutput("click_hint"))
          , div(class = "plot-card"
              , div(class = "card-label", "Forecast")
              , div(class = "plot-inner", uiOutput("unit_header"), tableOutput("forecast_table")
                    , plotOutput("forecast_plot", height = "170px")))
          , div(class = "plot-card"
              , div(class = "card-label", "Forecast quality")
              , div(class = "plot-inner", uiOutput("quality_badge"), tableOutput("quality_table")))
          , div(class = "plot-card"
              , div(class = "card-label", "Outbreak history")
              , div(class = "plot-inner", uiOutput("history_text"), tableOutput("history_table"))))

        )
      )

      ## GUIDE TAB ====================================================================
    , tabPanel(
        title = "User Guide"
      , tags$iframe(
          src   = "guides/forecast_explorer_guide.html"
        , style = "width:100%; height:85vh; border:none; background:#fff;"
        )
      )

    )
  )

}

#### Server ======================================================================

forecast_explorer_server <- function(app_data) {

  meta      <- app_data$meta
  windows   <- meta$forecast_windows
  forecasts <- app_data$forecasts
  context   <- app_data$context
  outbreaks <- app_data$outbreaks

  ## Units with the context columns used to style the map
  map_units <- app_data$geometry |>
    dplyr::left_join(context |> dplyr::select(unit_id, quality_class, history_class), by = "unit_id")

  ## Colour scale on log10 probability; limits saved with the training-run context (so colours
   ## match the static maps), falling back to the range of the forecasts
  fill_limits <- meta$fill_limits %||% range(forecasts$prob[forecasts$prob > 0], na.rm = TRUE)
  log_limits  <- log10(fill_limits)
  prob_palette <- colorNumeric(viridisLite::viridis(256), domain = log_limits, na.color = "#3a3d4a")
  ## Probabilities outside the limits (including exact zeros) take the end colours
  prob_colour  <- function(p) prob_palette(pmin(pmax(log10(p), log_limits[1]), log_limits[2]))

  function(input, output, session) {

    ## Reactive: selected base date and window
    selected_date   <- reactive({ as.Date(input$forecast_date) })
    selected_window <- reactive({ windows$forecast_interval[match(input$window, windows$window_label)] })

    ## Reactive: all forecasts for the base date, with each unit's percentile rank in its window
    date_forecasts <- reactive({
      forecasts |>
        dplyr::filter(date == selected_date()) |>
        dplyr::mutate(pct_rank = dplyr::percent_rank(prob), .by = forecast_interval)
    })

    ## Reactive: clicked unit id
    clicked_unit <- reactiveVal(NULL)

    ## Map frame; panes keep country borders and the selection outline above the redrawn units
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = TRUE)) |>
        addMapPane("land", zIndex = 400) |>
        addMapPane("units", zIndex = 410) |>
        addMapPane("borders", zIndex = 420) |>
        addMapPane("selected", zIndex = 430) |>
        ## No basemap tiles (no external tile service or API key); a faint land fill under the
         ## units separates land from the dark background
        addPolygons(
          data        = countries_sf
        , stroke      = FALSE
        , fillColor   = "#1f222b"
        , fillOpacity = 1
        , options     = pathOptions(pane = "land", interactive = FALSE)) |>
        ## Country border outlines on top of unit fills, non-interactive
        addPolygons(
          data    = countries_sf
        , fill    = FALSE
        , color   = "#9ca3af"
        , weight  = 1.2
        , opacity = 0.9
        , options = pathOptions(pane = "borders", interactive = FALSE)) |>
        setView(lng = 20, lat = 0, zoom = 3)
    })

    ## Redraw units when the date, window or mask changes
    shiny::observe({

      req(input$map_bounds)

      d <- map_units |>
        dplyr::left_join(date_forecasts() |> dplyr::filter(forecast_interval == selected_window()), by = "unit_id")

      ## Mask: fade units whose skill cannot be judged, dashed outline on low-skill units
      faded    <- input$mask & d$quality_class %in% "No outbreaks in evaluation period"
      low_skill <- input$mask & d$quality_class %in% "Low Forecast Skill"

      labels <- paste0(
        "<b>", htmltools::htmlEscape(d$unit_name), "</b><br>"
      , "P(outbreak) = ", format_prob(d$prob), "<br>"
      , "<span style='color:#9ca3af;'>", dplyr::coalesce(as.character(d$quality_class), "Not scored"), "</span>") |>
        lapply(htmltools::HTML)

      leafletProxy("map") |>
        clearGroup("units") |>
        clearControls() |>
        addPolygons(
          data        = d
        , layerId     = ~unit_id
        , group       = "units"
        , fillColor   = prob_colour(d$prob)
        , fillOpacity = ifelse(faded, 0.2, 0.85)
        , color       = ifelse(low_skill, "#ffffff", "#2a2d3a")
        , weight      = ifelse(low_skill, 1.6, 0.5)
        , dashArray   = ifelse(low_skill, "4 3", "")
        , opacity     = 1
        , label       = labels
        , highlightOptions = highlightOptions(weight = 2, color = "#ffffff", fillOpacity = 0.95, bringToFront = FALSE)
        , options     = pathOptions(pane = "units")) |>
        ## Legend with the highest probabilities on top
        addLegend(
          position = "bottomright"
        , colors   = rev(prob_palette(legend_breaks(log_limits)))
        , labels   = rev(format_prob(10^legend_breaks(log_limits)))
        , title    = "P(outbreak)"
        , opacity  = 0.9)

    })

    ## Click: remember the unit and outline it
    observeEvent(input$map_shape_click, {
      clicked_unit(input$map_shape_click$id)

      leafletProxy("map") |>
        clearGroup("selected") |>
        addPolygons(
          data    = map_units |> dplyr::filter(unit_id == clicked_unit())
        , fill    = FALSE
        , color   = "#ffff00"
        , weight  = 3
        , group   = "selected"
        , options = pathOptions(pane = "selected", interactive = FALSE))
    })

    ## Hint text
    output$click_hint <- renderUI({
      if (is.null(clicked_unit())) {
        span(paste0("← Select a ", meta$unit_label, " on the map to see its forecast, forecast quality and outbreak history"))
      } else {
        span(style = "color:#4a9eff;", paste0("Showing ", unit_info(clicked_unit())$unit_name))
      }
    })

    ## Geometry row of the clicked unit
    unit_info <- function(id) map_units |> sf::st_drop_geometry() |> dplyr::filter(unit_id == id)

    ## FORECAST: name, forecast by window and a small profile plot
    output$unit_header <- renderUI({
      req(clicked_unit())
      u <- unit_info(clicked_unit())
      tagList(
        div(class = "quality-badge", u$unit_name)
      , div(class = "muted", dplyr::coalesce(u$country_name, u$country, "")
          , if (!is.null(u$adm_km2)) paste0("  ·  ", format(round(u$adm_km2), big.mark = ","), " km²"))
        ## Regions: the full list of districts merged into the region
      , if (!is.null(u$districts)) tags$details(tags$summary("Districts in this region"), div(class = "muted", u$districts))
      )
    })

    output$forecast_table <- renderTable({
      req(clicked_unit())
      unit_forecast_table(date_forecasts() |> dplyr::filter(unit_id == clicked_unit()), windows, meta$units_label)
    }, sanitize.text.function = identity)

    output$forecast_plot <- renderPlot({
      req(clicked_unit())
      unit_forecast_plot(
        date_forecasts() |> dplyr::filter(unit_id == clicked_unit())
      , windows, selected_window(), fill_limits
      , typical = if ("median_prob" %in% names(context)) context$median_prob[context$unit_id == clicked_unit()])
    }, bg = "#16181f")

    ## FORECAST QUALITY: class, what it means and the scores behind it
    output$quality_badge <- renderUI({
      req(clicked_unit())
      cls <- as.character(context$quality_class[context$unit_id == clicked_unit()])
      if (!length(cls) || is.na(cls)) {
        return(div(class = "muted", paste0("Not scored: no back-test predictions for this ", meta$unit_label, ".")))
      }
      tagList(div(class = "quality-badge", style = paste0("color:", quality_colour(cls), ";"), cls)
            , div(class = "muted", quality_descriptions[[cls]]))
    })

    output$quality_table <- renderTable({
      req(clicked_unit())
      unit_quality_table(context |> dplyr::filter(unit_id == clicked_unit()), nrow(context), meta)
    }, sanitize.text.function = identity)

    ## OUTBREAK HISTORY: summary sentence and outbreaks by year
    output$history_text <- renderUI({
      req(clicked_unit())
      unit_history_text(outbreaks |> dplyr::filter(unit_id == clicked_unit()), meta)
    })

    output$history_table <- renderTable({
      req(clicked_unit())
      unit_history_table(outbreaks |> dplyr::filter(unit_id == clicked_unit()))
    })

  }

}

#### Helper functions ============================================================

## Probabilities as percentages with two significant digits
format_prob <- function(p) {
  ifelse(is.na(p), "no forecast", ifelse(p < 1e-5, "< 0.001%", paste0(signif(100 * p, 2), "%")))
}

## Six evenly spaced legend values on the log10 scale
legend_breaks <- function(log_limits) seq(log_limits[1], log_limits[2], length.out = 6)

## Badge colour for each forecast-quality class
quality_colour <- function(cls) {
  c("Demonstrated skill" = "#4ade80", "Low Forecast Skill" = "#f59e0b", "No outbreaks in evaluation period" = "#9ca3af")[[cls]]
}

## Forecast by window for one unit: probability, its rank among all units and, for regions,
## the range between perfect dependence and independence of the unit's hexes
unit_forecast_table <- function(f, windows, units_label) {

  if (!nrow(f)) return(data.frame(Note = "No forecast for this base date (too little of the area is covered by predicted hexes)."))

  out <- windows |>
    dplyr::left_join(f, by = "forecast_interval") |>
    dplyr::transmute(
      Window            = window_label
    , `P(outbreak)`     = format_prob(prob)
    , `Rank`            = ifelse(is.na(pct_rank), "", paste0("top ", pmax(1, round(100 * (1 - pct_rank))), "% of ", units_label))
    )

  if ("prob_lower" %in% names(f)) {
    ## Header with a hover tooltip (the table is rendered without escaping html)
    bounds_header <- paste0("<span title='Not an uncertainty interval: P(any outbreak) if all of the region",
                            "&#39;s hexes always had outbreaks together (largest hex) and if they had outbreaks independently' ",
                            "style='text-decoration: underline dotted; cursor: help;'>Dependence bounds (max hex &ndash; independence)</span>")
    out[[bounds_header]] <- paste0(format_prob(windows |> dplyr::left_join(f, by = "forecast_interval") |> dplyr::pull(prob_lower))
                                  , " - ", format_prob(windows |> dplyr::left_join(f, by = "forecast_interval") |> dplyr::pull(prob_upper)))
  }

  out

}

## Forecast profile across windows for one unit (log scale), with the selected window
## highlighted and, for hexes, the hex's typical test-period probability
unit_forecast_plot <- function(f, windows, selected_window, fill_limits, typical = NULL) {

  shiny::validate(need(nrow(f) > 0, "No forecast for this base date."))

  d <- windows |> dplyr::left_join(f, by = "forecast_interval") |> dplyr::mutate(prob = pmax(prob, fill_limits[1]))

  ggplot(d, aes(forecast_interval, prob)) +
    {
      if (length(typical) && !is.na(typical))
        geom_hline(yintercept = max(typical, fill_limits[1]), colour = "#9ca3af", linetype = "dashed", linewidth = 0.5)
      else list()
    } +
    geom_line(colour = "#4a9eff", linewidth = 1) +
    geom_point(colour = "#ffffff", fill = "#4a9eff", shape = 21, size = 2.4, stroke = 1) +
    geom_point(data = d |> dplyr::filter(forecast_interval == selected_window), colour = "#ffff00", shape = 21, size = 4, stroke = 1.4) +
    scale_x_continuous(name = NULL, breaks = d$forecast_interval, labels = d$window_label) +
    scale_y_log10(name = "P(outbreak)", labels = function(x) format_prob(x)) +
    labs(caption = if (length(typical) && !is.na(typical)) "Dashed line: this hex's typical (median) test-period probability" else NULL) +
    theme_minimal(base_family = "IBM Plex Sans") +
    theme(
        plot.background  = element_rect(fill = "#16181f", colour = NA)
      , panel.background = element_rect(fill = "#16181f", colour = NA)
      , panel.grid.major = element_line(colour = "#2a2d3a", linewidth = 0.4)
      , panel.grid.minor = element_blank()
      , axis.text        = element_text(colour = "#9ca3af", size = 8)
      , axis.title       = element_text(colour = "#e8eaf0", size = 9)
      , plot.caption     = element_text(colour = "#9ca3af", size = 7)
      , plot.margin      = margin(4, 10, 2, 4))

}

## Forecast-quality scores of one unit as a two-column table
unit_quality_table <- function(ctx, n_units, meta) {

  if (!nrow(ctx) || is.na(ctx$skill_source)) return(NULL)

  source_label <- c(
    outer_folds = "Back-tests before the test period (outer folds)"
  , test        = paste0("Test period (from ", meta$history_end + 1, ")"))[[ctx$skill_source]]

  rows <- list(
    "Skill judged on"                      = source_label
  , "Predictions scored (dates x windows)" = format(ctx$n_rows, big.mark = ",")
  , "Of which had an outbreak"             = format(ctx$n_events, big.mark = ",")
  , "Log-loss gain over a flat forecast"   = paste0(sprintf("%+.4f", ctx$logloss_gain), " (positive = better than flat)")
  , "Skill rank"                           = paste0(ctx$skill_rank, " of ", n_units, " (1 = largest gain)")
  , "Test-period gain over flat"           = if (is.na(ctx$test_logloss_gain)) "not scored" else sprintf("%+.4f", ctx$test_logloss_gain)
  )

  ## Hex-only test-period columns
  if ("within_hex_auc" %in% names(ctx)) {
    rows <- c(rows, list(
      "Timing skill (within-hex AUC, test)" = if (is.na(ctx$within_hex_auc)) "no test outbreaks" else paste0(sprintf("%.2f", ctx$within_hex_auc), " (0.5 = no timing skill)")
    , "Gain over its own history (test)"    = if (is.na(ctx$logloss_gain_vs_ref)) "not scored" else paste0(sprintf("%+.4f", ctx$logloss_gain_vs_ref), " (positive = better than the hex's historical rate)")
    , "Typical probability (test median)"   = format_prob(ctx$median_prob)
    ))
  }

  data.frame(Measure = names(rows), Value = unlist(rows), row.names = NULL, check.names = FALSE)

}

## Summary sentence of a unit's labelled outbreaks
unit_history_text <- function(ob, meta) {

  if (!nrow(ob)) {
    return(div(paste0("No labelled outbreaks (cases > 0) recorded in this ", meta$unit_label, ".")))
  }

  years   <- sort(unique(as.integer(format(ob$start_date, "%Y"))))
  n_prior <- sum(ob$start_date <= meta$history_end)

  div(
    paste0(
      dplyr::n_distinct(ob$outbreak_id), " labelled outbreak", if (dplyr::n_distinct(ob$outbreak_id) > 1) "s", " (cases > 0) in "
    , length(years), " year", if (length(years) > 1) "s", " (", year_list(years), "); "
    , "most recent started ", format(max(ob$start_date), "%d %b %Y"), ".")
  , div(class = "muted", paste0(
      n_prior, " started on or before ", format(meta$history_end, "%d %b %Y")
    , " (the outbreak history behind the forecast-quality context)."))
  )

}

## Years as a short list ("2008, 2010 and 2018"), or a range with a count when there are many
year_list <- function(years) {
  if (length(years) > 5) return(paste0(min(years), " to ", max(years)))
  if (length(years) == 1) return(as.character(years))
  paste0(paste(utils::head(years, -1), collapse = ", "), " and ", utils::tail(years, 1))
}

## Labelled outbreaks of a unit by year, most recent first
unit_history_table <- function(ob) {

  if (!nrow(ob)) return(NULL)

  ob |>
    dplyr::mutate(Year = as.integer(format(start_date, "%Y"))) |>
    dplyr::summarize(
      `# of times cases recorded` = dplyr::n_distinct(outbreak_id)
    , Cases            = format(sum(cases, na.rm = TRUE), big.mark = ",")
    , `First started`  = format(min(start_date), "%d %b")
    , `Last started`   = format(max(start_date), "%d %b")
    , .by = Year) |>
    dplyr::arrange(dplyr::desc(Year))

}
