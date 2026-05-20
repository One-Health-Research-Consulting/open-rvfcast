####
## App to explore predictions across various aggregation levels (space and forecast window)
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

#### Read in needed data =========================================================

## targets pipeline puts this is the correct folder
## examined_fits_within_country saves component prediction files to outpath_for_app
## then built_app_components saves "data_for_app.Rds" to "www/"
map_dat          <- readRDS(if (LOCAL) {
    "app_data_processed.Rds"
  } else {
    r2_path("www/app_data_processed.Rds")
  })

parent_lookup <- map_dat$parent_lookup
no_agg_with_big_hexes <- map_dat$no_agg_with_big_hexes
avg_map_data <- map_dat$avg_map_data
cal.double_agg <- map_dat$cal.double_agg
dat.no_agg <- map_dat$dat.no_agg
parent_lookup <- map_dat$parent_lookup
dat.temporal_agg <- map_dat$dat.temporal_agg
dat.spatial_agg <- map_dat$dat.spatial_agg
shap.dat        <- map_dat$shap.dat

## Country border outlines for map overlay (loaded once at startup)
countries_sf    <- sf::read_sf("countries.geojson")

#### Various setup needs =========================================================

## Build colours
prob_palette <- colorNumeric(
  palette  = c("#f7fbff", "#6baed6", "#08306b")  ## white -- steel -- navy
, domain   = c(0, 1)                             ## predictions are probabilities
, na.color = "#cccccc"
)

true_out_color <- "#e34a33"   ## bold orange-red outline for outbreaks that occurred

## Deal with dates for slider bar
sorted_dates <- sort(unique(dat.temporal_agg$date))
date_index   <- data.frame(idx = seq_along(sorted_dates), date = sorted_dates)

## For labeling of dates for side plots
num_lab <- scales::label_number(accuracy = 0.01)

## Unique forecast intervals
sorted_forecast_intervals <- unique(dat.spatial_agg$forecast_interval)

## Serve the app's own directory so guide HTML files are reachable via /guides/
addResourcePath("guides", ".")

#### User Interface ===============================================================
ui <- fluidPage(
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

    /* date slider */
    .date-bar {
      background: #16181f;
      border: 1px solid #2a2d3a;
      border-radius: 8px;
      padding: 14px 20px 8px;
      margin-bottom: 14px;
    }
    .irs--shiny .irs-bar          { width: 14px !important; height: 14px !important; top: 18px !important; background: #4a9eff; border-color: #4a9eff; }
    .irs--shiny .irs-handle       { background: #4a9eff; border-color: #fff; }
    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single       { background: #4a9eff; }

    .irs { width: 100% !important; }
    .irs--shiny .irs { height: 60px !important; }
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

    /* right-panel plots */
    .plot-card {
      background: #16181f;
      border: 1px solid #2a2d3a;
      border-radius: 8px;
      padding: 0;
      overflow: hidden;
      margin-bottom: 12px;
    }
    .plot-card .card-label {
      padding: 8px 14px 4px;
    }
    .plot-inner { padding: 4px 8px 8px; }

    /* instruction nudge */
    .nudge {
      font-size: .75rem;
      color: #6b7280;
      font-style: italic;
      padding: 4px 0 6px 2px;
    }

    /* leaflet dark basemap tint */
    .leaflet-container { background: #0f1117 !important; }
  ")))

, tabsetPanel(

    ## MAIN TAB (map over time)
    tabPanel(

    title = "Predictions over through time"

    ## header
  , div(class = "app-header"
        , p(class = "app-title",  "Prediction Explorer")
        , p(class = "app-subtitle", "Spatial · Temporal · Multi-scale"))

    ## forecast interval slider
  , div(class = "date-bar"
        , shinyWidgets::sliderTextInput(
          inputId  = "selected_date"
        , label    = NULL
        , choices  = as.character(sorted_dates)
        , selected = as.character(sorted_dates[1])
        , width    = "100%"
        , animate  = TRUE
        , grid     = TRUE
        )
  )

  , div(class = "date-bar"
          , shinyWidgets::sliderTextInput(
            inputId  = "selected_forecast_interval_tab1"
          , label    = NULL
          , choices  = as.character(sorted_forecast_intervals)
          , selected = as.character(sorted_forecast_intervals[1])
          , width    = "100%"
          , animate  = FALSE
          , grid     = TRUE
          )
    )

  ## main layout
, fluidRow(

    ## left: primary map
    column(
      6
      , div(class = "map-card"
            , div(class = "card-label", "Date-Specific Forecast  ·  click a hex to drill down")
            , leafletOutput("main_map", height = "620px")))

    ## detail panels
    , column(
      3
      , div(class = "nudge", uiOutput("click_hint_one"))
      , div(class = "plot-card"
            , div(class = "card-label", "Sub-Hex Spatial Detail")
            , div(class = "plot-inner", plotOutput("subhex_plot", height = "280px")
            ))
      , div(class = "plot-card"
            , div(class = "card-label", "Forecast Horizon Profile")
            , div(class = "plot-inner", plotOutput("timeseries_plot", height = "280px")
            )))

    ## second layer of detail panels
    , column(
        3
      , div(class = "plot-card"
            , div(class = "card-label", "Probability Distributions")
            , div(class = "plot-inner", plotOutput("density_plot", height = "280px")
            ))
      , div(class = "plot-card"
            , div(class = "card-label", "Calibration Curve")
            , div(class = "plot-inner", plotOutput("calibration_plot", height = "280px")
            )))

  ))

  ## SECOND TAB (hex specific forecasts over time) =================================
, tabPanel(

   title = "Predictions by Forecast Time Horizon and Hex Over Time"

    , fluidRow(

      ## left: primary map
      column(
        6
        , div(class = "map-card"
              , div(class = "card-label", "Average p(outbreak)  ·  click a hex to drill down")
              , leafletOutput("average_map", height = "620px")))

      ## detail panels
      , column(
        6
        , div(class = "nudge", uiOutput("click_hint_two"))
        , div(class = "plot-card"
              , div(class = "card-label", "Parent Hex-Specific Forecast Time Series")
              , div(class = "plot-inner", plotOutput("hex_timeseries_full_parent", height = "310px")
              ))
        , div(class = "plot-card"
              , div(class = "card-label", "Children Hex-Specific Forecast Time Series")
              , div(class = "plot-inner", plotOutput("hex_timeseries_full_child", height = "310px")
              ))
    ))

  )

## THIRD TAB (More detailed forecast diagnostics) =================================
, tabPanel(

  title = "Error Rates by Forecast Time Horizon"

  , fluidPage(

    div(class = "app-header",
        p(class = "app-title", "Error by Forecast Horizon"),
        p(class = "app-subtitle", "False-positive and negative rates | forecast horizon"))

  , div(class = "date-bar"
          , shinyWidgets::sliderTextInput(
            inputId  = "selected_forecast_interval_tab3"
          , label    = NULL
          , choices  = as.character(sorted_forecast_intervals)
          , selected = as.character(sorted_forecast_intervals[1])
          , width    = "100%"
          , animate  = FALSE
          , grid     = TRUE
          )
    )

  , fluidRow(

    ## left: primary map
    column(
      6
      , div(class = "map-card"
            , div(class = "card-label", "Average p(sqrt(outbreak))  ·  click a hex to drill down")
            , leafletOutput("average_map_big_hexes", height = "620px")))

    ## detail panels
    , column(
      6
      , div(class = "nudge", uiOutput("click_hint_three"))
      , div(class = "plot-card"
            , div(class = "card-label", "Overall Predicted Outbreak Probability across Forecast Horizons")
            , div(class = "plot-inner", plotOutput("interval_preds_overall", height = "310px")
            ))
      , div(class = "plot-card"
            , div(class = "card-label", "Hex-Specific Predicted Outbreak Probability for a Forecast Horizon")
            , div(class = "plot-inner", plotOutput("interval_preds_hex_single", height = "310px")
            ))
    ))

  ))

## FOURTH TAB (Forecast-specific variable importance) ==============================
, tabPanel(

  title = "Variable Importance by Forecast Horizon"

  , fluidPage(

    div(class = "app-header",
        p(class = "app-title", "Variable Importance by Forecast Horizon"),
        p(class = "app-subtitle", "VI | forecast horizon"))

  , fluidRow(

    ## left: SHAP heatmap
    column(
      6
      , div(class = "plot-card"
            , div(class = "card-label", "Shap variable importance")
            , div(class = "plot-inner", plotOutput("shap_heatmap_raw", height = "600px")
            ))
    )

    ## right: combined SHAP lineplot
    , column(
      6
      , div(class = "plot-card"
            , div(class = "card-label", "Shap feature importance by forecast horizon")
            , div(class = "plot-inner", plotOutput("shap_lineplot", height = "600px")
            ))
    ))

  ))

## GUIDE TAB ====================================================================
, tabPanel(
    title = "User Guide"
  , tags$iframe(
      src   = "guides/prediction_explorer_guide.html"
    , style = "width:100%; height:85vh; border:none; background:#fff;"
    )
  )

  )

)

#### Server ======================================================================
server <- function(input, output, session) {

  ## Reactive: parse sliderTextInput text back to Date
  selected_date <- reactive({
    as.Date(input$selected_date)
  })

  ## Reactive: forecast interval for tab1
  selected_forecast_interval_tab1 <- reactive({
    input$selected_forecast_interval_tab1
  })

  ## Reactive: forecast interval for tab3
  selected_forecast_interval_tab3 <- reactive({
    input$selected_forecast_interval_tab3
  })

  ## Reactive: date-filtered spatial-agg layer
  map_data <- reactive({
    dat.spatial_agg |> filter(date == selected_date(), forecast_interval == selected_forecast_interval_tab1())
  })

  calibration_filtered <- reactive({
    cal.double_agg |> filter(selected_date() >= as.Date(min_date), selected_date() <= as.Date(max_date))
  })

  ## Reactive: probabilities for a given forecast window
  forecast_filtered <- reactive({
    no_agg_with_big_hexes |> filter(forecast_interval == selected_forecast_interval_tab3())
  })

  ## Reactive: forecast-interval-filtered map data for Tab 3 (aggregated over time within horizon)
  avg_map_data_fi <- reactive({
    dat.spatial_agg |>
      filter(forecast_interval == selected_forecast_interval_tab3()) |>
      group_by(region_norm) |>
      summarise(
        prob_pred   = mean(prob_pred, na.rm = TRUE),
        an_outbreak = max(true_out),
        .groups     = "drop"
      ) |>
      rename(small_ids = region_norm)
  })

  ## Reactive: clicked hex id
  clicked_hex <- reactiveVal(NULL)
  clicked_hex_ts <- reactiveVal(NULL)
  clicked_hex_fi <- reactiveVal(NULL)

  observeEvent(input$main_map_shape_click, {
    clicked_hex(input$main_map_shape_click$id)

    leafletProxy("main_map") |>
      clearGroup("selected") |>
      addPolygons(
        data = map_data() |> filter(region_norm == input$main_map_shape_click$id)
      , fill = FALSE
      , color = "#ffff00"
      , weight = 3
      , group = "selected"
      )

  })
  observeEvent(input$average_map_shape_click, {
    clicked_hex_ts(input$average_map_shape_click$id)

    leafletProxy("average_map") |>
      clearGroup("selected") |>
      addPolygons(
        data = avg_map_data |> filter(region_norm == clicked_hex_ts())
      , fill = FALSE
      , color = "#ffff00"
      , weight = 3
      , group = "selected"
      )

  })
  observeEvent(input$average_map_big_hexes_shape_click, {
    clicked_hex_fi(input$average_map_big_hexes_shape_click$id)

    leafletProxy("average_map_big_hexes") |>
      clearGroup("selected") |>
      addPolygons(
        data = avg_map_data_fi() |>
          filter(small_ids == clicked_hex_fi())
      , fill = FALSE
      , color = "#ffff00"
      , weight = 3
      , group = "selected"
      )

  })

  ## Hint text
  output$click_hint_one   <- renderUI({
    if (is.null(clicked_hex())) {
      span("← Select a hex on the map to reveal drill-down panels")
    } else {
      span(style = "color:#4a9eff;",
           paste0("Tracking hex ", clicked_hex(), " across time"))
    }
  })
  output$click_hint_two   <- renderUI({
    if (is.null(clicked_hex_ts())) {
      span("← Select a hex on the map to reveal forecast-interval time series")
    } else {
      span(style = "color:#4a9eff;",
           paste0("Tracking hex ", clicked_hex_ts()))
    }
  })
  output$click_hint_three <- renderUI({
    if (is.null(clicked_hex_fi())) {
      span("← Select a hex on the map to reveal forecast-interval predictions")
    } else {
      span(style = "color:#4a9eff;",
           paste0("Tracking hex ", clicked_hex_fi()))
    }
  })

  ## PANEL 1: Leaflet map
  output$main_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(
        providers$CartoDB.DarkMatterNoLabels,
        options = tileOptions(opacity = 0.85)
      ) |>
      setView(lng = 15, lat = -4, zoom = 3)
  })

  ## PANEL 1: Update polygons when date or data changes
  shiny::observe({

    req(input$main_map_bounds)

    d <- map_data()

    ## Split into event / non-event for outline styling
    events     <- d |> filter(true_out == 1)
    non_events <- d |> filter(true_out != 1)

    proxy <- leafletProxy("main_map") |>
      clearShapes() |>
      clearControls()

    ## Non-event hexes
    if (nrow(non_events) > 0) {
      proxy <- proxy |>
        addPolygons(
          data        = non_events
        , layerId     = ~region_norm
        , fillColor    = ~prob_palette(prob_pred)
        , fillOpacity  = 0.6
        , color       = "#2a2d3a"
        , weight      = 0.8
        , opacity     = 1
        , highlightOptions = highlightOptions(
            weight       = 2
          , color        = "#ffffff"
          , fillOpacity   = 0.85
          , bringToFront = TRUE)
          , label = ~paste0(
            "<b>", region_norm, "</b><br>"
          , "P(event) = ", round(prob_pred, 3)) |>
            lapply(htmltools::HTML)
        )
    }

    ## Event hexes – bold red outline
    if (nrow(events) > 0) {
      proxy <- proxy |>
        addPolygons(
          data        = events
        , layerId     = ~region_norm
        , fillColor   = ~prob_palette(prob_pred)
        , fillOpacity = 0.75
        , color       = true_out_color
        , weight      = 2.5
        , opacity     = 1
        , highlightOptions = highlightOptions(
            weight      = 3.5
          , color       = "#ffffff"
          , fillOpacity = 0.9
          , bringToFront = TRUE)
          , label = ~paste0(
            "<b>", region_norm, "</b><br>",
            "P(event) = ", round(prob_pred, 3), "<br>",
            "<span style='color:", true_out_color, ";'>● Event observed</span>"
          ) |> lapply(htmltools::HTML)
        )
    }

    ## Legend
    proxy |>
      addLegend(
        position = "bottomright"
      , pal      = prob_palette
      , values   = c(0, 1)
      , title    = "P(event)"
      , opacity  = 0.85
      , labFormat = labelFormat(digits = 2)
      ) |>
      addControl(
        html = paste0(
          "<div style='background:#16181f;color:#e34a33;",
          "padding:4px 8px;border-radius:4px;font-size:11px;",
          "border:1px solid #e34a33;'>",
          "&#9646; Event observed</div>")
        , position = "bottomright")

    ## Country border outlines on top of hex fills, non-interactive
    leafletProxy("main_map") |>
      addPolygons(
        data        = countries_sf
      , fill        = FALSE
      , color       = "#9ca3af"
      , weight      = 1.5
      , opacity     = 0.9
      , options     = pathOptions(interactive = FALSE)
      )

    if (!is.null(clicked_hex())) {
      leafletProxy("main_map") |>
        addPolygons(
          data = d |> filter(region_norm == clicked_hex())
        , fill = FALSE
        , color = "#ffff00"
        , weight = 3
        , group = "selected"
        )
    }

  })

  ## PANEL 2: Average map for the second panel
  output$average_map <- renderLeaflet({

    leaflet(avg_map_data) |>
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        options = tileOptions(opacity = 0.85)
      ) |>
      setView(lng = 15, lat = -4, zoom = 3)

  })

  ## PANEL 2: Update polygons when date or data changes
  shiny::observe({

    req(input$average_map_bounds)

    ## Split into event / non-event for outline styling
    events     <- avg_map_data |> filter(an_outbreak == 1)
    non_events <- avg_map_data |> filter(an_outbreak != 1)

    proxy <- leafletProxy("average_map") |>
      clearShapes() |>
      clearControls()

    ## Non-event hexes
    if (nrow(non_events) > 0) {
      proxy <- proxy |>
        addPolygons(
          data        = non_events
        , layerId     = ~region_norm
        , fillColor   = ~prob_palette(prob_pred)
        , fillOpacity = 0.75
        , color       = "#2a2d3a"
        , weight      = 0.8
        , opacity     = 1
        , highlightOptions = highlightOptions(
            weight      = 2
          , color       = "#ffffff"
          , fillOpacity = 0.9
          , bringToFront = TRUE)
          , label = ~paste0(
            "<b>", region_norm, "</b><br>"
            , "Mean P(event) = ", round(prob_pred, 3)) |>
            lapply(htmltools::HTML)
        )
    }

    ## Event hexes – bold red outline
    if (nrow(events) > 0) {
      proxy <- proxy |>
        addPolygons(
          data        = events
          , layerId     = ~region_norm
          , fillColor   = ~prob_palette(prob_pred)
          , fillOpacity = 0.75
          , color       = true_out_color
          , weight      = 2.5
          , opacity     = 1
          , highlightOptions = highlightOptions(
            weight      = 3.5
            , color       = "#ffffff"
            , fillOpacity = 0.9
            , bringToFront = TRUE)
          , label = ~paste0(
            "<b>", region_norm, "</b><br>",
            "Mean P(event) = ", round(prob_pred, 3), "<br>",
            "<span style='color:", true_out_color, ";'>● Event observed</span>"
          ) |> lapply(htmltools::HTML)
        )
    }

    ## Legend
    proxy |>
      addLegend(
        position = "bottomright"
      , pal      = prob_palette
      , values   = c(0, 1)
      , title    = "P(event)"
      , opacity  = 0.85
      , labFormat = labelFormat(digits = 2)
      ) |>
      addControl(
        html = paste0(
          "<div style='background:#16181f;color:#e34a33;",
          "padding:4px 8px;border-radius:4px;font-size:11px;",
          "border:1px solid #e34a33;'>",
          "&#9646; Event observed</div>")
        , position = "bottomright")

    if (!is.null(clicked_hex_ts())) {
      leafletProxy("average_map") |>
        addPolygons(
          data = avg_map_data |> filter(region_norm == clicked_hex_ts())
        , fill = FALSE
        , color = "#ffff00"
        , weight = 3
        , group = "selected"
        )
    }

  })

  ## PANEL 3: Average map for the third panel
  output$average_map_big_hexes <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        options = tileOptions(opacity = 0.85)
      ) |>
      setView(lng = 15, lat = -4, zoom = 3)
  })

  ## PANEL 3: Update polygons when forecast interval or data changes
  shiny::observe({

    req(input$average_map_big_hexes_bounds)

    d <- avg_map_data_fi()

    ## Split into event / non-event for outline styling
    events     <- d |> filter(an_outbreak == 1)
    non_events <- d |> filter(an_outbreak != 1)

      proxy <- leafletProxy("average_map_big_hexes") |>
        clearShapes() |>
        clearControls()

      ## Non-event hexes
      if (nrow(non_events) > 0) {
        proxy <- proxy |>
          addPolygons(
            data        = non_events
          , layerId     = ~small_ids
          , fillColor   = ~prob_palette(prob_pred)
          , fillOpacity = 0.75
          , color       = "#2a2d3a"
          , weight      = 0.8
          , opacity     = 1
          , highlightOptions = highlightOptions(
              weight      = 2
            , color       = "#ffffff"
            , fillOpacity = 0.9
            , bringToFront = TRUE)
            , label = ~paste0(
              "<b>", small_ids, "</b><br>"
            , "Mean P(event) = ", round(prob_pred, 3)) |>
               lapply(htmltools::HTML)
          )
      }

      ## Event hexes – bold red outline
      if (nrow(events) > 0) {
        proxy <- proxy |>
          addPolygons(
            data        = events
          , layerId     = ~small_ids
          , fillColor   = ~prob_palette(prob_pred)
          , fillOpacity = 0.75
          , color       = true_out_color
          , weight      = 2.5
          , opacity     = 1
          , highlightOptions = highlightOptions(
              weight      = 3.5
            , color       = "#ffffff"
            , fillOpacity = 0.9
            , bringToFront = TRUE)
            , label = ~paste0(
              "<b>", small_ids, "</b><br>",
              "Mean P(event) = ", round(prob_pred, 3), "<br>",
              "<span style='color:", true_out_color, ";'>● Event observed</span>"
            ) |> lapply(htmltools::HTML)
          )
      }

      ## Legend
      proxy |>
        addLegend(
          position = "bottomright"
        , pal      = prob_palette
        , values   = c(0, 1)
        , title    = "P(event)"
        , opacity  = 0.85
        , labFormat = labelFormat(digits = 2)
        ) |>
        addControl(
          html = paste0(
            "<div style='background:#16181f;color:#e34a33;",
            "padding:4px 8px;border-radius:4px;font-size:11px;",
            "border:1px solid #e34a33;'>",
            "&#9646; Event observed</div>")
          , position = "bottomright")

      if (!is.null(clicked_hex_fi())) {
        leafletProxy("average_map_big_hexes") |>
          addPolygons(
            data = avg_map_data_fi() |>
              filter(small_ids == clicked_hex_fi())
          , fill = FALSE
          , color = "#ffff00"
          , weight = 3
          , group = "selected"
          )
      }

  })

  ## PANEL 1: sub-hex spatial plot
  output$subhex_plot <- renderPlot({
    req(clicked_hex())
    hex_id <- clicked_hex()

    ## Find child hex ids belonging to this parent
    children <- parent_lookup |>
      filter(region_norm == hex_id) |>
      pull(shapeName)

    sub_data <- dat.temporal_agg |>
      filter(shapeName %in% children, date == input$selected_date) |>
      st_as_sf()

    shiny::validate(need(nrow(sub_data) > 0, "No sub-hex data for this selection."))

    ## Identify event sub-hexes for outline
    sub_data <- sub_data |>
      mutate(outline_col = if_else(true_out == 1, true_out_color, "#3a3d4a"),
             outline_wt  = if_else(true_out == 1, 1.2, 0.3))

    ggplot(sub_data) +
      geom_sf(
        aes(fill = prob_pred)
      , colour = sub_data$outline_col
      , linewidth = sub_data$outline_wt) +
      scale_fill_gradient(
          low    = "#f7fbff"
        , high   = "#08306b"
        , limits = c(0, 1)
        , name   = "P(event)"
      ) +
      labs(
          title    = paste0("Sub-hexes  ·  ", format(as.Date(input$selected_date), "%b %d, %Y"))
        , subtitle = paste0("Parent hex: ", hex_id)
        , caption  = paste0(
          if (any(sub_data$true_out == 1))
            "Orange outline = observed event"
          else
            ""
        )
      ) +
      theme_void(base_family = "IBM Plex Sans") +
      theme(
          plot.background  = element_rect(fill = "#16181f", colour = NA)
        , panel.background = element_rect(fill = "#16181f", colour = NA)
        , plot.title       = element_text(colour = "#e8eaf0", size = 11, face = "bold", margin = margin(b = 2))
        , plot.subtitle    = element_text(colour = "#6b7280", size = 8)
        , plot.caption     = element_text(colour = true_out_color, size = 7)
        , legend.text      = element_text(colour = "#e8eaf0", size = 7)
        , legend.title     = element_text(colour = "#e8eaf0", size = 8)
        , legend.key.height = unit(0.4, "cm")
        , legend.key.width  = unit(0.3, "cm")
        , plot.margin      = margin(6, 6, 4, 6))
  }, bg = "#16181f")

  ## PANEL 1: forecast horizon time series
  output$timeseries_plot <- renderPlot({
    req(clicked_hex())
    hex_id <- clicked_hex()

    ts_data <- dat.spatial_agg |>
      filter(region_norm == hex_id, date == input$selected_date) |>
      arrange(forecast_interval)

    shiny::validate(need(nrow(ts_data) > 0, "No forecast-interval data for this hex."))

    ## true_out: if any interval has true_out == 1, draw a vertical band
    true_intervals <- ts_data |> filter(true_out == 1) |> pull(forecast_interval)

    p <- ggplot(ts_data, aes(x = forecast_interval, y = prob_pred)) +

      ## event-observed vertical lines (one per interval with true_out==1)
      {
        if (length(true_intervals) > 0)
        geom_vline(xintercept = true_intervals
                   , colour = true_out_color, linewidth = 0.8
                   , linetype = "dashed", alpha = 0.85)
        else list()
      } +

      ## ribbon for visual weight (fake ±0 band – swap for CI if available)
      geom_ribbon(aes(
          ymin = pmax(prob_pred - 0.05, 0)
        , ymax = pmin(prob_pred + 0.05, 1))
        , fill = "#4a9eff", alpha = 0.15) +
      geom_line(colour  = "#4a9eff", linewidth = 1.1) +
      geom_point(colour = "#ffffff", fill = "#4a9eff", shape = 21, size = 2.8, stroke = 1.1) +
      scale_x_continuous(name   = "Days ahead", breaks = ts_data$forecast_interval) +
      scale_y_continuous(
          name   = "P(event)"
        , limits = c(0, 1)
        , breaks = seq(0, 1, .25)
        , labels = num_lab) +
      labs(
        title    = paste0("Forecast horizon profile  ·  ", format(as.Date(input$selected_date), "%b %d, %Y")),
        subtitle = paste0("Hex: ", hex_id
                          , if (length(true_intervals) > 0)
                            paste0("   |   Event observed at ",
                                   paste(true_intervals, collapse = ", "), " days")
                          else "")
      ) +
      theme_minimal(base_family = "IBM Plex Sans") +
      theme(
          plot.background  = element_rect(fill = "#16181f", colour = NA)
        , panel.background = element_rect(fill = "#16181f", colour = NA)
        , panel.grid.major = element_line(colour = "#2a2d3a", linewidth = 0.4)
        , panel.grid.minor = element_blank()
        , axis.text        = element_text(colour = "#9ca3af", size = 8)
        , axis.title       = element_text(colour = "#e8eaf0", size = 9)
        , plot.title       = element_text(colour = "#e8eaf0", size = 11, face = "bold", margin = margin(b = 2))
        , plot.subtitle    = element_text(colour = true_out_color, size = 7.5)
        , plot.margin      = margin(6, 10, 4, 6)
      )

    ## Annotate event lines
    if (length(true_intervals) > 0) {
      p <- p + annotate(
          "text"
        , x     = true_intervals
        , y     = 0.97
        , label = "● observed"
        , colour = true_out_color
        , size   = 2.5
        , hjust  = -0.1
        , family = "IBM Plex Sans")
    }

    p
  }, bg = "#16181f")

  ## PANEL 1: density plot
  output$density_plot <- renderPlot({

    d <- map_data()

    shiny::validate(need(nrow(d) > 0, "No data for selected date"))

    d.t <- d |> mutate(
        true_out = plyr::mapvalues(true_out, from = c(0, 1), to = c("No", "Yes"))
      , true_out  = as.factor(true_out)
      , prob_pred = qlogis(prob_pred)
      )

        ggplot(d.t, aes(x = prob_pred)) +
          geom_density(aes(fill = true_out), alpha = 0.5, colour = NA) +
          scale_fill_brewer(palette = "Dark2", name = "Outbreak
Recorded") +
          theme(
            axis.text.x  = element_text(size = 10)
          , axis.text.y  = element_text(size = 10)
          , axis.title.x = element_text(size = 12)
          , axis.title.y = element_text(size = 12)
          , panel.background = element_rect(fill = "white")
          , panel.grid.major = element_line(color = "grey90")
          , panel.grid.minor = element_line(color = "grey95")
          ) +
          scale_x_continuous(
            breaks = qlogis(c(0.001, 0.01, 0.1, 0.5, 0.9)),
            labels = c("0.1%", "1%", "10%", "50%", "90%"),
            name   = "P(outbreak)"
          ) +
          ylab("Density") +
          ggtitle(paste(
            "Prediction Date = "
          , paste(strsplit(as.character(unique(d$date)), " ")[[1]], collapse = " - ")
          ))

  }, bg = "#16181f")

  ## PANEL 1: calibration curves plot
  output$calibration_plot <- renderPlot({

    d <- calibration_filtered()

    shiny::validate(need(nrow(d) > 0, "No calibration data for selected date"))

    d.t <- d |> filter(truth > 0)

      ggplot(d.t, aes(grp_mean, predicted_grp_med)) +
        geom_abline(color = "gray50") +
        geom_errorbarh(aes(xmin = grp_lwr, xmax = grp_upr, colour = Method), linewidth = 0.5, height = 0) +
        geom_errorbar(aes(ymin = predicted_grp_lwr, ymax = predicted_grp_upr, colour = Method), linewidth = 0.5, width = 0) +
        geom_errorbar(aes(ymin = predicted_grp_lwrn, ymax = predicted_grp_uprn, colour = Method), linewidth = 1, width = 0) +
        geom_point(aes(size = log(n), colour = Method), pch = 21, fill = "white") +
        scale_color_brewer(palette = "Dark2") +
        scale_size_continuous(name = "Number
Predicted
(log)") +
        scale_x_log10(labels = scales::label_percent()) +
        scale_y_log10(labels = scales::label_percent()) +
        labs(y = "Forecasted outbreak probability", x = "Observed outbreak rate", color = "") +
        theme(
          axis.text = element_text(size = 14)
        , axis.title = element_text(size = 16)
        , plot.title.position = "plot"
        , plot.caption = element_text(hjust = 0)
        , panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
        , panel.grid.minor = element_line(color = "grey95", linewidth = 0.3)
        , panel.background = element_rect(fill = "white")
        )

  }, bg = "#16181f")

  ## PANEL 2: Prediction probabilities for all inner hexes
  output$hex_timeseries_full_child <- renderPlot({

    req(clicked_hex_ts())

    hex_id <- clicked_hex_ts()

    ## Find child hex ids belonging to this parent
    children <- parent_lookup |>
      filter(region_norm == hex_id) |>
      pull(shapeName)

    ts_data <- dat.no_agg |>
      filter(shapeName %in% children) |>
      mutate(forecast_interval = as.factor(forecast_interval))

    shiny::validate(need(nrow(ts_data) > 0, "No sub-hex data for this selection."))

    ## Time series for the hexes internal to this hex
    ggplot(ts_data, aes(date, prob_pred)) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.001, ymax = 0.01, fill = "grey80", alpha = 0.2) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.01, ymax = 0.1, fill = "grey80", alpha = 0.35) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.1, ymax = 0.5, fill = "grey80", alpha = 0.50) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.5, ymax = 1, fill = "grey80", alpha = 0.65) +
     geom_line(aes(colour = forecast_interval, linetype = shapeName), alpha = 0.4) +
      scale_colour_brewer(palette = "Dark2", name = "Forecast Interval") +
      xlab("Date") +
      ylab("Predicted Outbreak Probability") +
      scale_y_log10(
        breaks = c(0.001, 0.01, 0.1, 0.5),
        labels = c("0.1%", "1%", "10%", "50%")
      ) +
      geom_vline(data = ts_data |> filter(true_out == 1)
                 , aes(xintercept = date, colour = forecast_interval, linetype = shapeName)
                 , alpha = 0.4, linetype = "dotdash") +
      theme(
        axis.text.x = element_text(size = 10)
      , axis.text.y = element_text(size = 10)
      , panel.background = element_rect(fill = "white")
      , panel.grid.major = element_line(color = "grey90")
      , panel.grid.minor = element_line(color = "grey95")
      )

  }, bg = "#16181f")

  ## PANEL 2: Prediction probabilities for the summary hex
  output$hex_timeseries_full_parent <- renderPlot({

    req(clicked_hex_ts())

    hex_id <- clicked_hex_ts()

    ts_data <- dat.spatial_agg |> filter(region_norm == hex_id) |>
      mutate(forecast_interval = as.factor(forecast_interval))

    shiny::validate(need(nrow(ts_data) > 0, "No sub-hex data for this selection."))

    ## Time series for the hexes internal to this hex
    ggplot(ts_data, aes(date, prob_pred)) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.001, ymax = 0.01, fill = "grey80", alpha = 0.2) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.01, ymax = 0.1, fill = "grey80", alpha = 0.35) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.1, ymax = 0.5, fill = "grey80", alpha = 0.50) +
      annotate("rect", xmin = min(ts_data$date), xmax = max(ts_data$date)
               , ymin = 0.5, ymax = 1, fill = "grey80", alpha = 0.65) +
      geom_line(aes(colour = forecast_interval), alpha = 0.75) +
      scale_colour_brewer(palette = "Dark2", name = "Forecast Interval") +
      xlab("Date") +
      ylab("Predicted Outbreak Probability") +
      scale_y_log10(
        breaks = c(0.001, 0.01, 0.1, 0.5),
        labels = c("0.1%", "1%", "10%", "50%")
      ) +
      geom_vline(data = ts_data |> filter(true_out == 1)
                 , aes(xintercept = date, colour = forecast_interval)
                 , alpha = 0.4, linetype = "dotdash") +
      theme(
        axis.text.x = element_text(size = 10)
      , axis.text.y = element_text(size = 10)
      , panel.background = element_rect(fill = "white")
      , panel.grid.major = element_line(color = "grey90")
      , panel.grid.minor = element_line(color = "grey95"))

  }, bg = "#16181f")

  ## PANEL 3: Forecast probability overall
  output$interval_preds_overall <- renderPlot({

    d.t <- no_agg_with_big_hexes |>
      mutate(
        forecast_interval = as.factor(forecast_interval)
      , true_out          = plyr::mapvalues(true_out, from = c(0, 1), to = c("No", "Yes"))
      , true_out          = as.factor(true_out)
      )

        ggplot(d.t, aes(forecast_interval, prob_pred)) +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.001, ymax = 0.01, fill = "grey80", alpha = 0.2) +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.01, ymax = 0.1, fill = "grey80", alpha = 0.35) +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.1, ymax = 0.5, fill = "grey80", alpha = 0.50) +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1, fill = "grey80", alpha = 0.65) +
          geom_violin(aes(fill = true_out), colour = NA, alpha = 0.4) +
          scale_fill_brewer(
            palette = "Dark2", name = "Outbreak") +
          scale_y_log10(
            breaks = c(0.001, 0.01, 0.1, 0.5),
            labels = c("0.1%", "1%", "10%", "50%")
          ) +
          xlab("Forecast Interval (Days)") +
          ylab("Predicted Outbreak Probability") +
          theme(
              axis.text.x = element_text(size = 10)
            , axis.text.y = element_text(size = 10)
            , axis.title.x = element_text(size = 12)
            , axis.title.y = element_text(size = 12)
            , panel.background = element_rect(fill = "white")
            , panel.grid.major = element_line(color = "grey90")
            , panel.grid.minor = element_line(color = "grey95")
          )

  }, bg = "#16181f")

  ## PANEL 3: Forecast probability by hex single forecast window
  output$interval_preds_hex_single <- renderPlot({

    req(clicked_hex_fi())

    hex_id  <- clicked_hex_fi()

    fi_data <- forecast_filtered() |> filter(small_ids == hex_id)

    shiny::validate(need(nrow(fi_data) > 0, "No sub-hex data for this selection."))

    d.t <- fi_data |> mutate(
        true_out    = plyr::mapvalues(true_out, from = c(0, 1), to = c("No", "Yes"))
      , true_out  = as.factor(true_out)
      , prob_pred = qlogis(prob_pred)
    )

      ggplot(d.t, aes(x = prob_pred)) +
        geom_density(aes(fill = true_out), alpha = 0.5, colour = NA) +
        scale_fill_brewer(palette = "Dark2", name = "Outbreak
Recorded") +
        annotate("rect", ymin = -Inf, ymax = Inf, xmin = qlogis(0.001), xmax = qlogis(0.01), fill = "grey80", alpha = 0.2) +
        annotate("rect", ymin = -Inf, ymax = Inf, xmin = qlogis(0.01), xmax = qlogis(0.1), fill = "grey80", alpha = 0.35) +
        annotate("rect", ymin = -Inf, ymax = Inf, xmin = qlogis(0.1), xmax = qlogis(0.5), fill = "grey80", alpha = 0.50) +
        annotate("rect", ymin = -Inf, ymax = Inf, xmin = qlogis(0.5), xmax = qlogis(1), fill = "grey80", alpha = 0.65) +
        theme(
          axis.text.x = element_text(size = 10)
        , axis.text.y = element_text(size = 10)
        , axis.title.x = element_text(size = 12)
        , axis.title.y = element_text(size = 12)
        , panel.background = element_rect(fill = "white")
        , panel.grid.major = element_line(color = "grey90")
        , panel.grid.minor = element_line(color = "grey95")
        ) +
        scale_x_continuous(
          breaks = qlogis(c(0.001, 0.01, 0.1, 0.5, 0.9)),
          labels = c("0.1%", "1%", "10%", "50%", "90%"),
          name   = "P(outbreak)"
        ) +
        ylab("Density") +
        ggtitle(paste(
          "Forecast Interval = "
        , as.character(unique(fi_data$forecast_interval))
        ))

  }, bg = "#16181f")

 ## PANEL 4: Temp VI plot 1
 output$shap_heatmap_raw <- renderPlot({

   ggplot(shap.dat, aes(x = forecast_interval, y = feature, fill = mean_abs_shap)) +
       geom_tile() +
       scale_fill_viridis_c(name = "Mean |SHAP|") +
       labs(x = "Forecast interval (days)", y = NULL) +
       theme_minimal() +
       theme(
           axis.text.x = element_text(size = 10),
           axis.text.y = element_text(size = 8)
       )

 }, bg = "#16181f")

 ## PANEL 4: Combined SHAP lineplot; forecast covariates distinguished by bolder lines/points
 output$shap_lineplot <- renderPlot({

  d.t <- shap.dat |>
    dplyr::mutate(
      forecast_interval_num = as.numeric(as.character(forecast_interval)),
      is_forecast           = grepl("forecast", feature)
    )

  ggplot(d.t, aes(x = forecast_interval_num, y = mean_abs_shap, color = feature)) +
    geom_line(aes(linewidth = is_forecast)) +
    geom_point(aes(size = is_forecast)) +
    scale_linewidth_manual(values = c(`FALSE` = 0.6, `TRUE` = 1.8), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 1.5, `TRUE` = 3.5), guide = "none") +
    scale_x_continuous(breaks = sort(unique(d.t$forecast_interval_num))) +
    scale_y_log10(labels = scales::label_number()) +
    labs(
      x       = "Forecast interval (days)"
    , y       = "Mean |SHAP|"
    , color   = "Feature"
    , caption = "Bold lines/points = forecast covariates"
    ) +
    theme_minimal()

 }, bg = "#16181f")

}

#### Run app
shinyApp(ui = ui, server = server)
