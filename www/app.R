####
## App to explore predictions across various aggregation levels (space and forecast window)
####

#### Load needed packages ========================================================

library(qs)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)
library(h3jsr)  
library(scales)
library(bslib)

#### Read in needed data =========================================================

## targets pipeline puts this is the correct folder
 ## examined_fits_within_country saves component prediction files to outpath_for_app
 ## then built_app_components saves "data_for_app.qs" to "www/"
map_dat          <- qread("data_for_app.qs")  
dat.double_agg   <- map_dat$double_agg[[1]]
dat.temporal_agg <- map_dat$temporal_agg[[1]]
dat.spatial_agg  <- map_dat$spatial_agg[[1]]

#### Various setup needs =========================================================

## Build colours
prob_palette <- colorNumeric(
  palette = c("#f7fbff", "#6baed6", "#08306b")  ## white -- steel -- navy
, domain  = c(0, 1)                             ## predictions are probabilities
, na.color = "#cccccc"
)

true_out_color <- "#e34a33"   ## bold orange-red outline for outbreaks that occurred
 
## Derive the parent -to- child hex lookup once 
make_parent_lookup <- function(small_ids, large_ids) {
  ## Infer the resolution of the large hexes from the first valid id
  large_res <- h3jsr::get_res(large_ids[1])
  ## Map every small hex to its parent at the large resolution
  parents   <- h3jsr::get_parent(small_ids, large_res)
  data.frame(shapeName   = small_ids,
             region_norm = parents,
             stringsAsFactors = FALSE)
}

## Build lookup (runs once at startup)
parent_lookup <- make_parent_lookup(
  small_ids = unique(dat.temporal_agg$shapeName)
, large_ids = unique(dat.double_agg$region_norm)
)

## Ensure date columns are Date class
dat.double_agg$date   <- as.Date(dat.double_agg$date)
dat.temporal_agg$date <- as.Date(dat.temporal_agg$date)
dat.spatial_agg$date  <- as.Date(dat.spatial_agg$date)

## Deal with dates for slider bar
sorted_dates <- sort(unique(dat.double_agg$date))
date_index   <- data.frame(idx = seq_along(sorted_dates), date = sorted_dates)

## For labeling of dates for side plots
num_lab <- scales::label_number(accuracy = 0.01)

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

  ## header 
, div(class = "app-header"
    , p(class = "app-title",  "// PREDICTION EXPLORER")
    , p(class = "app-subtitle", "Spatial · Temporal · Multi-scale"))
  
  ## date slider 
, div(class = "date-bar"
    , sliderTextInput(
        inputId  = "selected_date"
      , label    = NULL
      , choices  = as.character(sorted_dates)
      , selected = as.character(sorted_dates[1])
      , width    = "100%"
      , animate  = TRUE
      , grid     = TRUE
      )
  )

  ## main layout
, fluidRow(

    ## left: primary map
    column(
      7
    , div(class = "map-card"
          , div(class = "card-label", "AGGREGATED FORECAST  ·  click a hex to drill down")
          , leafletOutput("main_map", height = "620px")))
  
    ## detail panels
  , column(
      5
  , div(class = "nudge", uiOutput("click_hint"))
  , div(class = "plot-card"
      , div(class = "card-label", "SUB-HEX SPATIAL DETAIL")
      , div(class = "plot-inner", plotOutput("subhex_plot", height = "280px")
        ))
  , div(class = "plot-card"
      , div(class = "card-label", "FORECAST HORIZON PROFILE")
      , div(class = "plot-inner", plotOutput("timeseries_plot", height = "280px")
        ))
    ))
)

#### Server ======================================================================
server <- function(input, output, session) {
  
  ## Reactive: parse sliderTextInput text back to Date
  selected_date <- reactive({
    as.Date(input$selected_date)
  })
  
  ## Reactive: date-filtered double-agg layer 
  map_data <- reactive({
    dat.double_agg %>% filter(date == selected_date())
  })

  ## Reactive: clicked hex id 
  clicked_hex <- reactiveVal(NULL)

  observeEvent(input$main_map_shape_click, {
    clicked_hex(input$main_map_shape_click$id)
  })

  ## Reset click when date changes so stale side panel plots disappear
  observeEvent(input$selected_date, {
    clicked_hex(NULL)
  })

  ## Hint text 
  output$click_hint <- renderUI({
    if (is.null(clicked_hex())) {
      span("← Select a hex on the map to reveal drill-down panels")
    } else {
      span(style = "color:#4a9eff;", paste0("Showing detail for hex  ", clicked_hex()))
    }
  })

  ## Leaflet map 
  output$main_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        options = tileOptions(opacity = 0.85)
      ) %>%
      setView(lng = 15, lat = -4, zoom = 3) 
  })

  ## Update polygons when date or data changes
  shiny::observe({
    
    req(input$main_map_bounds)  
    
    d    <- map_data()
    pal  <- prob_palette

    ## Split into event / non-event for outline styling
    events     <- d %>% filter(true_out == 1)
    non_events <- d %>% filter(true_out != 1)

    proxy <- leafletProxy("main_map") %>%
      clearShapes() %>%
      clearControls()

    ## Non-event hexes
    if (nrow(non_events) > 0) {
      proxy <- proxy %>%
        addPolygons(
          data        = non_events
        , layerId     = ~region_norm
        , fillColor   = ~pal(prob_pred)
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
          , "P(event) = ", round(prob_pred, 3)) %>% 
          lapply(htmltools::HTML)
        )
    }

    ## Event hexes – bold red outline
    if (nrow(events) > 0) {
      proxy <- proxy %>%
        addPolygons(
          data        = events
        , layerId     = ~region_norm
        , fillColor   = ~pal(prob_pred)
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
          ) %>% lapply(htmltools::HTML)
        )
    }

    ## Legend
    proxy %>%
      addLegend(
        position = "bottomright"
      , pal      = pal
      , values   = c(0, 1)
      , title    = "P(event)"
      , opacity  = 0.85
      , labFormat = labelFormat(digits = 2)
      ) %>%
      addControl(
        html = paste0(
          "<div style='background:#16181f;color:#e34a33;",
          "padding:4px 8px;border-radius:4px;font-size:11px;",
          "border:1px solid #e34a33;'>",
          "&#9646; Event observed</div>")
      , position = "bottomright")
  })

  ## sub-hex spatial plot 
  output$subhex_plot <- renderPlot({
    req(clicked_hex())
    hex_id <- clicked_hex()

    ## Find child hex ids belonging to this parent
    children <- parent_lookup %>%
      filter(region_norm == hex_id) %>%
      pull(shapeName)

    sub_data <- dat.temporal_agg %>%
      filter(shapeName %in% children, date == input$selected_date) %>%
      st_as_sf()

    shiny::validate(need(nrow(sub_data) > 0, "No sub-hex data for this selection."))

    ## Identify event sub-hexes for outline
    sub_data <- sub_data %>%
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

  ## ── forecast horizon time series ─────────────────────────────────────────
  output$timeseries_plot <- renderPlot({
    req(clicked_hex())
    hex_id <- clicked_hex()

    ts_data <- dat.spatial_agg %>%
      filter(region_norm == hex_id, date == input$selected_date) %>%
      arrange(forecast_interval)

    shiny::validate(need(nrow(ts_data) > 0, "No forecast-interval data for this hex."))

    ## true_out: if any interval has true_out == 1, draw a vertical band
    true_intervals <- ts_data %>% filter(true_out == 1) %>% pull(forecast_interval)

    p <- ggplot(ts_data, aes(x = forecast_interval, y = prob_pred)) +

      ## event-observed vertical lines (one per interval with true_out==1)
      { if (length(true_intervals) > 0)
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
}

#### Run app
shinyApp(ui = ui, server = server)
