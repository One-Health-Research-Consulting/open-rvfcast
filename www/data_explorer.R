####
## Raw Data Explorer App
####

#### Packages ====================================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(h3jsr)
library(bslib)
library(scales)
library(arrow)
library(qs)


#### Functions ===================================================================

clean_region_data <- function(dat) {

  ## Drop all columns that already have a scaled counterpart
  scaled_cols <- names(dat)[str_detect(names(dat), "_scaled_")]
  base_cols   <- str_replace(scaled_cols, "_scaled_", "_")
  dat         <- dat |> select(-any_of(base_cols))

  ## Not ideal as this will need to be manually adjusted, but ok for now. Scale
  ## all of these
  vars_to_scale <- c(
      "glw_cattle", "glw_sheep", "glw_goats", "wc2.1_30s_elev", "Annual_Mean_Temperature"
    , "Mean_Diurnal_Range", "Isothermality", "Temperature_Seasonality", "Max_Temperature_of_Warmest_Month"
    , "Min_Temperature_of_Coldest_Month", "Temperature_Annual_Range", "Mean_Temperature_of_Wettest_Quarter"
    , "Mean_Temperature_of_Driest_Quarter", "Mean_Temperature_of_Warmest_Quarter"
    , "Mean_Temperature_of_Coldest_Quarter", "Annual_Precipitation"
    , "Precipitation_of_Wettest_Month", "Precipitation_of_Driest_Month"
    , "Precipitation_Seasonality", "Precipitation_of_Wettest_Quarter"
    , "Precipitation_of_Driest_Quarter", "Precipitation_of_Warmest_Quarter", "Precipitation_of_Coldest_Quarter"
    , "pred_sero"
  )

  dat <- dat |> mutate(across(all_of(vars_to_scale), ~ as.numeric(scale(.x)[, 1])))

  dat

}
click_hint_box <- function(msg) {
  div(
    style = "
      background:#16181f;
      border:1px solid #2a2d3a;
      color:#6b7280;
      padding:6px 10px;
      border-radius:6px;
      font-size:11px;
      margin-bottom:6px;
    ",
    msg
  )
}

#### Build required objects ======================================================

df_raw <- read_parquet("../data/pan_hex_joined_response_data/pan_hex_joined_response_data_final.parquet") |>
  ungroup() |>
  mutate(index = seq_len(n()), .before = 1) |>
  mutate(
    soil_texture = as.numeric(as.factor(soil_texture))
  , soil_drainage = as.numeric(as.factor(soil_drainage))) |>
  clean_region_data(.)

## Non-dynamic, needs to be cleaned up
all_cols        <- names(df_raw)[-c(1:10)]
static_covars   <- all_cols[c(4, 5, 6, 7, 8:37, 50, 51)]
lagged_covars   <- all_cols[c(38:49)]
forecast_covars <- all_cols[c(1:3)]
temporal_covars <- all_cols[c(52)]


countries_sf <- sf::st_read("countries.geojson", quiet = TRUE)

#### Pre-processing ==============================================================

df_raw$date <- as.Date(df_raw$date)

## Build geometry from H3
if (file.exists("hex_sf.qs")) {
  hex_sf <- qread("hex_sf.qs")
} else {
  hex_sf <- df_raw |>
    distinct(shapeName) |>
    mutate(
        geometry = h3jsr::cell_to_polygon(shapeName, simple = FALSE) |>
        pull(geometry)
    ) |>
    st_as_sf()
}

## Average across forecast intervals (default version)
if (file.exists("df_avg.parquet")) {
  df_avg <- read_parquet("df_avg.parquet")
} else {
  df_avg <- df_raw |>
    group_by(shapeName, date) |>
    summarise(
      across(
        all_of(c(static_covars, temporal_covars, lagged_covars, forecast_covars))
        , mean)
      , .groups = "drop")
}

## Hex-level summary (for map + scatter)
if (file.exists("hex_summary.qs")) {
  hex_summary <- qread("hex_summary.qs")
} else {
  hex_summary <- df_avg |>
    group_by(shapeName) |>
    summarise(across(-date, mean), .groups = "drop") |>
    left_join(hex_sf, by = "shapeName") |>
    st_as_sf()
}

## Long format for density / scatter
df_long <- df_avg |>
  pivot_longer(
    cols = -c(shapeName, date)
  , names_to = "variable"
  , values_to = "value"
  )


#### UI ===========================================================================

ui <- fluidPage(

  theme = bs_theme(
    bg = "#0f1117"
  , fg = "#e8eaf0"
  , primary = "#4a9eff"
  )

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

  , fluidRow(

    #### =========================================== LEFT PANEL (MAP + CONTROLS)
    column(6
    , div(
        style = "padding:10px;"

      , selectInput(
          "map_var"
        , "Map covariate:"
        , choices = c(
            static_covars
          , temporal_covars
          , lagged_covars
          , forecast_covars))

      , conditionalPanel(
          condition = "input.map_var_type == 'forecast'"
        , selectInput(
            "forecast_window", "Forecast window:"
          , choices = sort(unique(df_raw$forecast_interval))
          )
        )

      , tags$hr()

      , leafletOutput("map", height = 600)

      )
    )

    #### =========================================================== RIGHT PANEL
  , column(6
    , tabsetPanel(

        #### ----------------------------------------------------------- SCATTER
        tabPanel(
          "Correlation"

        , fluidRow(
            column(6
              , selectInput(
                  "x_var"
                , "X variable:"
                , choices = names(df_avg)[!names(df_avg) %in% c("shapeName", "date")]
                , selected = "glw_cattle"
                )
            )
          , column(6
              , selectInput(
                  "y_var"
                , "Y variable:"
                , choices = names(df_avg)[!names(df_avg) %in% c("shapeName", "date")]
                , selected = "glw_sheep"
                )
            )
          )

        , plotOutput("scatter_plot", height = 500)

        )

        #### ------------------------------------------------------- TIME SERIES
      , tabPanel(
          "Time Series"

        , selectizeInput(
            "ts_vars"
          , "Variables (max 4):"
          , choices = names(df_avg)[!names(df_avg) %in% c("shapeName", "date", static_covars)]
          , multiple = TRUE
          , options = list(maxItems = 4)
          )

      , div(
          uiOutput("timeseries_hint"),
          div(class = "plot-inner", plotOutput("timeseries_plot", height = "20px"))
        )

        , plotOutput("ts_plot", height = 500)

        )

        #### ----------------------------------------------------------- DENSITY
      , tabPanel(
          "Density"

        , selectInput(
            "dens_var"
          , "Variable:"
          , choices = names(df_avg)[!names(df_avg) %in% c("shapeName", "date")]
          )

        , div(
          uiOutput("density_hint"),
          div(class = "plot-inner", plotOutput("density_plot", height = "20px"))
        )

        , plotOutput("density_plot", height = 500)
        )

      ) ## tabsetPanel
    ) ## Column
  ) ## Fluid Row
) ## UI


#### SERVER ====================================================================
server <- function(input, output, session) {

  #### ---------------------------------------------------------- CLICKED HEX ID
  clicked_hex <- reactiveVal(NULL)
  observeEvent(input$map_shape_click, {
    clicked_hex(input$map_shape_click$id)
  })

  #### ------------------------------------------------------- CLICKED HEX VALUE
  clicked_value <- reactive({

    req(clicked_hex(), input$map_var)

    var <- input$map_var

    if (var %in% forecast_covars) {

      req(input$forecast_window)

      df_raw |>
        filter(shapeName == clicked_hex(),
               forecast_interval == input$forecast_window) |>
        summarise(val = mean(.data[[var]]), .groups = "drop") |>
        pull(val)

    } else {

      hex_summary |>
        st_drop_geometry() |>
        filter(shapeName == clicked_hex()) |>
        pull(.data[[var]])
    }

  })

  shiny::observe({

    req(clicked_hex(), clicked_value())

    val <- round(clicked_value(), 4)

    info_box <- paste0(
      "<div style='
      background: rgba(22,24,31,0.9);
      color: #e8eaf0;
      padding: 8px 10px;
      border-radius: 6px;
      border: 1px solid #4a9eff;
      font-size: 12px;
      line-height: 1.4;
      font-family: monospace;
      min-width: 140px;
    '>",
      "<b>Hex:</b><br>", clicked_hex(), "<br><br>",
      "<b>", input$map_var, ":</b><br>", val,
      "</div>"
    )

    leafletProxy("map") |>
      addControl(html = info_box, position = "bottomleft", layerId = "info_box")

  })

  #### --------------------------------------------------------------------- MAP
  output$map <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        options = tileOptions(opacity = 0.85)
      ) |>
      addPolygons(
        data = countries_sf
      , color = "#ffffff"
      , weight = 1.5
      , opacity = 0.9
      , fill = FALSE
      , group = "borders"
      ) |>
      setView(lng = 15, lat = -4, zoom = 3)

  })

  shiny::observe({

    req(input$map_var)

    var <- input$map_var

    ## handle forecast covariates separately
    if (var %in% forecast_covars) {

      req(input$forecast_window)

      map_data <- df_raw |>
        filter(forecast_interval == input$forecast_window) |>
        group_by(shapeName) |>
        summarise(value = mean(.data[[var]]), .groups = "drop")

    } else {

      map_data <- hex_summary |>
        st_drop_geometry() |>
        select(shapeName, value = all_of(var))
    }

    map_data <- map_data |>
      left_join(hex_sf, by = "shapeName") |>
      st_as_sf()

    pal <- colorNumeric("viridis", domain = map_data$value)

    leafletProxy("map") |>
      clearGroup("hexes") |>
      clearControls() |>
      addPolygons(
        data        = map_data
      , layerId     = ~shapeName
      , group       = "hexes"
      , fillColor   = ~pal(value)
      , fillOpacity = 0.75
      , color       = "#2a2d3a"
      , weight      = 0.5
      , highlightOptions = highlightOptions(
          weight       = 2
        , color        = "#ffffff"
        , fillOpacity  = 0.9
        , bringToFront = TRUE)
      , label       = ~paste0(shapeName, "<br>", round(value, 3)) |> lapply(htmltools::HTML)
      ) |>
      addLegend(
        position  = "bottomright"
      , pal       = pal
      , values    = c(0, 1)
      , title     = var
      , opacity   = 0.85
      , labFormat = labelFormat(digits = 2)
      , layerId   = "legend"
      )

  })

  ## highlight selection
  shiny::observe({

    req(clicked_hex())

    leafletProxy("map") |>
      clearGroup("selected") |>
      addPolygons(
        data = hex_sf |> filter(shapeName == clicked_hex())
      , fill = FALSE
      , color = "#ffff00"
      , weight = 3
      , group = "selected"
      )
  })

  #### ----------------------------------------------------------------- SCATTER
  output$scatter_plot <- renderPlot({

    req(input$x_var, input$y_var)

    plot_data <- hex_summary |> st_drop_geometry()

    ggplot(plot_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(x = input$x_var, y = input$y_var)

  })

  #### ------------------------------------------------------------- TIME SERIES
  output$ts_plot <- renderPlot({

    req(clicked_hex(), input$ts_vars)

    d <- df_avg |>
      filter(shapeName == clicked_hex()) |>
      pivot_longer(
        cols = all_of(input$ts_vars)
      , names_to = "variable"
      , values_to = "value"
      )

    ggplot(d, aes(date, value, color = variable)) +
      geom_line() +
      theme_minimal() +
      labs(title = paste("Hex:", clicked_hex()))

  })

  ## Hint in case a cell isn't picked yet when the density tab is chosen
  output$timeseries_hint <- renderUI({
    if (!is.null(clicked_hex())) return(NULL)
    click_hint_box("Click a hex on the map to view hex-specific covariate time series.")
  })

  #### ----------------------------------------------------------------- DENSITY
  output$density_plot <- renderPlot({

    req(clicked_hex(), input$dens_var)

    global <- df_long |> filter(variable == input$dens_var)

    hex_d <- global |> filter(shapeName == clicked_hex())

    ggplot() +
      geom_density(data = global, aes(x = value, y = after_stat(scaled)), fill = "gray70", alpha = 0.5) +
      geom_density(data = hex_d, aes(x = value, y = after_stat(scaled)), color = "#4a9eff", linewidth = 1.2) +
      theme_minimal() +
      labs(title = paste("Variable:", input$dens_var))

  })

  ## Hint in case a cell isn't picked yet when the density tab is chosen
  output$density_hint <- renderUI({
    if (!is.null(clicked_hex())) return(NULL)
    click_hint_box("Click a hex on the map to view hex-specific density.")
  })

}

shinyApp(ui, server)
