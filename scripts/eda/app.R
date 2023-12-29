library(shiny)
library(leaflet)
library(targets)
library(arrow)
library(tidyverse)

# Setup -------------------------------------------------------------------

# set targets store
targets_store <- here::here(targets::tar_config_get("store"))

# load raster template
continent_raster_template <- terra::rast(targets::tar_read(continent_raster_template, store = targets_store))
raster_crs <- terra::crs(continent_raster_template)
continent_bounding_box <- targets::tar_read(continent_bounding_box, store = targets_store)

# load selected model dates - user will be able to select from these
model_dates_selected <- targets::tar_read(model_dates_selected, store = targets_store)

# leaflet base
leafmap <- leaflet::leaflet() |>
  leaflet::setView(lng = median(c(continent_bounding_box["xmin"], continent_bounding_box["xmax"])),
                   lat = median(c(continent_bounding_box["ymin"], continent_bounding_box["ymax"])) - 3, 
                   zoom = 2.5) |>
  leaflet::addTiles()

# data parquet
augmented_data <- here::here(targets::tar_read(augmented_data, store = targets_store))

# define function to make maps from arrow dataset
create_arrow_leaflet <- function(conn, field, selected_date, palette, domain, include_legend = FALSE){
  
  r <- conn  |>
    # get data via arrow
    dplyr::select(x, y, !!field) |>
    dplyr::collect() |> 
    # for the purposes of visualizing data with long tails, replace values above the range of the domain (99%tile) with the min/max
    dplyr::mutate(!!field := dplyr::case_when(!!dplyr::sym(field) < min(domain) ~ min(domain),
                                              !!dplyr::sym(field) > max(domain) ~ max(domain),
                                              TRUE ~ !!dplyr::sym(field)))  |> 
    terra::rast() |>
    terra::`crs<-`(raster_crs)
  
  l <- leafmap |>
    leaflet::addRasterImage(r, colors = palette) |>
    leaflet::addControl(html = sprintf("<p style='font-size: 14px;'> %s</p>", selected_date),
                        position = "topright")
  
  if(include_legend){
    l <- l |> leaflet::addLegend(pal = palette,
                                 values = domain,
                                 position = "bottomleft")
  }
  
  return(l)
}

# anomaly text 
anamaly_text <- "Anomalies are calculated as the mean value for the lag period minus the historical mean for the same period."

# Color Palettes ----------------------------------------------------------

# NDVI palette
# v_ndvi_anomalies <- arrow::open_dataset(augmented_data) |> dplyr::select(anomaly_ndvi_30, anomaly_ndvi_60, anomaly_ndvi_90) |> dplyr::collect() |> as.matrix()
# min(v_ndvi_anomalies, na.rm = TRUE) # -0.5851203
# max(v_ndvi_anomalies, na.rm = TRUE) # 0.6348975
dom_ndvi <- c(-0.65, 0, 0.65)
pal_ndvi_anomalies <- leaflet::colorNumeric(palette = grDevices::colorRamp(c("#4C392D", "#C4A484", "#DDDAC3", "#90EE90", "#005249"), interpolate = "linear"), 
                                            domain = dom_ndvi,  # hardcode min/max
                                            na.color = "transparent")

# temp palette
# v_temperature_anomalies <- arrow::open_dataset(augmented_data) |> dplyr::select(anomaly_temperature_30, anomaly_temperature_60, anomaly_temperature_90) |> dplyr::collect() |> as.matrix()
# min(v_temperature_anomalies, na.rm = TRUE) # -6.081359
# max(v_temperature_anomalies, na.rm = TRUE) # 6.317933
dom_temperature <- c(-6.4, 0, 6.4)
pal_temperature_anomalies <- leaflet::colorNumeric(palette = grDevices::colorRamp(c("#00008B", "#ADD8E6", "#DDDAC3", "#FFB6C1", "#FF0000"), interpolate = "linear"), 
                                                   domain = dom_temperature,  # hardcode min/max 
                                                   na.color = "transparent")

# precip palette
# v_precipitation_anomalies <- arrow::open_dataset(augmented_data) |> dplyr::select(anomaly_precipitation_30, anomaly_precipitation_60, anomaly_precipitation_90) |> dplyr::collect() |> as.matrix()
# min(v_precipitation_anomalies, na.rm = TRUE) # -18.51957
# max(v_precipitation_anomalies, na.rm = TRUE) # 82.289
# quantile(v_precipitation_anomalies, 0.01, na.rm = TRUE) # -3.116396 
# quantile(v_precipitation_anomalies, 0.99, na.rm = TRUE) # 4.295101
dom_precipitation <- c(-10, 0, 10)
pal_precipitation_anomalies <- leaflet::colorNumeric(palette = grDevices::colorRamp(c("#4C392D", "#C4A484", "#DDDAC3", "#ADD8E6","#00008B"), interpolate = "linear"), 
                                                     domain = dom_precipitation,  # hardcode between 98th%tile and min/max
                                                     na.color = "transparent")

# rel humidity palette
# v_relative_humidity_anomalies <- arrow::open_dataset(weather_anomalies) |> dplyr::select(anomaly_relative_humidity_30, anomaly_relative_humidity_60, anomaly_relative_humidity_90) |> dplyr::collect() |> as.matrix()
# min(v_relative_humidity_anomalies, na.rm = TRUE) # -41.08809
# max(v_relative_humidity_anomalies, na.rm = TRUE) # 36.32504
# quantile(v_relative_humidity_anomalies, 0.01, na.rm = TRUE) # -13.55994
# quantile(v_relative_humidity_anomalies, 0.99, na.rm = TRUE) # 14.21287
dom_relative_humidity <- c(-20, 0, 20)
pal_relative_humidity_anomalies <- leaflet::colorNumeric(palette = grDevices::colorRamp(c("#4C392D", "#C4A484", "#DDDAC3", "#ADD8E6","#00008B"), interpolate = "linear"), 
                                                         domain = dom_relative_humidity, # hardcode between 98th%tile and min/max
                                                         na.color = "transparent")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("OpenRVF Dynamic Rasters"),
  
  ## User Inputs 
  fluidRow(
    column(4, radioButtons("selected_dataset", 
                           "Select Dataset", 
                           choices = c("NDVI" = "ndvi", 
                                       "Temperature" = "temperature", 
                                       "Temperature Forecast" = "temperature_forecast", 
                                       "Precipitation" = "precipitation", 
                                       "Precipitation Forecast" = "precipitation_forecast", 
                                       "Relative Humidity" = "relative_humidity",
                                       "Relative Humidity Forecast" = "relative_humidity_forecast"
                           ), inline = TRUE)),
    column(4, shinyWidgets::sliderTextInput("selected_date", 
                                            "Select a Date",
                                            choices = model_dates_selected,
                                            animate = TRUE)), # animationOptions to set faster but data load cant keep up
    conditionalPanel(
      condition = "input.selected_dataset == 'ndvi'",
      tags$h5(glue::glue("{anamaly_text} 
              Negative values indicate NDVI is more brown / less green than average.
              Positive values indicate NDVI is more green / less brown than average."))
    ),
    conditionalPanel(
      condition = "input.selected_dataset == 'temperature'",
      tags$h5(glue::glue("{anamaly_text} 
              Units are in celsius. 
              Negative values indicate temperature is colder than average.
              Positive values indicate temperature is hotter than average."))
    ),
    conditionalPanel(
      condition = "input.selected_dataset == 'precipitation'",
      tags$h5(glue::glue("{anamaly_text} 
              Units are in mm/day. 
              Negative values indicate lower precipitation than average.
              Positive values indicate higher precipitation than average."))
    ),
    conditionalPanel(
      condition = "input.selected_dataset == 'relative_humidity'",
      tags$h5(glue::glue("{anamaly_text} 
              Units are in %. s
              Negative values indicate lower relative humidity than average.
              Positive values indicate higher relative humidity than average."))
    ),
  ),
  
  ## Maps
  ### Recorded 
  conditionalPanel(
    condition = "input.selected_dataset == 'ndvi' || input.selected_dataset == 'temperature' || input.selected_dataset == 'precipitation' || input.selected_dataset == 'relative_humidity'",
    fluidRow(
      #### 30 days
      column(4, 
             tags$h5("1-30 days previous"),
             leaflet::leafletOutput("anomalies_map_recorded_30")
      ),
      #### 60 days
      column(4, 
             tags$h5("31-60 days previous"),
             leaflet::leafletOutput("anomalies_map_recorded_60")
      ),
      #### 90 days
      column(4, 
             tags$h5("61-90 days previous"),
             leaflet::leafletOutput("anomalies_map_recorded_90")
      )   
    )
  ),
  
  ### Forecasted 
  conditionalPanel(
    condition = "input.selected_dataset == 'temperature_forecast' || input.selected_dataset == 'precipitation_forecast' || input.selected_dataset == 'relative_humidity_forecast'",
    fluidRow(
      #### 29 days
      column(4, 
             tags$h5("0-29 day forecast"),
             leaflet::leafletOutput("anomalies_map_forecast_29")
      ),
      #### 59 days
      column(4, 
             tags$h5("30-59 day forecast"),
             leaflet::leafletOutput("anomalies_map_forecast_59")
      ),
      #### 89 days
      column(4, 
             tags$h5("60-89 day forecast"),
             leaflet::leafletOutput("anomalies_map_forecast_89")
      ),
      #### 119 days
      column(4, 
             tags$h5("90-119 day forecast"),
             leaflet::leafletOutput("anomalies_map_forecast_119")
      ),
      #### 149 days
      column(4, 
             tags$h5("120-149 day forecast"),
             leaflet::leafletOutput("anomalies_map_forecast_149")
             
      )      
    )
  )
  
  
)

# input <- list()
# input$selected_date <- "2005-04-14"
# input$selected_dataset <- "precipitation_forecast"

# server ----------------------------------------------------------------------
server <- function(input, output) {
  
  conn <- reactive({
    arrow::open_dataset(augmented_data) |> 
      dplyr::filter(date == input$selected_date)
  })
  
  pal <- reactive({
    ds <- stringr::str_remove(input$selected_dataset, "_forecast")
    get(glue::glue("pal_{ds}_anomalies"))
  })
  
  dom <- reactive({
    ds <- stringr::str_remove(input$selected_dataset, "_forecast")
    get(glue::glue("dom_{ds}"))
  })
  
  render_arrow_leaflet <- function(map_type, day, include_legend) {
    
    output_id <- glue::glue("anomalies_map_{map_type}_{day}")
    
    output[[output_id]] <- renderLeaflet({
      create_arrow_leaflet(
        conn = conn(), 
        field = paste0("anomaly_", input$selected_dataset, "_", day),
        selected_date = input$selected_date, 
        palette = pal(),  
        domain = dom(),
        include_legend = include_legend
      )
    })
  }
  
  render_arrow_leaflet(map_type = "recorded", day = "30", include_legend = TRUE)
  render_arrow_leaflet(map_type = "recorded", day = "60", include_legend = FALSE)
  render_arrow_leaflet(map_type = "recorded", day = "90", include_legend = FALSE)
  render_arrow_leaflet(map_type = "forecast", day = "29", include_legend = TRUE)
  render_arrow_leaflet(map_type = "forecast", day = "59", include_legend = FALSE)
  render_arrow_leaflet(map_type = "forecast", day = "89", include_legend = FALSE)
  render_arrow_leaflet(map_type = "forecast", day = "119", include_legend = FALSE)
  render_arrow_leaflet(map_type = "forecast", day = "149", include_legend = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
