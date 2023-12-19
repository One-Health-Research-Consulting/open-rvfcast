library(shiny)
library(leaflet)
library(targets)

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

# data
ndvi_anomalies <- here::here(targets::tar_read(ndvi_anomalies, store = targets_store))
weather_anomalies <- here::here(targets::tar_read(weather_anomalies, store = targets_store))

# NDVI palette
# v_ndvi_anomalies <- arrow::open_dataset(ndvi_anomalies) |> 
#   dplyr::select(anomaly_ndvi_30, anomaly_ndvi_60, anomaly_ndvi_90) |> 
#   dplyr::collect() |> 
#   as.matrix()
# anomaly is the current value minus the historical mean
# positive means more green or less brown
# negative means less green or more brown
pal_ndvi_anomalies <- leaflet::colorNumeric(palette = grDevices::colorRamp(c("#4C392D", "#C4A484", "#DDDAC3", "#90EE90", "#005249"), interpolate = "linear"), 
                                            domain = c(-0.65, 0, 0.65),  # hardcode min/max from v_ndvi_anomalies
                                            na.color = "transparent")

# Weather palettes
# v_temperature_anomalies <- arrow::open_dataset(weather_anomalies) |> 
#     dplyr::select(anomaly_temperature_30, anomaly_temperature_60, anomaly_temperature_90) |> 
#     dplyr::collect() |>
#     as.matrix()
pal_temperature_anomalies <- leaflet::colorNumeric(palette = grDevices::colorRamp(c("#4C392D", "#C4A484", "#DDDAC3", "#90EE90", "#005249"), interpolate = "linear"), 
                                                   domain = c(-6.4, 0, 6.4),  # hardcode min/max from v_ndvi_anomalies
                                                   na.color = "transparent")

# define function to make maps from arrow dataset
create_arrow_leaflet <- function(conn, field, selected_date, palette, domain, include_legend = FALSE){
  
  r <- conn  |>
    dplyr::select(x, y, !!field) |>
    dplyr::collect()|>
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


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("OpenRVF Dynamic Rasters"),
  
  ## User Inputs 
  fluidRow(
    column(4, shinyWidgets::sliderTextInput("selected_date", "Select a Date",
                                            choices = model_dates_selected,
                                            animate = TRUE)), # animationOptions to set faster but data load cant keep up
    column(4, radioButtons("selected_dataset", "Select Dataset", choices = c("NDVI", "Temperature"), inline = TRUE))
  ),
  
  ## NDVI Maps
  fluidRow(
    column(4, 
           conditionalPanel(
             condition = "input.selected_dataset == 'NDVI'",
             leaflet::leafletOutput("ndvi_anomalies_map_30")
           )),
    column(4, 
           conditionalPanel(
             condition = "input.selected_dataset == 'NDVI'",
             leaflet::leafletOutput("ndvi_anomalies_map_60")
           )),
    column(4, 
           conditionalPanel(
             condition = "input.selected_dataset == 'NDVI'",
             leaflet::leafletOutput("ndvi_anomalies_map_90")
           ))
  ),
  
  ## Temperature Maps
  fluidRow(
    column(4, 
           conditionalPanel(
             condition = "input.selected_dataset == 'Temperature'",
             leaflet::leafletOutput("temperature_anomalies_map_30")
           )),
    column(4, 
           conditionalPanel(
             condition = "input.selected_dataset == 'Temperature'",
             leaflet::leafletOutput("temperature_anomalies_map_60")
           )),
    column(4, 
           conditionalPanel(
             condition = "input.selected_dataset == 'Temperature'",
             leaflet::leafletOutput("temperature_anomalies_map_90")
           ))
  )
)
# server ----------------------------------------------------------------------
server <- function(input, output) {
  
  
  # NDVI --------------------------------------------------------------------
  ndvi <- reactive({
    filename <- ndvi_anomalies[grepl(input$selected_date, ndvi_anomalies)]
    arrow::open_dataset(filename) 
  })
  
  output$ndvi_anomalies_map_30 <- renderLeaflet({
    
    create_arrow_leaflet(conn = ndvi(), 
                         field =  "anomaly_ndvi_30", 
                         selected_date = input$selected_date, 
                         palette = pal_ndvi_anomalies,  
                         domain = c(-0.65, 0, 0.65),
                         include_legend = TRUE)
    
  })
  
  output$ndvi_anomalies_map_60 <- renderLeaflet({
    
    create_arrow_leaflet(conn = ndvi(), 
                         field =  "anomaly_ndvi_60", 
                         selected_date = input$selected_date, 
                         palette = pal_ndvi_anomalies,  
                         domain = c(-0.65, 0, 0.65),
                         include_legend = FALSE)
  })
  
  output$ndvi_anomalies_map_90 <- renderLeaflet({
    
    create_arrow_leaflet(conn = ndvi(), 
                         field =  "anomaly_ndvi_90", 
                         selected_date = input$selected_date, 
                         palette = pal_ndvi_anomalies,  
                         domain = c(-0.65, 0, 0.65),
                         include_legend = FALSE)
  })
  
  # Weather -----------------------------------------------------------------
  weather <- reactive({
    filename <- weather_anomalies[grepl(input$selected_date, weather_anomalies)]
    arrow::open_dataset(filename) 
  })
  
  output$temperature_anomalies_map_30 <- renderLeaflet({
    
    create_arrow_leaflet(conn = weather(), 
                         field =  "anomaly_temperature_30", 
                         selected_date = input$selected_date, 
                         palette = pal_temperature_anomalies,  
                         domain = c(-6.4, 0, 6.4),
                         include_legend = TRUE)
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
