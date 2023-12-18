library(shiny)
library(leaflet)
library(targets)
library(terra)

# set targets store
targets_store <- here::here(targets::tar_config_get("store"))

# load raster template
continent_raster_template <- terra::rast(targets::tar_read(continent_raster_template, store = targets_store))
raster_crs <- terra::crs(continent_raster_template)

# load selected model dates - user will be able to select from these
model_dates_selected <- targets::tar_read(model_dates_selected, store = targets_store)

# leaflet base
leafmap <- leaflet() |>
  setView(lng = 21, lat = 5, zoom = 2) |>
  addTiles()

# NDVI
ndvi_date_lookup <- targets::tar_read(ndvi_date_lookup, store = targets_store)
ndvi_anomalies <- targets::tar_read(ndvi_anomalies, store = targets_store)
ndvi_anomalies <- here::here(ndvi_anomalies)

# testing
# input <- list()
# input$selected_date <- model_dates_selected[[100]]

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("NDVI Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_date", "Select a Date", choices = model_dates_selected)
    ),
    mainPanel(
      fluidRow(
      column(6, leafletOutput("ndvi_map")),
      column(6, leafletOutput("ndvi_anomalies_map"))
      )
    )
  )
)

# server ----------------------------------------------------------------------
server <- function(input, output) {
  
  output$ndvi_map <- renderLeaflet({
    
    # actual data 
    ## subset for user selected date and read in to memory as raster
    r_ndvi <- ndvi_date_lookup |> 
      dplyr::filter(purrr::map_lgl(lookup_dates, ~input$selected_date %in% .)) |> 
      dplyr::mutate(filename = here::here(filename)) |> 
      dplyr::pull(filename) |>
      arrow::open_dataset() |>
      dplyr::select(x, y, ndvi) |>
      dplyr::collect() |>
      terra::rast() |>
      terra::`crs<-`(raster_crs)
    
    pal_ndvi <- colorNumeric(c(rev(grDevices::terrain.colors(50))), terra::values(r_ndvi),  na.color = "transparent")
    
    leafmap |> 
      leaflet::addRasterImage(r_ndvi, colors = pal_ndvi) |> 
      leaflet::addLegend(pal = pal_ndvi,
                values = terra::values(r_ndvi),
                title = "NDVI")
  })
  
  output$ndvi_anomalies_map <- renderLeaflet({
    
    r_ndvi_anomalies <- arrow::open_dataset(ndvi_anomalies) |> 
      dplyr::filter(date == input$selected_date) |>
      dplyr::select(x, y, anomaly_ndvi_30) |>
      dplyr::collect() |>
      terra::rast() |>
      terra::`crs<-`(raster_crs)

    pal_ndvi_anomalies <- colorNumeric(palette = c("#783f04", "#f6efee","green"), domain = c(-1, 0 , 1),  na.color = "transparent")
    
    leafmap |> 
      addRasterImage(r_ndvi_anomalies, colors = pal_ndvi_anomalies) |> 
      addLegend(pal = pal_ndvi_anomalies, 
                values = terra::values(r_ndvi_anomalies),
                title = "NDVI anomalies")
    

  })

}

# Run the application 
shinyApp(ui = ui, server = server)
