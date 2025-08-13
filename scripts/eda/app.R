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
dates_to_process <- targets::tar_read(dates_to_process, store = targets_store)

# leaflet base
leafmap <- leaflet::leaflet() |>
  leaflet::setView(lng = median(c(continent_bounding_box["xmin"], continent_bounding_box["xmax"])),
                   lat = median(c(continent_bounding_box["ymin"], continent_bounding_box["ymax"])) - 3, 
                   zoom = 2.5) |>
  leaflet::addTiles()

# data parquet
augmented_data <- here::here(targets::tar_read(augmented_data, store = targets_store))
forecasts_anomalies_validate <- here::here(targets::tar_read(forecasts_anomalies_validate, store = targets_store))

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

# dataset and period choices
recorded_dataset <- c("NDVI" = "ndvi", 
                      "Temperature" = "temperature", 
                      "Precipitation" = "precipitation", 
                      "Relative Humidity" = "relative_humidity")

forecast_dataset <- c("Temperature" = "temperature_forecast", 
                      "Precipitation" = "precipitation_forecast",
                      "Relative Humidity" = "relative_humidity_forecast")

recorded_periods <- c("1-30" = 30, "31-60" = 60, "61-90" = 90)
forecast_periods <- c("0-29" = 29, "30-59" = 59, "60-89" = 89, "90-119" = 119, "120-149" = 149)


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
  titlePanel("OpenRVF Data"),
  
  fluidRow(
    column(2, selectInput("data_options",
                          "",
                          choices = c("Recorded" = "recorded_data",
                                      "Forecast" = "forecast_data",
                                      "Comparison" = "comparison"))),
    column(4, radioButtons("selected_dataset", 
                           "", 
                           choices = recorded_dataset, 
                           inline = FALSE)),
    column(2, checkboxGroupInput("selected_period", 
                                 "", 
                                 choices =  recorded_periods, 
                                 selected = recorded_periods[1])),
    
    column(4, shinyWidgets::sliderTextInput("selected_date", 
                                            "",
                                            choices = dates_to_process,
                                            animate = TRUE)), # animationOptions to set faster but data load cant keep up
  ),
  
  fluidRow(
    uiOutput("maps")
  )
)

# server ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # TODO layout - fix spacing on top
  # TODO colors - especially for comparisons
  # TODO labels on legend - "wetter than average", "colder than average", "recorded was wetter than forecast" 
  # TODO try linked zooms - using reference map - https://github.com/rstudio/leaflet/issues/347
  # TODO discretize everything
  # TODO option to aggregate for comparison
  # TODO summary statistics
  # TODO MASKING
  
  # Update input options based on user selection
  observeEvent(input$data_options, {
    
    if (input$data_options == "recorded_data") {
      
      dataset_choices <- recorded_dataset
      period_choices <- recorded_periods
      
    } else if (input$data_options %in% c("forecast_data", "comparison")) {
      
      dataset_choices <- forecast_dataset
      period_choices <- forecast_periods
    }
    
    updateRadioButtons(session, "selected_dataset", choices = dataset_choices, inline = FALSE)
    updateCheckboxGroupInput(session, "selected_period", choices = period_choices, selected = period_choices[1])
  })
  
  # Connection to data
  get_conn <- reactive({
    
    if(input$data_options %in% c("recorded_data", "forecast_data")){
      
      arrow::open_dataset(augmented_data) |>
        dplyr::filter(date == input$selected_date)
      
    } else if (input$data_options == "comparison"){
      
      arrow::open_dataset(forecasts_anomalies_validate) |>
        dplyr::filter(date == input$selected_date)
    }
    
  })
  
  # Range of values for maps
  get_dom <- reactive({
    get(glue::glue("dom_{stringr::str_remove(input$selected_dataset, '_forecast')}"))
  })
  
  # Palettes for maps
  get_pal <- reactive({
    get(glue::glue("pal_{stringr::str_remove(input$selected_dataset, '_forecast')}_anomalies"))
  })
  
  # Render the maps
  output$maps <- renderUI({
    
    if(input$data_options %in% c("recorded_data", "forecast_data")){
      
      # Iterate through each selected period and generate a map
      map_list <- purrr::map(input$selected_period, function(i) {
        create_arrow_leaflet(
          conn = get_conn(),
          field = paste0("anomaly_", input$selected_dataset, "_", i),
          selected_date = input$selected_date,
          palette = get_pal(),
          domain = get_dom(),
          include_legend = TRUE
        )
      })
      
      # Generate an associated tag for each map
      tag_list <- purrr::map(input$selected_period, function(i){
        lab <- switch(input$data_options,
                      "recorded_data" = "previous",
                      "forecast_data" = "forecast",
                      "comparison" = "forecast")
        period_choices <- ifelse(lab == "previous", "recorded", lab)
        period_choices <- get(glue::glue("{period_choices}_periods"))
        
        paste(names(period_choices[period_choices == i]), "days", lab)
      })
      
    } else if (input$data_options == "comparison"){
      map_list <- purrr::map(input$selected_period, function(i) {
        
        selected_dataset <- stringr::str_remove(input$selected_dataset, '_forecast')
        
        purrr::map(c("forecast", "recorded", "difference"), function(x){
          create_arrow_leaflet(
            conn = get_conn(),
            field = paste0("anomaly_", selected_dataset, "_", x, "_", i),
            selected_date = input$selected_date,
            palette = get_pal(),
            domain = get_dom(),
            include_legend = TRUE
          )
        })
        
      }) 
      map_list <- unlist(map_list, recursive = FALSE)
      
      # Generate an associated tag for each map
      tag_list <- purrr::map(input$selected_period, function(i){
        period_choices <- get("forecast_periods")
        list(
          paste(names(period_choices[period_choices == i]), "days ahead forecast"),
          paste(names(period_choices[period_choices == i]), "days ahead recorded"),
          paste(names(period_choices[period_choices == i]), "days", "difference forecast - recorded")
        )
      })
      tag_list <- unlist(tag_list, recursive = FALSE)
    }
    
    # Create dynamic columns
    columns <- purrr::map2(map_list, tag_list, function(map, tag) {
      column(4, tags$h5(tag), map)
    })
    
    # Combine columns into a single list of tags
    do.call(tagList, columns)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
