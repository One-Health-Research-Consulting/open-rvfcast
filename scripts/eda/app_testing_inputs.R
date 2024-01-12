# app_testing
# useful for running the code within the app

input <- list()
input$selected_dataset <- "ndvi"
input$selected_date <- model_dates_selected[[4]]
input$selected_period <- c(30, 60, 90)
input$data_options <- "recorded_data"


get_conn <- function(){
  arrow::open_dataset(augmented_data) |>
    dplyr::filter(date == input$selected_date)
}

get_dom <- function(){
  get(glue::glue("dom_{stringr::str_remove(input$selected_dataset, '_forecast')}"))
}

get_pal <- function(){
  get(glue::glue("pal_{stringr::str_remove(input$selected_dataset, '_forecast')}_anomalies"))
}