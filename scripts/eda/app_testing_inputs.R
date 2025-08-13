# app_testing
# useful for running the code within the app

input <- list()
input$selected_dataset <- "temperature"
input$selected_date <- dates_to_process[[4]]
input$selected_period <- c(29, 59, 89)
input$data_options <- "comparison"


get_conn <- function(){
  if(input$data_options %in% c("recorded_data", "forecast_data")){
    
    arrow::open_dataset(augmented_data) |>
      dplyr::filter(date == input$selected_date)
    
  } else if (input$data_options == "comparison"){
    
    arrow::open_dataset(forecasts_anomalies_validate) |>
      dplyr::filter(date == input$selected_date)
  }
}

get_dom <- function(){
  get(glue::glue("dom_{stringr::str_remove(input$selected_dataset, '_forecast')}"))
}

get_pal <- function(){
  get(glue::glue("pal_{stringr::str_remove(input$selected_dataset, '_forecast')}_anomalies"))
}


