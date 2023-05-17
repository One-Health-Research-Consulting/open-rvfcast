#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Emma Mendelsohn
#' @export
set_nasa_api_parameter <- function(nasa_weather_coordinates,
                                   start_year,
                                   variables  = c("RH2M", "T2M", "PRECTOTCORR")) {
  
    
  # start and end dates
  current_time <- Sys.Date()
  dates <- tibble(year = start_year:year(current_time)) |> 
    mutate(start = paste0(year, "-01-01"), end = paste0(year, "-12-31")) |> 
    mutate(end = if_else(year == year(current_time), as.character(current_time), end)) |> 
    rowwise() |> 
    mutate(dates = list(c(start, end))) |> 
    ungroup() |> 
    select(-start, -end)
  
  daily_recorded_parameters <- dates |> 
    mutate(variables = list(variables)) |> 
    mutate(coordinates = list(nasa_weather_coordinates))

  return(daily_recorded_parameters)
  
}

rolling_box <- function(x){
  out <- list()
  for(i in 1:(length(x)-1)){
    out[[i]] <- c(x[i], x[i+1])
  }
  return(out)
}
