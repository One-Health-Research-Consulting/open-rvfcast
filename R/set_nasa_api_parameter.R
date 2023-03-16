#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Emma Mendelsohn
#' @export
set_nasa_api_parameter <- function(bounding_boxes) {
  
  xy <- bounding_boxes |> 
    mutate(across(x_min:y_max, ~round(., 1))) |> 
    filter(region != "africa") |> 
    group_split(region) |> 
    map_dfr(function(reg){
      x <- c(seq(reg$x_min, reg$x_max, by = 4)) # by 4 instead of 5 to be able to handle adding 2 to the range below
      if(reg$x_max > x[length(x)]) x <- c(x, reg$x_max)
      if(x[length(x)] - x[length(x)-1] < 2) x[length(x)] <- x[length(x)-1] + 2 # API requires at least 2 degree range
      
      y <- c(seq(reg$y_min, reg$y_max, by = 4))
      if(reg$y_max > y[length(y)]) y <- c(y, reg$y_max)
      if(y[length(y)] - y[length(y)-1] < 2) y[length(y)] <- y[length(y)-1] + 2 # API requires at least 2 degree range
      
      out <- crossing(x = rolling_box(x), y = rolling_box(y))  |> 
        mutate(region = reg$region)
    })
  
    
  # start and end dates
  current_time <- Sys.Date()
  dates <- tibble(year = 1993:year(current_time)) |> 
    mutate(start = paste0(year, "-01-01"), end = paste0(year, "-12-31")) |> 
    mutate(end = if_else(year == year(current_time), as.character(current_time), end)) |> 
    rowwise() |> 
    mutate(dates = list(c(start, end))) |> 
    ungroup() |> 
    select(-start, -end)
  
  daily_recorded_parameters <- crossing(dates, xy) |> 
    group_by(year, region) |> 
    mutate(i = row_number()) |> 
    ungroup()

  return(daily_recorded_parameters)
  
}

rolling_box <- function(x){
  out <- list()
  for(i in 1:(length(x)-1)){
    out[[i]] <- c(x[i], x[i+1])
  }
  return(out)
}
