#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param start_year
#' @param end_year
#' @param n_per_month
#' @return
#' @author Emma Mendelsohn
#' @export
set_model_dates <- function(start_year, end_year, n_per_month, lag_intervals, seed = 1) {
  
  set.seed(seed)
  
  # Create a vector of dates from January 2005 to December 2022
  dates <- seq(as.Date(paste0(start_year,"-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day")
  
  model_dates <-tibble(date = dates) |> 
    mutate(year = format(dates, "%Y"),
           month = format(dates, "%m"),
           day = format(dates, "%d"),
           day_of_year = format(dates, "%j")) |>     
    mutate(across(c(year, month, day, day_of_year), as.integer)) |> 
    mutate(year_day_of_year = paste(year, day_of_year, sep = "_")) |> 
    group_by(month, year) |> 
    mutate(select_date = row_number() %in% sample(n(), n_per_month)) |> 
    ungroup()  |> 
    arrange(year, month, day) 
  
  # make sure we always have enough days to calculate lags
  model_dates$select_date[1:max(lag_intervals)] <- FALSE
  
  return(model_dates)
}
