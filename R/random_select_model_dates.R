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
random_select_model_dates <- function(start_year, end_year, n_per_month, seed = 1) {
  
  set.seed(seed)
  
  # Create a vector of dates from January 2005 to December 2022
  dates <- seq(as.Date(paste0(start_year,"-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day")
  
  tibble(date = dates) |> 
    mutate(year = format(dates, "%Y"),
           month = format(date, "%m"),
           day = format(dates, "%d")) |> 
    group_by(month, year) |> 
    sample_n(size = n_per_month, replace = FALSE)  |> 
    ungroup()  |> 
    mutate(across(c(year, month, day), as.integer)) |> 
    arrange(year, month, day) 
  
  
}
