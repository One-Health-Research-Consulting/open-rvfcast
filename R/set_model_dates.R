#' Set the Dates for a Model
#'
#' This function generates a sequence of dates for use in a model, given the provided parameters for the start and end years, the desired number of dates per month, the lag intervals, and the seed for random number generation.
#'
#' @author Nathan C. Layman
#'
#' @param start_year The first year in the range of dates.
#' @param end_year The final year in the range of dates.
#' @param n_per_month The number of dates per month that should be generated.
#' @param lag_intervals The intervals at which the dates should lag.
#' @param seed The seed to be used in the random number generator. This is used to ensure reproducibility in the sequence of dates. 
#'
#' @return A sequence of dates to be used in a model, based on the provided parameters.
#'
#' @note This function generates a sequence of dates using 'seq', 'sample', and 'lubridate::days_in_month'. The seed ensures reproducibility. The sequence of dates can be lagged by intervals and includes a desired number of dates per month between the start and end years.
#'
#' @examples
#' set_model_dates(start_year = 2000, end_year = 2002, n_per_month = 30, lag_intervals = 1, seed = 42) 
#'
#' @export
set_model_dates <- function(start_year, end_year, n_per_month, lag_intervals, seed = 123) {
  
  # Create a vector of dates from January of start year to December of end year with n_per_month random days
  # drawn from each month in the sequence.
  model_dates <- seq(as.Date(paste0(start_year,"-01-01")), as.Date(paste0(end_year, "-12-31")), by = "month")
  
  # Set seed after setting up sequence to ensure reproducibility.
  # Specifying the algorithm used to convert the seed into a random number
  # helps to ensure reproducibility between machines and operating systems.
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  
  # We use map here so that if we change the length of model_dates, say by adding a new year to the sequence,
  # we won't change the random draw for previous months unless we change the seed.
  model_dates <- map(model_dates, ~.x + sample(lubridate::days_in_month(.x), n_per_month, replace = FALSE) - 1) |> unlist() |> as.Date()
  
  return(model_dates)
}
