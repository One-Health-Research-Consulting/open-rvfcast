#' Set the Dates for a Model
#'
#' This function generates a sequence of dates for use in a model, given the provided parameters for the start and end years, the desired number of dates per month (or all days if NULL), and the seed for random number generation.
#'
#' @author Nathan C. Layman
#'
#' @param start_year The first year in the range of dates. Must be a positive integer.
#' @param end_year The final year in the range of dates. Must be a positive integer greater than or equal to start_year.
#' @param n_per_month The number of dates per month that should be generated. If NULL (default), all days in each month will be included. Must be a positive integer if specified.
#' @param seed The seed to be used in the random number generator. This is used to ensure reproducibility in the sequence of dates. Must be numeric.
#'
#' @return A sequence of dates to be used in a model, based on the provided parameters. Only returns dates up to today's date.
#'
#' @note This function generates a sequence of dates using 'seq', 'sample', and 'lubridate::days_in_month'. The seed ensures reproducibility. The sequence includes a desired number of dates per month (or all days if n_per_month is NULL) between the start and end years.
#'
#' @importFrom purrr map
#' @importFrom lubridate days_in_month
#'
#' @examples
#' # Get 15 random days per month from 2020-2022
#' set_model_dates(start_year = 2020, end_year = 2022, n_per_month = 15, seed = 42)
#' 
#' # Get all days from 2020-2022
#' set_model_dates(start_year = 2020, end_year = 2022, n_per_month = NULL, seed = 42)
#' 
#' # Get all days from 2020-2022 (same as above)
#' set_model_dates(start_year = 2020, end_year = 2022, seed = 42)
#'
#' @export
set_model_dates <- function(start_year, end_year, n_per_month = NULL, seed = 123) {
  
  # Basic input validation
  if (start_year > end_year) {
    stop("start_year must be less than or equal to end_year")
  }
  
  if (!is.null(n_per_month) && n_per_month <= 0) {
    stop("n_per_month must be positive or NULL")
  }
  
  # If n_per_month is NULL, set to 31 to get all days in any month
  if (is.null(n_per_month)) n_per_month <- 31
  
  # Create a vector of dates from January of start year to December of end year with n_per_month random days
  # drawn from each month in the sequence.
  model_dates <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "month")
  
  # Set seed after setting up sequence to ensure reproducibility.
  # Specifying the algorithm used to convert the seed into a random number
  # helps to ensure reproducibility between machines and operating systems.
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  
  # We use map here so that if we change the length of model_dates, say by adding a new year to the sequence,
  # we won't change the random draw for previous months unless we change the seed.
  model_dates <- map(model_dates, ~.x + sample(lubridate::days_in_month(.x), 
                                               min(n_per_month, lubridate::days_in_month(.x)), 
                                               replace = FALSE) - 1) |> unlist() |> as.Date()
  
  # Remove dates earlier than today
  model_dates <- model_dates[model_dates <= Sys.Date()]
  
  return(model_dates)
}
