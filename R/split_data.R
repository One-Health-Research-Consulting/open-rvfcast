#' Split data into training and test data
#'
#'
#' @title split_data

#' @param dat Complete region_data
#' @param end_date last day of training data
#' @param forecast_horizon window of prediction
#' @return Tibble containing training and test data
#' @author Morgan Kain
#' @export

split_data <- function(dat, end_date, forecast_horizon) {
  
  #### Split data into training and test sets -------------------------------------
  
  train_data <- dat %>% filter(date <= end_date)
  test_data  <- dat %>% filter(date >= (end_date + forecast_horizon))
  
  ## Alternative using tidymodels
  #first_date_of_test <- unique(dat$date)[which(unique(dat$date) >= end_date)[1]]
  #split_prop <- 1 - (length(which(unique(dat$date) >= first_date_of_test)) / n_distinct(dat$date))
  #data_split <- initial_time_split(dat, prop = split_prop)
  #train_data <- data_split %>% training()
  #test_data  <- data_split %>% testing()

  
  return(
    tibble(
      train_data = train_data %>% ungroup() %>% list()
    , test_data  = test_data %>% ungroup() %>% list()
    )
  )
  
}
