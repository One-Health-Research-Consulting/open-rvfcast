#' Split data into training and test data
#'
#'
#' @title split_data

#' @param dat Complete region_data
#' @param end_date last day of training data
#' @param reduce if TRUE filter out some data
#' @return Tibble containing training and test data
#' @author Morgan Kain
#' @export

split_data <- function(dat, end_date, reduce = FALSE) {
  
  #### Split data into training and test sets -------------------------------------
  
  train_data <- dat %>% filter(date <= end_date) 
  
  ## Could add these as another argument or save as in input so as to not hide these
   ## specifics in a function, but still debugging, so leaving for now
  if (reduce) {
    all_zero_cells <- train_data %>% 
      group_by(shapeName) %>%
      summarize(outbreak = sum(outbreak)) %>%
      filter(outbreak == 0)
    
    ## *Explored manually*
    can_be_dropped <- train_data %>% 
      filter(
        anomaly_forecast_scaled_precipitation > 1
        | Temperature_Seasonality > 1.25
        | trees > 0.5 
        | wetland > 0.04
        | mangroves > 0.0025
        | Precipitation_of_Coldest_Quarter > 1
      ) %>% filter(shapeName %in% all_zero_cells$shapeName) %>% 
      mutate(drop = 1, .before = 1)
    
    train_data <- train_data %>%
      left_join(., can_be_dropped %>% dplyr::select(drop, index)) %>% 
      filter(is.na(drop)) %>%
      dplyr::select(-drop)
  }
  
  test_data  <- dat %>% filter(date > end_date)
  
  return(
    tibble(
      train_data = train_data %>% ungroup() %>% list()
    , test_data  = test_data %>% ungroup() %>% list()
    )
  )
  
}
