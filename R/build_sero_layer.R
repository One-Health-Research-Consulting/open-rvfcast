#' Make the predictions for sero ~ cases across all of space and time to match the 
#' other prepared covariates

#' @title build_sero_layer

#' @param fitted_stan_model path to fitted stan model
#' @param sero_cases_dat prepped data
#' @param cov_dat path to otherwise completed dataset
#' @param map_dat spatial groupings
#' @param outpath path to the saved
#' @param overwrite 
#' @return tibble of data
#' @author Morgan Kain
#' @export

build_sero_layer <- function(fitted_stan_model, sero_cases_dat, cov_dat, map_dat, outpath, overwrite) {
  
  if (file.exists(outpath) & !overwrite) {
    print("Layer already generated from fitted stan model, returning previously saved covariate layer")
    return(outpath)
  }
  
  ## First do a bit of work to extract the stan model coefficients
  stan_fit  <- readRDS(fitted_stan_model)
  samps     <- as.array(stan_fit) 
  
  ## grab the stan data to get the correct indices for the H3 hex names
  stan_data <- sero_cases_dat$stan_data[[1]]
  map_data  <- sero_cases_dat$map_data[[1]]
  
  ## Load the covariate stack and strip out the unique combination of H3 and dates
  all_dates <- read_parquet(cov_dat) %>% dplyr::select(shapeName, date) %>% distinct()
  
  ## join in geometry for calculating distances
  all_dates_sf <- all_dates %>% dplyr::select(shapeName) %>% 
    distinct() %>%
    left_join(., map_dat) %>% st_as_sf()
  
  ## pull out cases
  cases_sf <- sero_cases_dat$cases_sf[[1]]
  
  ## For each unique H3 hex, find all of the possible oubtreaks that could contribute to that
   ## hex, regardless of date (figure out date in the next step below)
  possible_idx.f <-  purrr::map(1:nrow(all_dates_sf), .f = function(i) {
    
    ## this row
    trow  <- all_dates_sf[i, ]
    ## distances
    dists <- as.numeric(st_distance(trow$geometry, cases_sf) / 1000)
    
    ## Filter down to a max distance
    possible_idx <- cases_sf[which(dists <= 1000), ] %>% 
      mutate(
        ## Figure out the window in time from this outbreak over which it can contribute serology
        date_max  = date + 8*365
      , distances = dists[which(dists <= 1000)]
      ) %>% rename(h3_case = h3_id) %>%
      mutate(h3_id = all_dates_sf[i, ]$shapeName, .before = 1) %>%
      as.data.frame() %>%
      dplyr::select(-geometry)
    
    return(possible_idx)
    
  }) %>% bind_rows()
  
  ## Join in so that each H3 for prediction has all possible H3 that had cases
  all_possible <- all_dates_sf %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>%
    rename(h3_id = shapeName) %>%
    left_join(., possible_idx.f) %>%
    filter(!is.na(h3_case))
  
  ## Now strip down to which dates make sense
  all_possible_time_ranges <- all_possible %>%
    group_by(h3_id) %>%
    summarize(date_min = min(date), date_max = max(date_max))
  
  ## Find the hexes that have at least one linked outbreak
  idx_with_linked_outbreaks <- unique(all_possible$h3_id)
  
  ## Extract these
  all_dates.space    <- all_dates %>% filter(shapeName %in% idx_with_linked_outbreaks) 
  
  all_dates.st       <- all_dates.space %>% 
    rename(h3_id = shapeName) %>% 
    left_join(all_possible_time_ranges)
  
  ## Now subset down to those whose time lines up
  all_dates.time     <- all_dates.st %>% 
    filter(date < date_max & date > date_min) %>%
    left_join(., map_data)
  
  all_dates.prepped  <- all_dates.time %>% 
    dplyr::select(-date_min, -date_max) %>%
    left_join(., all_possible %>% dplyr::select(-date_max, -h3_case) %>%
                rename(date_cases = date) %>%
                mutate(date_cases = as_date(date_cases))) %>%
    mutate(time_diff = as.numeric(date - date_cases)/365) %>%
    filter(time_diff <= 8, time_diff > 0) %>%
    split_tibble(., c("h3_id", "date"))
  
  all_dates_with_outbreaks <- lapply(all_dates.prepped, FUN = function(x) {
    
    predval <- get_pred(
        b0    = samps[,, 1] %>% c()
      , b     = the_bs[,,x$idx[1]] %>% c()
      , alpha = samps[,, 2] %>% c()
      , rho_d = samps[,, 3] %>% c() %>% exp() 
      , rho_t = samps[,, 4] %>% c() %>% exp()
      , dists = x$distances
      , times = x$time_diff
    )
    
    tibble(
      h3_id     = x$h3_id[1]
    , date      = x$date[1]
    , pred_sero = predval
    )
    
  }) %>% do.call("rbind", .)
  
  all_dates_without_outbreaks <- all_dates %>% 
    rename(h3_id = shapeName) %>% 
    left_join(., cells_with_outbreaks) %>%
    left_join(., map_data %>% dplyr::select(-hexgroup)) %>%
    filter(is.na(pred_sero))
  
  all_dates_without_outbreaks <- all_dates_without_outbreaks %>% 
    rowwise() %>%
    mutate(pred_sero = mean(samps[,, 1] %>% c() + the_bs[,,idx] %>% c()))
  
  all_dates.f <- rbind(
    all_dates_with_outbreaks
  , all_dates_without_outbreaks
  ) %>% 
    distinct() %>% 
    ungroup() %>% 
    mutate(pred_sero = ifelse(is.na(pred_sero), mean(samps[,, 1] %>% c()), pred_sero))
  
  arrow::write_parquet(
      all_dates.f
    , outpath
    , compression = "gzip", compression_level = 5
    )
  
  return(outpath)
  
}

#' Some helper functions

## Make predictions from stan samples for a given H3 cell
get_pred <- function(b0, b, alpha, rho_d, rho_t, dists, times) {
  
  K       <- rowSums(exp(-outer(1/rho_d, dists, "*")) * exp(-outer(1/rho_t, times, "*")))
  p_draws <- b + b0 + alpha * K
  p_mean  <- mean(p_draws)
  
  return(p_mean)
}

