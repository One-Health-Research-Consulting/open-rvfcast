#' Make the predictions for sero ~ cases across all of space and time to match the
#' other prepared covariates

#' @title sero layer functions

#' @param fitted_stan_model path to fitted stan model
#' @param sero_cases_dat prepped data
#' @param cov_dat path to otherwise completed dataset
#' @param map_dat spatial groupings
#' @param time_adjustment boolean to overwrite fitted temporal decay curve with a more defined curve (from a priori knowledge)
#' @param the_pairs links from all hex-date combos to all relevant outbreaks
#' @param with_outbreaks seroprevalence estimates for all of the the_pairs
#' @param the_samps cleaned up samples
#' @param outpath path to the saved
#' @param overwrite
#' @return tibble of data
#' @author Morgan Kain
#' @export

prep_all_pairs <- function(sero_cases_dat, cov_dat, map_dat) {

  ## grab the stan data to get the correct indices for the H3 hex names
  stan_data <- sero_cases_dat$stan_data[[1]]
  map_data  <- sero_cases_dat$map_data[[1]]

  ## Load the covariate stack and strip out the unique combination of H3 and dates
  all_dates <- cov_dat |> dplyr::select(shapeName, date) |> distinct()

  ## join in geometry for calculating distances
  all_dates_sf <- all_dates |>
    dplyr::select(shapeName) |>
    distinct() |>
    left_join(map_dat) |>
    st_as_sf()

  ## pull out cases
  cases_sf <- sero_cases_dat$cases_sf[[1]]

  ## For each unique H3 hex, find all of the possible oubtreaks that could contribute to that
  ## hex, regardless of date (figure out date in the next step below)
  possible_idx.f <-  purrr::map(seq_len(nrow(all_dates_sf)), .f = function(i) {

    ## this row
    trow  <- all_dates_sf[i, ]
    ## distances
    dists <- as.numeric(st_distance(trow$geometry, cases_sf) / 1000)

    ## Filter down to a max distance
    possible_idx <- cases_sf[which(dists <= 1000), ] |>
      mutate(
        ## Figure out the window in time from this outbreak over which it can contribute serology
        date_max  = date + 8 * 365
      , distances = dists[which(dists <= 1000)]
      ) |>
      rename(h3_case = h3_id) |>
      mutate(h3_id = all_dates_sf[i, ]$shapeName, .before = 1) |>
      as.data.frame() |>
      dplyr::select(-geometry)

    possible_idx

  }) |>
  bind_rows()

  ## Join in so that each H3 for prediction has all possible H3 that had cases
  all_possible <- all_dates_sf |>
    as.data.frame() |>
    dplyr::select(-geometry) |>
    rename(h3_id = shapeName) |>
    left_join(possible_idx.f) |>
    filter(!is.na(h3_case))

  ## Now strip down to which dates make sense
  all_possible_time_ranges <- all_possible |>
    group_by(h3_id) |>
    summarize(date_min = min(date), date_max = max(date_max))

  ## Find the hexes that have at least one linked outbreak
  idx_with_linked_outbreaks <- unique(all_possible$h3_id)

  ## Extract these
  all_dates.space    <- all_dates |> filter(shapeName %in% idx_with_linked_outbreaks)

  all_dates.st       <- all_dates.space |>
    rename(h3_id = shapeName) |>
    left_join(all_possible_time_ranges)

  ## Now subset down to those whose time lines up
  all_dates.time     <- all_dates.st |>
    filter(date < date_max & date > date_min) |>
    left_join(map_data)

  all_dates.prepped  <- all_dates.time |>
    dplyr::select(-date_min, -date_max) |>
    left_join(
        all_possible |>
        dplyr::select(-date_max, -h3_case) |>
                rename(date_cases = date) |>
                mutate(date_cases = as_date(date_cases))) |>
    mutate(time_diff = as.numeric(date - date_cases) / 365) |>
    filter(time_diff <= 8, time_diff > 0) |>
    split_tibble(c("h3_id", "date"))

  split(all_dates.prepped, ceiling(seq_along(all_dates.prepped) / 2000))

}
prep_samps     <- function(fitted_stan_model, time_adjustment) {

  ## First do a bit of work to extract the stan model coefficients
  param_samps  <- readRDS(fitted_stan_model)

  ## Don't need them all so drop some
  param_samps  <- param_samps[sample.int(dim(param_samps)[1], 2000), , ]
  ## Exponentiate the needed params
  param_samps[, , 3] <- param_samps[, , 3] |> exp()
  param_samps[, , 4] <- param_samps[, , 4] |> exp()

  ## Pull down the temporal decay rate if desired
  if (time_adjustment) {
    param_samps[, , 4] <- param_samps[, , 4] / 10
  }

  param_samps

}
prep_all_dates <- function(cov_dat) {
  ## Load the covariate stack and strip out the unique combination of H3 and dates
  all_dates <- read_parquet(cov_dat) |> dplyr::select(shapeName, date) |> distinct()
  all_dates
}


#' @param the_pairs links from all hex-date combos to all relevant outbreaks
#' @param the_samps cleaned up samples
#' @param use_intercept boolean for whether or not to use the background random
#'    effect intercept layer (sero unaccounted for by the cases)
#'    or just rely on the spatio-temporal component of the kernel without the intercept
#' @return tibble of data
#' @author Morgan Kain
#' @export

build_sero_for_outbreaks <- function(the_pairs, the_samps, use_intercept) {

  the_pairs <- the_pairs[[1]]

  all_dates_with_outbreaks <- lapply(the_pairs, FUN = function(x) {

    ## select the background random effect draws for this hex's specific cell index, not all cells
    the_bs <- the_samps[, , 4 + x$idx[1]] |> c()
    if (!use_intercept) the_bs[] <- 0
    
    predval <- get_pred(
      b0    = the_samps[, , 1] |> c()
    , b     = the_bs
    , alpha = the_samps[, , 2] |> c()
    , rho_d = the_samps[, , 3] |> c()
    , rho_t = the_samps[, , 4] |> c()
    , dists = x$distances
    , times = x$time_diff)
    
    #print(x); print(predval)

    tibble(
      h3_id     = x$h3_id[1]
    , date      = x$date[1]
    , pred_sero = predval
    )

  }) |>
  bind_rows()

  all_dates_with_outbreaks

}
finish_sero_layer        <- function(sero_cases_dat, samps, with_outbreaks, use_intercept, all_dates, outpath, overwrite) {

  if (file.exists(outpath) && !overwrite) {
    print("Layer already generated from fitted stan model, returning previously saved covariate layer")
    return(outpath)
  }

  map_data  <- sero_cases_dat$map_data[[1]]

  all_dates_without_outbreaks <- all_dates |>
    rename(h3_id = shapeName) |>
    left_join(with_outbreaks) |>
    left_join(
        map_data |>
        dplyr::select(-hexgroup)) |>
    filter(is.na(pred_sero))

  if (use_intercept) {
    
  all_dates_without_outbreaks <- all_dates_without_outbreaks |>
    rowwise() |>
    mutate(pred_sero = mean(
      samps[, , 1] |> c() +
      ## grab the appropriate conditional mode of the GRM
      samps[, , 4 + idx] |> c()
      )
    ) |>
    dplyr::select(-idx)
  
  } else {
    
    all_dates_without_outbreaks <- all_dates_without_outbreaks |>
      rowwise() |>
      mutate(pred_sero = mean(
        samps[, , 1] |> c()
      )) |>
      dplyr::select(-idx)
    
  }

  all_dates.f <- rbind(
    with_outbreaks
  , all_dates_without_outbreaks
  ) |>
    distinct() |>
    ungroup() |>
    mutate(pred_sero = ifelse(is.na(pred_sero), mean(samps[, , 1] |> c()), pred_sero))
  
  ## convert to anomaly and scaled anomaly
  all_dates.f <- all_dates.f |>
    group_by(h3_id) |>
    mutate(
      historical_mean = mean(pred_sero)
    , historical_sd   = sd(pred_sero)
    , historical_sd   = ifelse(historical_sd < 0.1, 0.1, historical_sd)
    ) |>
    ungroup() |>
    mutate(
      anomaly_sero        = pred_sero - historical_mean
    , anomaly_scaled_sero = anomaly_sero / historical_sd
    )

  arrow::write_parquet(
    all_dates.f
  , outpath
  , compression = "gzip", compression_level = 5
  )

  outpath

}


#' Some helper functions

## Make predictions from stan samples for a given H3 cell
get_pred <- function(b0, b, alpha, rho_d, rho_t, dists, times) {

  K       <- rowSums(exp(-outer(1 / rho_d, dists, "*")) * exp(-outer(1 / rho_t, times, "*")))
  p_draws <- b + b0 + alpha * K
  p_mean  <- mean(p_draws)

  p_mean
  
}
