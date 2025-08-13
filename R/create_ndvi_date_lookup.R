#' Create NDVI Date Lookup
#'
#' This function converts two ndvi transformed Datasets into a lookup format 
#' which includes a sequence of each day between start_date and end_date for 
#' each satellite's dataframe of distinct start_date and end_date. A column 
#' lookup_day_of_year is added. It contains the day number of each date in 
#' lookup_dates. The row binding function bind_rows() is used to combine MODIS 
#' and Sentinel datasets.
#'
#' @authorEmma Mendelsohn
#' 
#' @param sentinel_ndvi_transformed Path to a directory containing the sentinel_ndvi_transformed dataset.
#' @param modis_ndvi_transformed Path to a directory containing the modis_ndvi_transformed dataset.
#'
#' @return A tibble containing the following columns: satellite, start_date, filename, lookup_dates, lookup_day_of_year.
#'
#' @note Only the dates of sentinel dataset after 2018 is considered.
#' Functions from the following packages are used: dplyr, arrow, purrr, bindrows.
#'
#' @examples
#' sentinel_ndvi_path <- "./data/sentinel_ndvi_transformed/"
#' modis_ndvi_path <- "./data/modis_ndvi_transformed/"
#' create_ndvi_date_lookup(sentinel_ndvi_path, modis_ndvi_path)
#'
#' @export
create_ndvi_date_lookup <- function(sentinel_ndvi_transformed,
                                    modis_ndvi_transformed) {
  
  # Connect to Sentinel and Modis datasets
  sentinel_dataset <- arrow::open_dataset(sentinel_ndvi_transformed) 
  modis_dataset <- arrow::open_dataset(modis_ndvi_transformed) 
  
  # Sentinel dates handling -------------------------------------------------
  
  # Get start and end dates from sentinel, as reported in by the source
  # create a list column of all the dates covered by the interval
  sentinel_dates <- sentinel_dataset |> 
    distinct(start_date, end_date) |> 
    arrange(start_date) |> 
    collect() |> 
    mutate(satellite = "sentinel") |> 
    mutate(filename = sort(sentinel_ndvi_transformed))
  
  # Visual inspection of the rasters shows that the Sentinel data in 2018 is not same scale/format as 2019 onward, let's filter that out
  sentinel_dates <- sentinel_dates |> 
    filter(year(start_date) > 2018)
  
  # Notice that the reported intervals are mostly 10 or 11 days
  # 7 day lengths are 2/21 - 2/28
  # 9 day lengths: end date does not overlap with the next start date
  # 10 day lengths: end date does overlap with the next start date
  reported_sentinel_interval_lengths <- sentinel_dates$end_date - sentinel_dates$start_date
  # sentinel_dates |> slice(which(reported_sentinel_interval_lengths == 7))
  # sentinel_dates |> slice(which(reported_sentinel_interval_lengths == 9))
  # sentinel_dates |> slice(which(reported_sentinel_interval_lengths == 10))
  
  # Check reported overlap between end of one reporting cycle and start of next
  # the negative values are missing dates. two cases of 10-11 days missing. two cases of 2 days missing. 
  # the positive values reflects a three day overlap in 2020
  reported_sentinel_overlap <- sentinel_dates$end_date[-length(sentinel_dates$end_date)] - sentinel_dates$start_date[-1]
  # check_neg <- which(reported_sentinel_overlap < -1)
  # sentinel_dates |> slice(sort(c(check_neg, check_neg+1)))
  # check_pos <- which(reported_sentinel_overlap >0 )
  # sentinel_dates |> slice(sort(c(check_pos, check_pos+1)))

  # Because of above, to avoid overlaps: let's assume the end date is the day before the next start date
  sentinel_dates_diffs <- diff(sentinel_dates$start_date)
  sentinel_dates_diffs <- c(sentinel_dates_diffs, "NA") # NA for the last start date
  
  # Now get end dates in date format and replace existing end dates
  sentinel_dates_diffs_as_date <-  sentinel_dates$start_date + sentinel_dates_diffs-1
  sentinel_dates <- sentinel_dates |> 
    mutate(end_date_reported = end_date) |> 
    mutate(end_date = coalesce(sentinel_dates_diffs_as_date, end_date_reported)) |> # replace last NA with reported date
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y, by = "1 day")))  
    
  # When did sentinel data start? We'll use this to filter the MODIS data
  min_sentinel_start <- min(sentinel_dates$start_date) 
  
  # MODIS dates handling -------------------------------------------------
  
  # Get start dates from MODIS 
  # end dates are not provided by source and will need to be calculated
  modis_dates <- modis_dataset |> 
    distinct(start_date) |> 
    arrange(start_date) |> 
    collect() 
  
  # Get days in between start dates
  # we are assuming that the end date is the day before the next start date
  modis_dates_diffs <- diff(modis_dates$start_date)
  modis_dates_diffs <- c(modis_dates_diffs, "NA") # NA for the last start date
  
  # ^ Note that some intervals are 13 or 14 days
  # there is always a report on December 19th (or 18th if leap year), 
  # and then the next is January 1st, which makes the interval 13 (or 14) days instead of 16.
  
  # Now get end dates in date format
  modis_dates_diffs_as_date <- modis_dates$start_date + modis_dates_diffs-1
  
  # Add end date and create a list column of all the dates covered by the interval
  # filter to end where sentinel starts
  modis_dates <- modis_dates |> 
    mutate(end_date = modis_dates_diffs_as_date) |> 
    mutate(satellite = "modis") |> 
    mutate(filename = sort(modis_ndvi_transformed)) |> 
    drop_na(end_date) |> # we could assume an end date, but not necessary for modis because we're using sentinel past 2018
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y, by = "1 day"))) |> 
    mutate(lookup_dates = map(lookup_dates, ~na.omit(if_else(.>=min_sentinel_start, NA, .)))) |> 
    filter(map_lgl(lookup_dates, ~length(.) > 0)) 
  
  
  # Combine modis and NDVI --------------------------------------------------
  
  # Create lookup table so we know which rows to query, without doing an expansion on the actual data
  ndvi_dates <- bind_rows(modis_dates, sentinel_dates) |> 
    mutate(lookup_day_of_year = map(lookup_dates, yday)) |> 
    relocate(satellite) |> 
    select(-end_date_reported, -end_date)
  
  # Check there is no overlap in the dates
  all_dates <- reduce(ndvi_dates$lookup_dates, c)
  assertthat::are_equal(length(all_dates), n_distinct(all_dates))
  
  # Check that all dates are there
  all_dates_check <- seq(from = min(all_dates), to = max(all_dates), by = 1)
  assertthat::assert_that(all(all_dates_check %in% all_dates))
  
  return(ndvi_dates)
  
}
