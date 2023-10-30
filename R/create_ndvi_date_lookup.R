#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sentinel_ndvi_transformed
#' @param sentinel_ndvi_transformed_directory
#' @param modis_ndvi_transformed
#' @param modis_ndvi_transformed_directory
#' @return
#' @author Emma Mendelsohn
#' @export
create_ndvi_date_lookup <- function(sentinel_ndvi_transformed,
                                    sentinel_ndvi_transformed_directory,
                                    modis_ndvi_transformed,
                                    modis_ndvi_transformed_directory) {
  
  # Connect to Sentinel and Modis datasets
  sentinel_dataset <- open_dataset(sentinel_ndvi_transformed_directory) 
  modis_dataset <- open_dataset(modis_ndvi_transformed_directory) 
  
  
  # Sentinel dates handling -------------------------------------------------
  
  # Get start and end dates from sentinel, as reported in by the source
  # create a list column of all the dates covered by the interval
  sentinel_dates <- sentinel_dataset |> 
    distinct(start_date, end_date) |> 
    arrange(start_date) |> 
    collect() |> 
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y, by = "1 day"))) |>  
    mutate(satellite = "sentinel") |> 
    mutate(filename = sort(sentinel_ndvi_transformed))
  
  # Visual inspection of the rasters shows that the Sentinel data in 2018 is not same scale/format as 2019 onward, let's filter that out
  sentinel_dates <- sentinel_dates |> 
    filter(year(start_date) > 2018)
  
  # Notice that the intervals are mostly 9 or 10 days
  # 7 day lengths are 2/21 - 2/28
  # 9 day lengths: end date does not overlap with the next start date
  # 10 day lengths: end date does overlap with the next start date
  sentinel_interval_lengths <- map_int(sentinel_dates$lookup_dates, length)
  
  # Notice that there is three day overlap at one point in 2020
  sentinel_overlap_dates <- sentinel_dates |> filter(start_date %in% c("2020-05-04", "2020-05-11"))
  
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
