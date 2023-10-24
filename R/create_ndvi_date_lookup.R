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
  
  # Get start and end dates from sentinel
  # create a list column of all the dates covered by the interval
  sentinel_dates <- sentinel_dataset |> 
    distinct(start_date, end_date) |> 
    arrange(start_date) |> 
    collect() |> 
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y-1, by = "1 day"))) |>  
    mutate(satellite = "sentinel") |> 
    mutate(filename = sort(sentinel_ndvi_transformed))
  
  # TODO investigate
  map_int(sentinel_dates$lookup_dates, length)
  sentinel_dates |> filter(start_date %in% c("2020-05-04", "2020-05-11"))
  
  # When did sentinel data start?
  min_sentinel_start <- min(sentinel_dates$start_date) # this is when sentinel starts
  
  # Get start dates from MODIS (end dates are variable and need to be calculated)
  modis_dates <- modis_dataset |> 
    distinct(start_date) |> 
    arrange(start_date) |> 
    collect() 
  
  # Get days in between start dates
  modis_dates_diffs <- diff(modis_dates$start_date)
  modis_dates_diffs <- c(modis_dates_diffs, "NA") # NA for the last start date
  
  # Now get end dates in date format
  modis_dates_diffs_as_date <- modis_dates$start_date + modis_dates_diffs-1
  
  # Get start and end dates from modis
  # create a list column of all the dates covered by the interval
  modis_dates <- modis_dates |> 
    mutate(end_date = modis_dates_diffs_as_date) |> 
    mutate(satellite = "modis") |> 
    mutate(filename = sort(modis_ndvi_transformed)) |> 
    drop_na(end_date) |> # we could assume an end date, but not necessary for modis because we're using sentinel past 2018
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y, by = "1 day"))) |> 
    mutate(lookup_dates = map(lookup_dates, ~na.omit(if_else(.>=min_sentinel_start, NA, .)))) |> 
    filter(map_lgl(lookup_dates, ~length(.) > 0)) 
  
  map_int(modis_dates$lookup_dates, length)
  
  # Create lookup table so we know which rows to query, without doing an expansion on the actual data
  ndvi_dates <- bind_rows(modis_dates, sentinel_dates) |> 
    mutate(lookup_day_of_year = map(lookup_dates, yday)) |> 
    relocate(satellite)
  
  # Check there is no overlap in the dates
  all_dates <- reduce(ndvi_dates$lookup_dates, c)
  assertthat::are_equal(length(all_dates), n_distinct(all_dates))
  
  return(ndvi_dates)

}
