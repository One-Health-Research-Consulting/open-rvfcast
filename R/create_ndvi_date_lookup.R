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

  
  sentinel_dataset <- open_dataset(sentinel_ndvi_transformed_directory) 
  modis_dataset <- open_dataset(modis_ndvi_transformed_directory) 
  
  # create lookup table so we know which rows to query, without doing an expansion on the actual data
  sentinel_dates <- sentinel_dataset |> 
    distinct(start_date, end_date) |> 
    arrange(start_date) |> 
    collect() |> 
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y-1, by = "1 day"))) |>  
    mutate(satellite = "sentinel") |> 
    mutate(filename = sort(sentinel_ndvi_transformed))
  
  min_sentinel_start <- min(sentinel_dates$start_date) # this is when sentinel starts
  
  modis_dates <- modis_dataset |> 
    distinct(start_date, end_date) |> 
    arrange(start_date) |> 
    collect() |> 
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y-1, by = "1 day"))) |> 
    mutate(satellite = "modis") |> 
    mutate(filename = sort(modis_ndvi_transformed)) |> 
    mutate(lookup_dates = map(lookup_dates, ~na.omit(if_else(.>=min_sentinel_start, NA, .)))) |> 
    filter(map_lgl(lookup_dates, ~length(.) > 0)) 
  
  ndvi_dates <- bind_rows(modis_dates, sentinel_dates) |> 
    mutate(lookup_day_of_year = map(lookup_dates, yday)) |> 
    relocate(satellite)
  
  return(ndvi_dates)

}
