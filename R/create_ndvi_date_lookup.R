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

  
  sent_dataset <- open_dataset(sentinel_ndvi_transformed_directory) 
  modi_dataset <- open_dataset(modis_ndvi_transformed_directory) 
  
  # create lookup table so we know which rows to query, without doing an expansion on the actual data
  sent_dates <- sent_dataset |> 
    distinct(start_date, end_date) |> 
    arrange(start_date) |> 
    collect() |> 
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y-1, by = "1 day"))) |>  
    mutate(satellite = "sentinel")
  
  min_sent_start <- min(sent_dates$start_date) # this is when sentinel starts
  
  modi_dates <- modi_dataset |> 
    distinct(start_date, end_date) |> 
    arrange(start_date) |> 
    collect() |> 
    mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y-1, by = "1 day"))) |> 
    mutate(satellite = "modis") |> 
    mutate(lookup_dates = map(lookup_dates, ~na.omit(if_else(.>=min_sent_start, NA, .)))) |> 
    filter(map_lgl(lookup_dates, ~length(.) > 0))
  
  ndvi_dates <- bind_rows(modi_dates, sent_dates) |> 
    mutate(lookup_day_of_year = map(lookup_dates, yday)) |> 
    relocate(satellite)
  
  return(ndvi_dates)

}
