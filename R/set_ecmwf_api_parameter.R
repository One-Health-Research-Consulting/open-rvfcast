#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
set_ecmwf_api_parameter <- function(years = 2005:2018,
                                    bbox_coords = continent_bounding_box,
                                    variables = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                    product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                    leadtime_months = c("1", "2", "3", "4", "5", "6")) {

  # Setup spatial bounds
  # N, W, S, E
  spatial_bounds <- c("N" = unname(bbox_coords["ymax"]),
                      "W" = unname(bbox_coords["xmin"]),
                      "S" = unname(bbox_coords["ymin"]),
                      "E" = unname(bbox_coords["xmax"]))
  
  # Up till last month.
  # dates <- seq.Date(ymd("2017-9-01"), floor_date(today() - months(1), "month"), by = "month")
  dates <- seq.Date(lubridate::ymd(min(years), truncated = 2L), floor_date(today() - months(1), "month"), by = "month")

  seasonal_forecast_parameters <- tibble(year = year(dates), month = month(dates))
  
  message(glue::glue("Preparing to fetch ecmwf {product_types}"))
  
  seasonal_forecast_parameters |>
    mutate(spatial_bounds = list(spatial_bounds)) |> 
    mutate(variables = list(variables)) |> 
    mutate(product_types = list(product_types)) |> 
    mutate(leadtime_months = list(leadtime_months))
  
}