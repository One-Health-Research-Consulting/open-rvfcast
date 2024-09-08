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
  dates <- seq.Date(ymd("2017-9-01"), floor_date(today() - months(1), "month"), by = "month")

  seasonal_forecast_parameters <- tibble(year = year(dates), 
                                         month = month(dates)) |>
    filter(year > 2017 | month < 11) |> # 2017 is strange
    mutate(system = case_when( # Case when is sequential. The first match wins
      year > 2021 ~ 51,
      year > 2017 ~ 5,
      year >= 2017 ~ 4)) |>
    group_by(system, year) |>
    summarize(month = list(month), .groups = "drop") 
  
  seasonal_forecast_parameters |>
    mutate(spatial_bounds = list(spatial_bounds)) |> 
    mutate(variables = list(variables)) |> 
    mutate(product_types = list(product_types)) |> 
    mutate(leadtime_months = list(leadtime_months))
  
}