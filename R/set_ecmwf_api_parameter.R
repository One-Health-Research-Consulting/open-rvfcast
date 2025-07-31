#' Set ECMWF API Parameter for Climate Data Retrieval
#'
#' This function sets parameters needed for the retrieval of climate data from the European
#' Centre for Medium-range Weather Forecasts (ECMWF) API based on the user's requirements.
#' It is used internally by other functions to fetch and process the data.
#'
#' @author Emma Mendelsohn and Nathan Layman
#'
#' @param start_year The year from which to start the retrieval of the climate data, default is 2005.
#' @param bbox_coords The bounding box coordinates for the location for which the data are to be retrieved.
#' @param variables The variables to retrieve from the API; default are '2m_dewpoint_temperature', '2m_temperature', and 'total_precipitation'.
#' @param product_types The type of data product to retrieve; default are 'monthly_mean', 'monthly_maximum', 'monthly_minimum', 'monthly_standard_deviation'.
#' @param lead_months The lead times in months for which the forecasts are made; default are '1', '2', '3', '4', '5', '6'.
#'
#' @return A tibble containing the set parameters.
#'
#' @note This function is used internally and does not directly communicate with the ECMWF API.
#'
#' @examples
#' set_ecmwf_api_parameter(start_year = 2010, 
#'                         bbox_coords = c(50.8503, 4.3517), 
#'                         variables = c("2m_temperature", "total_precipitation"),
#'                         product_types = c("monthly_mean", "monthly_maximum"),
#'                         lead_months = c("1", "2", "3"))
#'
set_ecmwf_api_parameter <- function(start_year = 2005,
                                    bbox_coords = continent_bounding_box,
                                    variables = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                    product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                    lead_months = seq(1, 6)) {


  # API details from:
  # https://confluence.ecmwf.int/display/CKB/How+to+use+the+CDS+interactive+forms+and+CDS+API+for+seasonal+forecast+datasets
  
  # Originating centre: Select the name of the institution the forecasting system of your interest originates from.
  # 
  # System: Select the version of the forecasting system. This is a numeric label and the available values are different for the different "originating centres". Note that for a given start date you could find more than one system available for a single "originating centre".
  # Please note that you must pair up your forecasts with the relevant hindcasts by using the same "system" for both of them. A full description of the use of the system keyword is available from the "Documentation" tab.
  # 
  # Variable: Select the parameter(s) you are interested in.
  # 
  # Product type: Select the monthly product you are interested in.
  # Note that the products under the category "Individual members" consist of all available individual ensemble members, while those in the category "Ensemble" have a single value for each forecast system ensemble.
  # 
  # Year: Select the year of the initialization date of the model run(s) you are interested in.
  # Note that years in both the hindcast period and real-time forecasts are shown here together. You can find more details about hindcast and forecast years availability in the Start dates available in the CDS per forecast system page.
  # 
  # Please note that you must use the hindcast data in order to make a meaningful use of the forecast data. And remember you must pair forecasts and hindcasts up by using the same "system" for both of them.
  # 
  # Month: Select the month of the initialization date of the model run(s) you are interested in.
  # Note that in the current setup all monthly products are encoded using as nominal start date the 1st of each month, regardless of the real initialization date. You can find more information about how nominal start dates are assigned in the "Documentation" tab.
  # 
  # Leadtime month: Select the lead time(s) you are interested in. This is the time, in months, from the initialization date.
  # Note that the convention used for labelling the data implies that leadtime_month=1 is the first complete calendar month after the initialization date. In the current setup of all datasets that means that for a forecast initialised on the 1st of November, leadtime_month=1 is November.
  # 
  # Geographical area: You can select a global field ("whole available area") or a regional subselection by providing the latitude and longitude of its corners.
  # 
  # Data format: You can select the data format for your data retrieval.
  # Currently, GRIB format is used to store C3S seasonal forecast datasets data files.
  # An experimental netCDF conversion can be also accessed. You can find some related information in the "Documentation" tab.
  
  
  # Setup spatial bounds
  # N, W, S, E
  spatial_bounds <- c("N" = unname(bbox_coords["ymax"]),
                      "W" = unname(bbox_coords["xmin"]),
                      "S" = unname(bbox_coords["ymin"]),
                      "E" = unname(bbox_coords["xmax"]))
  
  # Up till last month.
  # dates <- seq.Date(ymd("2017-9-01"), floor_date(today() - months(1), "month"), by = "month")
  dates <- seq.Date(lubridate::ymd(start_year, truncated = 2L), floor_date(today() - months(1), "month"), by = "month")
  dates <- seq.Date(lubridate::ymd(start_year, truncated = 2L), floor_date(today(), "month") - months(1), by = "month")

  seasonal_forecast_parameters <- tibble(year = year(dates), month = month(dates))
  
  message(glue::glue("Preparing to fetch ecmwf {product_types}"))
  
  # 2m_temperature (monthly mean) is the average 2-meter air temperature over the calendar month.
  # 2m_dewpoint_temperature is the average dew point temperature over the same period.
  # total_precipitation is typically the total accumulation for the month (not an average). Check metadata to confirm, as some datasets might report mean daily precipitation rates.
  
  seasonal_forecast_parameters |>
    mutate(spatial_bounds = list(spatial_bounds)) |> 
    mutate(variables = list(variables)) |> 
    mutate(product_types = list(product_types)) |> 
    mutate(leadtime_months = list(as.character(lead_months)))
}
