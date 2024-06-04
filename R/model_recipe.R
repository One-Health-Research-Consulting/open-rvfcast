#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param training_data
#' @return
#' @author Emma Mendelsohn
#' @export
model_recipe <- function(training_data) {

  recipe(formula = as.formula(outbreak_30 ~
                                # TODO add day of year
                                # TODO add static
                                # TODO add immunity layer
                                # TODO add recent outbreak layer - has there been an outbreak this season
                                anomaly_relative_humidity_30 +
                                anomaly_temperature_30 +
                                anomaly_precipitation_30 +
                                anomaly_relative_humidity_60 +
                                anomaly_temperature_60 +
                                anomaly_precipitation_60 +
                                anomaly_relative_humidity_90 +
                                anomaly_temperature_90 +
                                anomaly_precipitation_90 +
                                anomaly_temperature_forecast_29 +
                                anomaly_precipitation_forecast_29+
                                anomaly_relative_humidity_forecast_29+
                                anomaly_ndvi_30 +
                                anomaly_ndvi_60 +
                                anomaly_ndvi_90 + 
                                area) ,
         data = training_data)

}
