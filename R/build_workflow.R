#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_data_train
#' @return
#' @author Emma Mendelsohn
#' @export
build_workflow <- function(model_data_train) {
  
  rec <-  recipe(formula = as.formula(outbreak_30 ~
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
                                        anomaly_ndvi_90) ,
                                      data = model_data_train) |>
                   step_mutate(outbreak_30 = as.factor(outbreak_30), skip = TRUE)
                 
                 spec <-
                   boost_tree(trees = 1000, min_n = tune(), tree_depth = tune(), learn_rate = tune(),
                              loss_reduction = tune()) %>%
                   set_mode("classification" ) %>%
                   set_engine("xgboost", num_class = 2, objective = "binary:logistic")
                 
                 model_workflow <-
                   workflow() %>%
                   add_recipe(rec) %>%
                   add_model(spec)
                 
                 model_workflow
                 

}
