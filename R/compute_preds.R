#' Compute Predictions Using XGBoost Model
#'
#' This function utilizes the XGBoost model, recipe, and metrics to make predictions from the resample data provided. It applies grid tuning to optimize the results.
#'
#' @author Nathan Layman
#'
#' @param inner_resample Resample data utilized for predictions.
#' @param xgb_mod The XGBoost model used.
#' @param xgb_recipe The preprocessing recipe used.
#' @param xgb_metrics The metrics used to evaluate the model.
#' @param outer_id An identifying parameter for the outer resampling iteration.
#'
#' @return The function returns the resultant model after the tune_grid function is applied.
#'
#' @note The 'tune_grid' function from the 'tune' package is utilised to optimise the XGBoost model.
#'
#' @examples
#' compute_preds(inner_res = resample_data,
#'               xgb_mod = model,
#'               xgb_recipe = recipe,
#'               xgb_metrics = metrics,
#'               outer_id = id_val)
#' 
#' @export
compute_preds <- function(inner_resample,
                          xgb_mod,
                          xgb_recipe,
                          xgb_metrics,
                          outer_id) {
  xgb_res <- tune_grid(
    xgb_mod,
    xgb_recipe,
    resamples = inner_resample,
    grid = xgb_grid,
    metrics = xgb_metrics,
    control = control_grid(save_pred = FALSE)
  )
}
