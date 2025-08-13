# # Function to perform tuning for each set of inner folds
# compute_preds <- function(inner_resample,
#                           xgb_mod,
#                           xgb_recipe,
#                           xgb_metrics,
#                           outer_id) {
#   xgb_res <- tune_grid(
#     xgb_mod,
#     xgb_recipe,
#     resamples = inner_resample,
#     grid = xgb_grid,
#     metrics = xgb_metrics,
#     control = control_grid(save_pred = FALSE)
#   ) {
#     
#   }
#   }