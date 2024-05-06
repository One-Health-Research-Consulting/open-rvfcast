#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wf
#' @param splits
#' @param grid
#' @return
#' @author Emma Mendelsohn
#' @export
model_tune <- function(wf, splits, grid) {

  tuned_grid <- tune::tune_grid(
    wf,
    resamples = splits,
    grid = grid,
    metrics = metric_set(brier_class),#  scoring probabilities instead of class
    control = tune::control_grid(verbose = TRUE)
  )
  
  list(obj_class = class(tuned_grid), obj_attributes = attributes(tuned_grid), tuned_grid= tuned_grid |> select(-splits))

  # tune::show_best(tuned_grid, metric = "brier_class")

}
