#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_workflow
#' @param training_splits
#' @param grid_size
#' @param n_cores
#' @return
#' @author Emma Mendelsohn
#' @export
tune_parameters <- function(model_workflow, training_splits, grid_size = 10,
                            n_cores = 4) {
    doMC::registerDoMC(cores=n_cores)
    
    xgboost_params <-
      dials::parameters(
        min_n(),
        tree_depth(),
        learn_rate(),
        loss_reduction()
      )
    
    xgboost_grid <-
      dials::grid_latin_hypercube(
        xgboost_params,
        size = grid_size
      )
    
    xgboost_search <- tune_grid(
      model_workflow,
      resamples = training_splits,
      grid = xgboost_grid,
      param_info = xgboost_params,
      control = control_grid(verbose = TRUE, parallel_over = "everything")
    )
    
    check <- xgboost_search |>
      filter(map_int(.notes, nrow)>0)
    
    xgboost_tuned_param <- select_best(xgboost_search)
    
    return(xgboost_tuned_param)

}
