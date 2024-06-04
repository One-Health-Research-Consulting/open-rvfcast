#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
model_grid <- function(training_data) {

  dials::grid_latin_hypercube(
    dials::tree_depth(),
    dials::min_n(),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    dials::finalize(dials::mtry(), training_data),
    dials::learn_rate(),
    size = 10
  ) 

}
