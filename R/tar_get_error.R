#' Check error recorded in meta file for a target
#'
#' @param branch 
#'
#' @return
#' @export
#'
#' @examples
tar_get_error <- function(branch) {
  branch_name <- deparse(substitute(branch))
  tar_meta() |> filter(name == branch_name) |> pull(error)
}
