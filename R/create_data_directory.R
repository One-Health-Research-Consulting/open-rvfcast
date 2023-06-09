#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
create_data_directory <- function(directory_path) {
  
  dir.create(directory_path, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(directory_path, ".gitkeep"))

  return(file.path(directory_path))
}
