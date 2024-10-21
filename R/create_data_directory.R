#' Creates a New Data Directory
#'
#' This function creates a new directory at the provided `directory_path`. If the directory already exists,
#' no action is taken. A ".gitkeep" file is also created within the new directory to ensure it can be
#' tracked by git.
#'
#' @author Emma Mendelsohn
#'
#' @param directory_path A string specifying the path where the new directory should be created.
#'
#' @return A string specifying the path where the new directory is located.
#'
#' @note If the directory already exists, the function will return the existing directory's path 
#' without creating a new one.
#'
#' @examples
#' create_data_directory(directory_path = './new_directory')
#'
#' @export
create_data_directory <- function(directory_path) {
  
  dir.create(directory_path, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(directory_path, ".gitkeep"))

  return(file.path(directory_path))
}
