#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author Emma Mendelsohn
#' @export
cache_aws_target <- function(tmp_path, ext, cleanup = TRUE) {
  
  local_path <- gsub(ext, paste0("_cache", ext), tmp_path)
  file.copy(tmp_path, local_path, overwrite = TRUE)
  
  if(cleanup) {
    unlink(tmp_path)
  }
  
  local_path

}
