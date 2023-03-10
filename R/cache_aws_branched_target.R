#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author Emma Mendelsohn
#' @export
cache_aws_branched_target <- function(tmp_path, ext, cleanup = FALSE) {

  local_path <- map(tmp_path, ~gsub(ext, paste0("_cache", ext), .))
    
  walk2(tmp_path, local_path, function(tmp, local){
    message(paste("caching", basename(tmp)))
    file.copy(tmp, local, overwrite = TRUE)
  })
  
  if(cleanup) {
    unlink(tmp_path)
  }
  
  unlist(local_path)

}
