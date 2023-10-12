#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param path
#' @param bucket
#' @param key
#' @param check
#' @return
#' @author Emma Mendelsohn
#' @export
aws_s3_upload_single_type <- function(directory_path,
                                      bucket, 
                                      key, 
                                      check = TRUE) {
  
  file.remove(file.path(directory_path, ".gitkeep"))
  containerTemplateUtils::aws_s3_upload(path = directory_path,bucket =  bucket,key =  key, check = check)
  file.create(file.path(directory_path, ".gitkeep"))
  
}
