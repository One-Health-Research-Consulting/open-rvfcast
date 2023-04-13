#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param 
#' @return
#' @author Whitney Bagge
#' @export
get_glw <- function() {
  
  url_cattle <- "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LHBICE#"
  url_cattle_out <- GET(url_cattle)
  unzipped_glw_cattle <- unzip(url_cattle_out)
  Aw_layer <- if_else(str_detect(names(unzipped_glw_cattle), "Aw")==TRUE, )
  
}