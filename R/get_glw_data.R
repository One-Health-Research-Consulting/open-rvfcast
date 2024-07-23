#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param 
#' @return
#' @author Whitney Bagge
#' @export
#' 
get_glw_data <- function(glw_directory_raw) {
  
  options(timeout=200)
  
  location <- c("url_cattle", "url_sheep", "url_goats")
  
  for(loc in location) { 
    
    url_out<- switch(loc,  "url_cattle" = "https://dataverse.harvard.edu/api/access/datafile/6769710", 
                           "url_sheep" = "https://dataverse.harvard.edu/api/access/datafile/6769629",
                           "url_goats" = "https://dataverse.harvard.edu/api/access/datafile/6769692")
    
    filename <- paste("data/glw/", loc, sep="", ".tif")
    
    download.file(url=url_out, destfile = filename)
  
  }

  return(glw_directory_raw)

  
  
}
