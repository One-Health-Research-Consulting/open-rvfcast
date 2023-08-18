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
  
  taxa <- c("url_cattle","url_sheep","url_goats")
  
  for(i in 1:length(taxa)) {                                    
  
  url_out <- switch(taxa[i], "url_cattle" = "https://dataverse.harvard.edu/api/access/datafile/6769710", 
                            "url_sheep" = "https://dataverse.harvard.edu/api/access/datafile/6769629",
                            "url_goats" = "https://dataverse.harvard.edu/api/access/datafile/6769692"
                  )

  url_taxa_out<-download.file(url_out, destfile = paste("data/",taxa[i],sep="",".tif"))

  }
  
  
}