#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param 
#' @return
#' @author Whitney Bagge
#' @export
soil_download <- function() {
  
  options(timeout=200)
   
  location <- c("soil_raster", "soil_database")
  
  for(i in 1:length(location)) { 

  url_out <- switch(location[i], "soil_raster" = "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip", 
                                 "soil_database" = "https://www.isric.org/sites/default/files/HWSD2.sqlite")
  
  
  file_name <- paste("data/soil/",location[i],sep="",".zip")

  download.file(url=url_out, destfile = file_name)
  
  unzipped_soil <- unzip(file_name, exdir = "data/soil/")
  
  
  
  return("data/soil/")
  
  }

}

