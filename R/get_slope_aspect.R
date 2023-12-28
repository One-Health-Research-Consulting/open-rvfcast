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
#' 
#' https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v12/en/
library(archive)
get_slope_aspect<- function(slope_aspect_directory_dataset, slope_aspect_directory_raw, continent_raster_template) {
  
  slope_aspect <- c("aspect_zero", "aspect_fortyfive", "aspect_onethirtyfive", "aspect_twotwentyfive", "aspect_undef",
                   "slope_zero", "slope_pointfive", "slope_two", "slope_five", "slope_ten", "slope_fifteen", 
                    "slope_thirty", "slope_fortyfive")
  
  for(asp in slope_aspect) { 
    
    url_out<- switch(asp,  "aspect_zero" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClN_30as.rar",
                    "aspect_fortyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClE_30as.rar", 
                    "aspect_onethirtyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClS_30as.rar",
                    "aspect_twotwentyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClW_30as.rar",
                    "aspect_undef" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClU_30as.rar",
                    "slope_zero" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl1_30as.rar",
                    "slope_pointfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl2_30as.rar",
                    "slope_two" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl3_30as.rar",
                    "slope_five" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl4_30as.rar",
                    "slope_ten" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl5_30as.rar",
                    "slope_fifteen" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl6_30as.rar",
                    "slope_thirty" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl7_30as.rar",
                    "slope_fortyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl8_30as.rar")
    
    filename <- paste("data/slope_aspect/", asp, sep="", ".rar")
    
    #download.file(url=url_out, destfile = filename)
    
    rar_name <- file.path(dirname(filename), system2("unrar", c("lb", filename), stdout = TRUE))
    system2("unrar", c("e", "-o+", filename, dirname(filename), ">/dev/null"))

    GloAspectClN_30as <- rast("GloAspectClN_30as.asc")
    GloAspectClE_30as <- rast("GloAspectClE_30as.asc")
    GloAspectClS_30as <- rast("GloAspectClS_30as.asc")
    GloAspectClW_30as <- rast("GloAspectClW_30as.asc")
    GloAspectClU_30as <- rast("GloAspectClU_30as.asc")
    
    raster_stack_aspect <- c(GloAspectClN_30as, GloAspectClE_30as, GloAspectClS_30as, GloAspectClW_30as, GloAspectClU_30as)
    
    raster_max_aspect <- max(raster_stack_aspect, na.rm = TRUE)
    
    transformed_raster <- transform_raster(raw_raster = rast(raster_max_aspect),
                                           template = rast(continent_raster_template)) 
    
    
    
    #if_else((str_detect(names(transformed_raster), "Aspect", negate=FALSE))==TRUE, c(transformed_raster), 0)
    #if_else statement for aspect and one for slope
    
    # Convert to dataframe
    #dat_out<- as.data.frame(transformed_raster, xy = TRUE) |> 
    #  as_tibble() 
    
    #dat_full <- full_join(dat_out, dat_out, by=c("x", "y"))
 
    
  }

    # Save as parquet 
    write_parquet(dat_full,  "data/slope_aspect_dataset/slope_dataset", compression = "gzip", compression_level = 5)
  
  
  
  return(slope_aspect_directory_dataset)
  
  #go with highest percentage of pixels to make categorical variable for aspect 5 categories
  #for slope - take the median 3-arc second slope; midpoint of the slope class
  
  
}
