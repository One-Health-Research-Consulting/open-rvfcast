#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Whitney Bagge
#' @export
#' 
library(raster)
library(terra)
preprocess_glw_data<- function(glw_directory_dataset, glw_directory_raw, glw_downloaded, continent_raster_template) {
  
  transformed_raster_cat <- transform_raster(raw_raster = rast(paste0(glw_downloaded, "/url_cattle.tif")),
                                         template = rast(continent_raster_template))
  transformed_raster_sh <- transform_raster(raw_raster = rast(paste0(glw_downloaded, "/url_sheep.tif")),
                                         template = rast(continent_raster_template))
  transformed_raster_go <- transform_raster(raw_raster = rast(paste0(glw_downloaded, "/url_goats.tif")),
                                         template = rast(continent_raster_template))
  
  # Convert to dataframe
  dat_out_cat<- as.data.frame(transformed_raster_cat, xy = TRUE) |> 
    as_tibble() 
  dat_out_sh<- as.data.frame(transformed_raster_sh, xy = TRUE) |> 
    as_tibble() 
  dat_out_go<- as.data.frame(transformed_raster_go, xy = TRUE) |> 
    as_tibble() 
  
  # Save as parquet 
  write_parquet(dat_out_cat,  "data/glw_dataset/glw_cattle", compression = "gzip", compression_level = 5)
  write_parquet(dat_out_sh, "data/glw_dataset/glw_sheep", compression = "gzip", compression_level = 5)
  write_parquet(dat_out_go, "data/glw_dataset/glw_goats", compression = "gzip", compression_level = 5)
  
  
  return(glw_directory_raw)
  
}
