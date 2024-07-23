#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Whitney Bagge
#' @export
process_elevation<- function(elevation_directory_raw, elevation_directory_dataset, elevation_downloaded, continent_raster_template) {
  
  
  transformed_raster <- transform_raster(raw_raster = rast("data/elevation/srtm_africa.tif"),
                                            template = rast(continent_raster_template))
  
  # Convert to dataframe
  dat_out2 <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() 
  
  # Save as parquet 
  write_parquet(dat_out2,  "data/elevation_dataset/elevation_dataset", compression = "gzip", compression_level = 5)
  
  
  return(elevation_directory_dataset)
  
}
