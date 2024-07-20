#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param 
#' @return
#' @author Whitney Bagge, Nathan Layman
#' @export
library(paws)
get_elevation<- function(output_dir, continent_raster_template) {
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  template <- terra::unwrap(continent_raster_template)
  sf_use_s2(FALSE)
  
  CopernicusDEM::aoi_geom_save_tif_matches(sf_or_file = sf::st_as_sfc(sf::st_bbox(template)),
                                           dir_save_tifs = output_dir,
                                           resolution = 90,
                                           crs_value = 4326,
                                           threads = parallel::detectCores(),
                                           verbose = TRUE)

  # Read big raster into memory
  elevation_rast <- terra::rast(filename)
  
  # Save as compressed raster
  terra::writeRaster(gdal=c("COMPRESS=LZW")
                     
  # Clean up big uncompressed raster to save hard drive space
  unlink(filename)
  
  # Return path to compressed raster
  return(filename)
  
}