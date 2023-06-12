#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param raster_file
#' @param template
#' @param transform_directory
#' @return
#' @author Emma Mendelsohn
#' @export
transform_sentinel_ndvi <- function(sentinel_ndvi_downloaded,
                                    continent_raster_template) {
  
  start_date <- as.Date(str_extract(sentinel_ndvi_downloaded, "(\\d{8}T\\d{6})"), format = "%Y%m%dT%H%M%S")
  end_date <- as.Date(str_extract(sentinel_ndvi_downloaded, "(?<=_)(\\d{8}T\\d{6})(?=_\\w{6}_)"), format = "%Y%m%dT%H%M%S")

  message(paste0("Transforming ", sentinel_ndvi_downloaded))
  
  transformed_raster <- transform_raster(raw_raster = rast(sentinel_ndvi_downloaded),
                                         template = rast(continent_raster_template))
  
  # Convert to dataframe
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = NDVI) |> 
    mutate(start_date = start_date,
           end_date = end_date)
  
  return(dat_out)

}
