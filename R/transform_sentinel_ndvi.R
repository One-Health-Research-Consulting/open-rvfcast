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
                                    continent_raster_template,
                                    transform_directory =
                                      "data/sentinel_ndvi_transformed") {
  
  filename <- basename(sentinel_ndvi_downloaded)
  start_date <- as.Date(str_extract(filename, "(\\d{8}T\\d{6})"), format = "%Y%m%dT%H%M%S")
  end_date <- as.Date(str_extract(filename, "(?<=_)(\\d{8}T\\d{6})(?=_\\w{6}_)"), format = "%Y%m%dT%H%M%S")
  save_filename <- glue::glue("transformed_NDVI_{start_date}_to_{end_date}.gz.parquet")
  
  suppressWarnings(dir.create(transform_directory, recursive = TRUE))
  existing_files <- list.files(transform_directory)
  
  message(paste0("Transforming ", save_filename))
  
  if(save_filename %in% existing_files){
    message("file already exists, skipping transform")
    return(file.path(transform_directory, filename))
  }
  
  transformed_raster <- transform_raster(raster_file = sentinel_ndvi_downloaded,
                                         template = continent_raster_template, 
                                         verbose = FALSE)
  
  # Convert to dataframe
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |> 
    as_tibble() |> 
    rename(ndvi = NDVI) |> 
    mutate(start_date = start_date,
           end_date = end_date)
  
  # Save as parquet 
  write_parquet(dat_out, here::here(transform_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(transform_directory, save_filename))
  
  
}
