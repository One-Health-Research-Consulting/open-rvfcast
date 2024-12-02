transform_ndvi <- function(modis_ndvi_transformed, 
                           sentinel_ndvi_transformed,
                           ndvi_transformed_directory,
                           overwrite = FALSE,
                           ...) {
  
  # ndvi_transformed_dataset <- arrow::open_dataset(c(sentinel_ndvi_transformed, modis_ndvi_transformed))
  # 
  # years <- ndvi_transformed_dataset |> select(year) |> distinct() |> arrange(year) |> pull(year, as_vector = T)
  # 
  # ndvi_transformed <- map(years, function(yr) {
  #   
  #   ndvi_transformed_dataset <- arrow::open_dataset(c(sentinel_ndvi_transformed, modis_ndvi_transformed)) |> filter(year == yr)
  #   
  #   # Set filename
  #   save_filename <- file.path(ndvi_transformed_directory, glue::glue("ndvi_transformed_{yr}.gz.parquet"))
  #   message(paste0("Combining ndvi sources for ", yr))
  #   
  #   # Check if file already exists and can be read
  #   error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  #   
  #   if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
  #     message("file already exists and can be loaded, skipping")
  #     return(save_filename)
  #   }
  #   
  #   arrow::write_parquet(ndvi_transformed_dataset |> filter(year == yr), save_filename)
  #   
  #   rm(ndvi_transformed_dataset)
  #   
  #   return(save_filename) 
  # })
  # 
  # ndvi_transformed
  
}