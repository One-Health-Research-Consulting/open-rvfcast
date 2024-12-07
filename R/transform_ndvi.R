transform_ndvi <- function(modis_ndvi_transformed, 
                           sentinel_ndvi_transformed,
                           ndvi_transformed_directory,
                           ndvi_years,
                           overwrite = FALSE,
                           ...) {

  
  # NCL I noticed some issues with duplicates in modis_transformed. Check why.
  # There is a duplicate filename in the modis_ndvi_transformed target. WHY?
  # data/modis_ndvi_transformed/transformed_modis_NDVI_2018-12-19.gz.parquet
  # Removing that file fixed the problem of duplicates with arrow::open_dataset at least for 2018
  
  # For sentinel the problem is duplication of boundary date data
  # "data/sentinel_ndvi_transformed/transformed_sentinel_NDVI_2018-10-12_to_2018-10-22.gz.parquet" and
  # "data/sentinel_ndvi_transformed/transformed_sentinel_NDVI_2018-10-22_to_2018-11-01.gz.parquet"
  # Has 2018-10-22 in both files. 
  ndvi_transformed <- map_vec(ndvi_years, function(yr) {

    ndvi_transformed_dataset <- arrow::open_dataset(c(sentinel_ndvi_transformed, modis_ndvi_transformed)) |> 
      filter(year == yr) |> 
      distinct()

    # Set filename
    save_filename <- file.path(ndvi_transformed_directory, glue::glue("ndvi_transformed_{yr}.gz.parquet"))
    message(paste0("Combining ndvi sources for ", yr))

    # Check if file already exists and can be read
    error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

    if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
      message("file already exists and can be loaded, skipping")
      return(save_filename)
    }

    arrow::write_parquet(ndvi_transformed_dataset, save_filename)

    rm(ndvi_transformed_dataset)

    return(save_filename)
  })

  ndvi_transformed
  
}