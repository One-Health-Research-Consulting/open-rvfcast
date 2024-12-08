transform_ndvi <- function(modis_ndvi_transformed, 
                           sentinel_ndvi_transformed,
                           ndvi_transformed_directory,
                           ndvi_years,
                           ndvi_months,
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
  
  # Also we've got overlap between MODIS and sentinal data. For some dates 2018 
  # and 2019 i.e. there are ndvi values for each location and date from both 
  # sources. Current solution is to average them.
  ndvi_transformed <- map2_vec(ndvi_years, ndvi_months, function(.y, .m) {
      
    ndvi_transformed_dataset <- arrow::open_dataset(c(sentinel_ndvi_transformed, modis_ndvi_transformed)) |> 
      filter(year == .y,
             month == .m) |>
      select(-source) |>
      group_by(x, y, date, doy, month, year) |>
      summarize(ndvi = mean(ndvi), .groups = "drop")

    # Set filename
    save_filename <- file.path(ndvi_transformed_directory, glue::glue("ndvi_transformed_{.y}_{.m}.gz.parquet"))
    message(paste("Combining ndvi sources for", .y, "month", .m))

    # Check if file already exists and can be read
    error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

    if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
      message("file already exists and can be loaded, skipping")
      return(save_filename)
    }

    arrow::write_parquet(ndvi_transformed_dataset, save_filename, compression = "gzip", compression_level = 5)

    rm(ndvi_transformed_dataset)

    return(save_filename)
  })

  ndvi_transformed
  
}