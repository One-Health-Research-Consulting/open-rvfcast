

get_rvf_response <- function(wahis_outbreaks,
                             wahis_raster_template,
                             forecast_intervals,
                             model_dates_selected,
                             local_folder = "data/rvf_response",
                             save_filename = "rvf_response.gz.parquet") {
  
  save_filename <- file.path(local_folder, save_filename)
  
  # Unwrap packed template raster
  wahis_raster_template <- terra::rast(wahis_raster_template)
  
  # Convert outbreak locations to a terra vector
  pts <- terra::vect(cbind(wahis_outbreaks$longitude, wahis_outbreaks$latitude), crs = crs(wahis_raster_template))
  
  # Get cell indices for points
  cell_indices <- cellFromXY(wahis_raster_template, cbind(wahis_outbreaks$longitude, wahis_outbreaks$latitude))
  
  # Convert cell indices to standardized lat-lon coordinates based on the template raster cell grid
  # This will allow the outbreaks to be joined to the other data based on lat / long.
  pt_coords <- xyFromCell(wahis_raster_template, cell_indices) |> as_tibble() |> setNames(c("x", "y"))
  
  # Add cell x,y coords to outbreaks tibble.
  wahis_outbreaks_gridded <- wahis_outbreaks |> bind_cols(pt_coords)
  
  # For every date in the range, sum cases across every interval
  # This is an important issue. What exactly are we predicting? Probability 
  # of an outbreak _occurring_ within a forecast window? Or of an outbreak 
  # _starting_ within the forecast window? Going with starting. Much easier.
  rvf_respone <- map_dfr(model_dates_selected, function(model_date) {

    map2_dfr(head(forecast_intervals, -1), tail(forecast_intervals, -1), function(interval_start, interval_end) {
      
      # Not inclusive exclusive handling of range
      outbreaks <- wahis_outbreaks_gridded |> 
        filter(start_date >= lubridate::as_datetime(model_date) + days(interval_start), start_date < lubridate::as_datetime(model_date) + days(interval_end))
      
      if(nrow(outbreaks) > 0) {
        outbreaks <- outbreaks |>
          group_by(x, y) |>
          summarize(date = model_date,
                    forecast_interval = interval_end,
                    forecast_start = lubridate::as_datetime(model_date) + days(interval_start),
                    forecast_end = lubridate::as_datetime(model_date) + days(interval_end),
                    cases = sum(cases, na.rm = T),
                    .groups = "drop")
      }
    })
    
  })
  
  arrow::write_parquet(rvf_respone, save_filename, compression = "gzip", compression_level = 5)
  
  save_filename
}