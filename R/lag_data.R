lag_data <- function(data_files, 
                     lag_intervals,
                     model_dates_selected,
                     lagged_data_directory,
                     basename_template = "lagged_data_{model_dates_selected}.gz.parquet",
                     overwrite = TRUE,
                     ...) {
  
  # Check that we're only working on one date at a time
  stopifnot(length(model_dates_selected) == 1)
  
  # Set filename
  save_filename <- file.path(lagged_data_directory, glue::glue(basename_template))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(save_filename)
  }
  
  # The goal of this is to figure out the average of the data column over the interval
  # Find dates at start and end interval back from date
  # Group by x, y, start_interval, end_interval, and take the mean don't forget na.rm = T
  message(glue::glue("calculating lagged data for {dirname(data_files[1])} starting from {model_dates_selected}"))
  
  lagged_data <- map2_dfr(tail(lag_intervals,-1), head(lag_intervals,-1), function(lag_interval_start, lag_interval_end) {
    
    start_date = model_dates_selected + days(lag_interval_start) # start, i.e. 30 days prior.
    end_date = model_dates_selected + days(lag_interval_end) # end, i.e. 0 days prior.
    message(glue::glue("lag_interval range ({lag_interval_start}, {lag_interval_end}]: ({start_date}, {end_date}]"))
      
    # Note: lags go back in time so the inequality symbols are reversed. Also
    # date > start_date makes the range _exclusive_ (start_date, end_date] to avoid
    # duplication problems.
    arrow::open_dataset(data_files) |> 
      filter(date > start_date, date <= end_date) |>
      collect() |>
      group_by(x, y) |> 
      summarize(across(contains("anomaly"), ~mean(.x, na.rm = T)), .groups = "drop") |>
      mutate(lag_interval_start = abs(lag_interval_start)) |>
      select(x, y, lag_interval_start, everything())
  }) |>       
    pivot_wider(names_from = lag_interval_start, 
                values_from = -c(x, y, lag_interval_start),
                names_glue = "{.value}_lag_{lag_interval_start}") |>
    mutate(date = model_dates_selected) |>
    select(x, y, date, everything())
  
  arrow::write_parquet(lagged_data, save_filename, compression = "gzip", compression_level = 5)
  
  save_filename
  
}