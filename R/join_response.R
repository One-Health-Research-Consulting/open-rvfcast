join_response <- function(rvf_response,
                          africa_full_data,
                          model_dates_selected,
                          local_folder = "data/africa_full_model_data",
                          basename_template = "africa_full_model_data_{model_dates_selected}.parquet",
                          overwrite = FALSE,
                          ...) {
  
  # Check that we're only working on one date at a time
  stopifnot(length(model_dates_selected) == 1)
  
  # Set filename
  save_filename <- file.path(local_folder, glue::glue(basename_template))
  message(paste0("Combining explanatory variables for ", model_dates_selected))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping join")
    return(save_filename)
  }
  
  result <- arrow::open_dataset(africa_full_data) |> 
    filter(date == model_dates_selected) |>
    left_join(arrow::open_dataset(rvf_response) |> select(-forecast_start, -forecast_end)) |>
    mutate(cases = coalesce(cases, 0)) |>
    select(x, y, cases, date, forecast_interval, lag_interval, everything())
  
  # Write output to a parquet file
  arrow::write_parquet(result, save_filename, compression = "gzip", compression_level = 5)
  
  rm(result)
  
  save_filename
  
}