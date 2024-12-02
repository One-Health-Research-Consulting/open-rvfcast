lag_data <- function(data_files, 
                     lag_intervals,
                     model_dates_selected,
                     overwrite = TRUE,
                     ...) {
  
  # The goal of this is to figure out the average of the data column over the interval
  # Find dates at start and end interval back from date
  # Group by x, y, start_interval, end_interval, and take the mean don't forget na.rm = T
  
  data <- arrow::open_dataset(data_files)
    
  lagged_data <- map_dfr(1:(length(lag_intervals) - 1), function(i) {
    
    start_date = model_dates_selected - days(lag_intervals[i])
    end_date = model_dates_selected - days(lag_intervals[i+1] - 1)
      
    data |> filter(date >= end_date, date <= start_date) |>
      collect() |>
      select(-source) |>
      group_by(x, y, date, doy, month, year) |> 
      summarize(across(everything(), ~mean(.x, na.rm = T))) |>
      mutate(lag_interval = lag_intervals[i])
    
    select(-doy, -month, -year) |> mutate(date = dplyr::lag(date, lag_interval)) |> rename_with(~ paste(.x, "lag", lag_interval, sep = "_"), contains("ndvi")) |> drop_na(date)
  }) |> reduce(left_join, by = c("x", "y", "date")) |>
    drop_na() |> 
    rename_with(~gsub("_0", "", .x), contains("_0"))
  
}