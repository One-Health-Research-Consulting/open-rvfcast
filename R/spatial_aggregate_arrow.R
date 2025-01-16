spatial_aggregate_arrow <- function(parquet_file_list, 
                                     polygons_sf, 
                                     predictor_aggregating_functions,
                                     model_dates_selected,
                                     local_folder = "data/RSA_rvf_model_data",
                                     basename_template = "RSA_rvf_model_data_{model_dates_selected}.parquet",
                                     overwrite = FALSE,
                                     ...) {
  
  # Check input types
  if (!inherits(polygons_sf, "sf")) stop("Input `polygons_sf` must be an SF dataframe.")
  if (!all(sf::st_geometry_type(polygons_sf) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("polygons_sf must be an sf object and contain POLYGON or MULTIPOLYGON geometries.")
  }
  
  # Check that we're only working on one date at a time
  stopifnot(length(model_dates_selected) == 1)
  
  # Set filename
  save_filename <- file.path(local_folder, glue::glue(basename_template))
  message(paste0("Aggregating model data for ", model_dates_selected))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping join")
    return(save_filename)
  }
  
  polygons_bbox <- sf::st_bbox(polygons_sf)
  
  # Open point data
  points_sf <- arrow::open_dataset(parquet_file_list) |> 
    filter(date == model_dates_selected) |> 
    filter(x > polygons_bbox$xmin) |>
    filter(x < polygons_bbox$xmax) |>
    filter(y > polygons_bbox$ymin) |>
    filter(y < polygons_bbox$ymax) |>
    collect() |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Spatial join
  joined_sf <- sf::st_join(points_sf, polygons_sf |> select(shapeName), join = sf::st_intersects) |>
    sf::st_drop_geometry() |>
    filter(!is.na(shapeName))
  
  # Group by grouping variables and perform aggregation
  
  # Separate grouping columns from aggregation columns
  grouping_vars <- predictor_aggregating_functions |> 
    filter(is.na(aggregating_function), !var %in% c("x", "y")) |> 
    pull(var)
  
  sum_vars <- predictor_aggregating_functions |>
    filter(aggregating_function == "SUM") |>
    pull(var)
  
  avg_vars <- predictor_aggregating_functions |>
    filter(aggregating_function == "AVG") |>
    pull(var)
  
  mode_vars <- predictor_aggregating_functions |>
    filter(aggregating_function == "MODE") |>
    pull(var)
  
  # Function to calculate mode
  mode_fn <- function(x) {
    uniq_vals <- unique(x)
    uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
  }
  
  result <- joined_sf |> 
    group_by_at(c("shapeName",grouping_vars)) |>
    summarize(
      across(all_of(avg_vars), ~ mean(.x, na.rm = TRUE), .names = "{.col}_avg"),
      across(all_of(sum_vars), ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"),
      across(all_of(mode_vars), ~ mode_fn(.x), .names = "{.col}_mode"),
      .groups = "drop")
  
  # Write output to a parquet file
  arrow::write_parquet(result, save_filename, compression = "gzip", compression_level = 5)
  
  save_filename
}
