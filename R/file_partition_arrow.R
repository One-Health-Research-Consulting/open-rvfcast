# static_sources <- c(soil_preprocessed,
#                     aspect_preprocessed,
#                     slope_preprocessed,
#                     glw_preprocessed,
#                     elevation_preprocessed,
#                     bioclim_preprocessed,
#                     landcover_preprocessed)
# 
# sources <- list(weather_anomalies = weather_anomalies,
#              ndvi_anomalies = ndvi_anomalies)
# 
# join_sources <- function(sources, 
#                          join_by = c("x", "y"), 
#                          join_function = inner_join,
#                          model_dates_selected = NULL,
#                          filename = "africa_full_static_data.gz.parquet", 
#                          local_folder = "data/africa_full_static_data",
#                          overwrite = FALSE,
#                          ...) {
# 
#   # Check that we're only working on one date at a time
#   stopifnot(length(model_dates_selected) <= 1)
#   
#   save_file <- file.path(local_folder, filename)
#   
#   # Check if joined data file already exists and can be read and that we don't want to overwrite them.
#   error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
#   
#   if(!is.null(error_safe_read_parquet(outbreak_history_filename)) & !overwrite) {
#     message(glue::glue("Joined data parquet already exists and can be loaded, skipping join"))
#     return(outbreak_history_filename)
#   }
#   
#   # Open arrow datasets
#   datasets <- map(unlist(sources), ~arrow::open_dataset(.x)) |> 
#     reduce(join_function) 
#   
#   # Filter all datasets if partitioning by date
#   if(!is.null(model_dates_selected)) {
#     message(paste0("Combining explanatory variables for ", model_dates_selected))
#     datasets <- map(datasets, ~.x |> filter(date == model_dates_selected))
#   }
#   |> 
#     arrow::write_parquet(save_file, compression = "gzip", compression_level = 5)
#   
#   save_file
#     
# }
# 
# # Write forecast_data
# # This includes weather forecast and rvf_response
# 
# 
# forecast_data <- map()
