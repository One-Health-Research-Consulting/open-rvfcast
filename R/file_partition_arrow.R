# file_partition_arrow <- function(sources, # A named, nested list of parquet files
#                                  dates_to_process,
#                                  local_folder = "data/africa_full_data",
#                                  basename_template = "africa_full_data_{dates_to_process}.parquet",
#                                  overwrite = FALSE,
#                                  ...) {
#   
#   # NCL change to branch off of model date for combo
#   # This approach does work. Only writing complete datasets
#   # 2005 doesn't have any outbreak history so what do we input?
#   # Next step is lagged data.
#   # JOINING ON dates_to_process means going back and changing 'base_date' to 'date' in ecmwf_transformed and anomaly
#   
#   # Check that we're only working on one date at a time
#   stopifnot(length(dates_to_process) == 1)
#   
#   # Set filename
#   save_filename <- file.path(local_folder, glue::glue(basename_template))
#   message(paste0("Combining explanatory variables for ", dates_to_process))
#   
#   # Check if file already exists and can be read
#   error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
#   
#   if (!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
#     message("file already exists and can be loaded, skipping download")
#     return(save_filename)
#   }
#   
#   source_data <- map(sources, ~arrow::open_dataset(.) |> 
#                        filter(across(any_of("date"), ~ . == dates_to_process)) |>
#                        collect())
#   
#   # Create a function that performs the join and reports row counts
#   join_with_reporting <- function(accumulated_df, new_df, join_by = "id_column") {
#     
#     # Report rows before joining
#     cat("Rows in accumulated dataset before join:", nrow(accumulated_df), "\n")
#     cat("Rows in new dataset:", nrow(new_df), "\n")
#     
#     # Get names of the datasets for reporting
#     acc_name <- deparse(substitute(accumulated_df))
#     new_name <- deparse(substitute(new_df))
#     
#     # Perform the join
#     result <- inner_join(accumulated_df, new_df, by = join_by)
#     
#     # Report rows after joining
#     cat("Joining in: ", new_name, "\n")
#     cat("Rows after joining:", nrow(result), "\n")
#     cat("------------------------------\n")
#     
#     # If zero rows, report unique values in join columns to diagnose
#     if (nrow(result) == 0) {
#       cat("WARNING: Join resulted in zero rows! Checking join keys...\n")
#       cat("Unique values in accumulated dataset join column:\n")
#       print(head(unique(accumulated_df[[join_by]]), 10))
#       cat("Unique values in new dataset join column:\n")
#       print(head(unique(new_df[[join_by]]), 10))
#       
#       # Check for type differences
#       cat("Data type in accumulated dataset:", class(accumulated_df[[join_by]]), "\n")
#       cat("Data type in new dataset:", class(new_df[[join_by]]), "\n")
#       cat("------------------------------\n")
#     }
#     
#     return(result)
#   }
#   
#   # Apply the joining process with reporting
#   result <- reduce(source_data, join_with_reporting, join_by = c("x", "y"))
#   
#   # Assuming list_of_dfs contains all your dataframes
#   joined_data <- reduce(source_data, inner_join, by = c("x", "y"))
#   
#   