#' Join RVF Response with Africa Full Data
#'
#' This function joins RVF response dataset with the complete Africa dataset for a specific model date. The joined dataset 
#' is saved in a specified local directory as a parquet file. Operation is skipped if file already exists in directory and overwrite 
#' flag is set to FALSE.
#'
#' @author Nathan C. Layman
#'
#' @param rvf_response File path for the RVF response dataset.
#' @param africa_full_data File path for the complete Africa dataset.
#' @param model_dates_selected The specific model date for which the join operation is performed.
#' @param local_folder Directory where the processed files will be saved. Default value is "data/africa_full_model_data".
#' @param basename_template Filename template for the processed file. Default value is "africa_full_model_data_{model_dates_selected}.parquet".
#' @param overwrite Boolean flag indicating whether existing processed files should be overwritten. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A string containing the filepath to the processed file.
#'
#' @note This function performs a join operation, saves the result as a parquet file in the specified directory. If a file 
#' already exists at the target filepath and overwrite is FALSE, the existing file's path is returned.
#'
#' @examples
#' join_response(rvf_response = 'path/to/rvf_response',
#'               africa_full_data = 'path/to/africa_full_data',
#'               model_dates_selected = 'selected_date',
#'               local_folder = './data/africa_full_model_data',
#'               basename_template = "africa_full_model_data_{model_dates_selected}.parquet",
#'               overwrite = TRUE)
#'
#' @export
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
    select(x, y, cases, date, forecast_interval, everything())
  
  # Write output to a parquet file
  arrow::write_parquet(result, save_filename, compression = "gzip", compression_level = 5)
  
  rm(result)
  
  save_filename
  
}