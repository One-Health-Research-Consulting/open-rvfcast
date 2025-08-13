#' Augment and store disparate datasets
#'
#' This function ingests multiple datasets, augments them, and stores them in Parquet format in a specified directory.
#'
#' @author Nathan C. Layman
#'
#' @param augmented_data_sources The list of data sources to be augmented. These sources are ingested as Arrow datasets.
#' @param augmented_data_directory The directory where augmented datasets are to be stored.
#' @param ... Additional arguments not used by this function but included for potential function extensions.
#'
#' @return A vector of strings containing the filepaths to the newly created Parquet files.
#'
#' @note This function uses Apache Arrow for data ingestion and to write Parquet files. The output files are partitioned by date and compressed using gzip.
#'
#' @examples
#' augment_data(augmented_data_sources = list("dataset1.csv", "dataset2.feather"),
#'              augmented_data_directory = "./data")
#'
#' @export
augment_data <- function(augmented_data_sources,
                         augmented_data_directory,
                         ...) {
  
  # DON'T collect if at all possible. Keep everything in arrow to keep it out of memory until the last possible moment before hive writing
  ds <- reduce(map(unlist(augmented_data_sources$static_layers), arrow::open_dataset), dplyr::left_join, by = c("x", "y"))
  
  message("Save as parquets using hive partitioning by date")
  ds |> mutate(hive_date = date) |> group_by(hive_date) |> arrow::write_dataset(augmented_data_directory, compression = "gzip", compression_level = 5)
  
  return(list.files(augmented_data_directory, pattern = ".parquet", recursive = TRUE, full.names = TRUE))
}
