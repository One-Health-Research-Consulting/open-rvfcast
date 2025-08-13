#' Open multiple datasets and join them
#'
#' This function opens multiple Arrow datasets from provided file paths and joins them into a single dataset.
#' Specifically, it uses a left_join operation, keeping all records from the first dataset and matching
#' records from the remaining datasets. When a match is not found, NAs are added.
#'
#' @author Nathan Layman
#'
#' @param dataset_list List of file paths to datasets. These datasets are expected to be in Arrow format.
#'
#' @return An Arrow dataset obtained by left joining all datasets in the provided list.
#'
#' @note This function uses the 'arrow' R package for opening Arrow datasets and joining them.
#'
#' @examples
#' open_and_join(dataset_list = c("./data/dataset_1.arrow", "./data/dataset_2.arrow"))
#'
#' @export
open_and_join <- function(dataset_list) {
  
  # Use reduce to iteratively open and join in parquet datasets without collecting
  joined_dataset <- reduce(dataset_list, 
         .init = arrow::open_dataset(dataset_list[[1]]), 
         function(x, y) {x |> left_join(arrow::open_dataset(y))})
  
  joined_dataset
}
