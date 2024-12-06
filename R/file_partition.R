#' Partition and Write an Arrow Dataset to Parquet Files
#'
#' This function partitions an Arrow dataset by specified groups, writes each partition to its own parquet file, and saves the files to the specified path. 
#' The filenames are created by combining the basename template and the group values.
#'
#' @author Nathan C. Layman
#'
#' @param arrow_dataset Arrow dataset to be partitioned and written to disk.
#' @param path Directory where the parquet files will be saved. 
#' @param basename_template Template for creating the file names of the output parquet files.
#' @param groups Groups to partition the Arrow dataset. Default is c("year","month").
#'
#' @return A vector of the filenames of the written parquet files.
#'
#' @note The Arrow dataset is partitioned according to the groups specified. Each partition is written as a separate parquet file with a filename derived from the basename template and the group values. Any existing data on the file path is overwritten.
#'
#' @examples
#' library(arrow)
#' dataset <- open_dataset("path/to/data")
#' file_partition(dataset, "path/to/save", "basename_template")
#' file_partition(dataset, "path/to/save", "basename_template", groups = c("year", "month"))
#'
#' @export
file_partition <- function(explanatory_variable_sources, 
                           path, 
                           basename_template, 
                           years = 2007:2009,
                           months = 1:2) {
  
  # Extract filtering groups
  data_groups <- expand.grid(year = years, month = months)
  
  # Write each group using file partitioning instead of hive partitioning
  files <- map_vec(years, function(y) {
    
    map_vec(months, function(m) {
      
    # Okay I need to map through the dataset then map through each file in the list
    first 
    
    # Use reduce to iteratively open and join in parquet datasets without collecting
    joined_dataset <- reduce(dataset_list, 
                             .init = arrow::open_dataset(explanatory_variable_sources$list_of_files[[1]]) |> filter(year == y), 
                             function(x, y) {x |> left_join(arrow::open_dataset(y))})

    # Set up a file name for file based partitioning instead of hive
    filename <- paste(names(unlist(data_groups[i,])), unlist(data_groups[i,]), sep = "_", collapse = "_")
    filename <- glue::glue("{tools::file_path_sans_ext(basename_template)}_{filename}.{tools::file_ext(basename_template)}")
    
    # Write the filtered parquet file
    arrow::write_parquet(filtered_data, sink = file.path(path, filename))
    
    # Clean up
    rm(filtered_data)
    
    # Return filename
    filename
  })
  })
  
  # Return list of files
  files
}
