#' Partition Files Using DuckDB
#'
#' This function partitions files on disk using DuckDB and saves them to the specified directory. It 
#' allows for partitioning of data in an efficient and memory-sensitive manner. The function relies 
#' on DuckDB to query data from Parquet format files and merge them in a single table.
#'
#' @author Nathan Layman
#'
#' @param explanatory_variable_sources A named list of files with the names being the type of file (dynamic or static).
#' @param path The directory where the partitions should be saved. It is created if it doesn't exist. Default is 'data/explanatory_variables'.
#' @param years The years of the data you want to partition. Default is 2007 to 2010.
#' @param months The months of the year associated with the data. Default is 1 to 12.
#'
#' @return A vector of paths to the saved file partitions.
#'
#' @note The function leverages the in-memory capabilities of DuckDB to merge large datasets in an efficient 
#' and memory-sensitive manner. You should make sure DuckDB is installed and working in your R environment.
#'
#' @examples
#' file_partition(explanatory_variable_sources = list("static" = static_files, "dynamic" = dynamic_files),
#'                path = "./data/partitioned_files",
#'                years = 2007:2009,
#'                months = 1:3)
#'               
#' @export
file_partition_duckdb <- function(explanatory_variable_sources, 
                                  path = "data/explanatory_variables",
                                  years = 2007:2010,
                                  months = 1:12) {
  
  files <- map2_vec(years, month, function(.y, .m) {
    
    # Create a connect to a DuckDB database
    con <- duckdb::dbConnect(duckdb::duckdb())
    
    # For each explanatory variable target create a table filtered appropriately
    pwalk(explanatory_variable_sources, function(type, name, list_of_files) {
      
      # Prepare the list of files
      parquet_list <- glue::glue("SELECT * FROM '{list_of_files}'")
      
      # Filter if the type is dynamic to reduce as much as possible the memory footprint
      if(type == "dynamic") parquet_list <- glue::glue("{parquet_list} WHERE year == {.y} AND month == {.m}")
      
      # Check if all schemas are identical
      if(all(map_vec(list_of_files, ~arrow::open_dataset(.x)$schema == arrow::open_dataset(list_of_files[[1]])$schema))) {
        
        # If all schema are identical: union all files
        parquet_list <- paste(parquet_list, collapse = " UNION ALL ")
        
      } else {
        
        # If not: inner join all files
        parquet_list <- glue::glue("({parquet_list})")
        parquet_list <- glue::glue("{parquet_list} AS {tools::file_path_sans_ext(basename(list_of_files))}")
        parquet_list <- paste0("SELECT * FROM ", paste(parquet_list, collapse = " NATURAL JOIN "))
      }
      
      # Set up query to add the table to the database
      query <- glue::glue("CREATE TABLE {name} AS {parquet_list}")
      
      # Execute the query
      DBI::dbExecute(con, query) 
    })  
    
    # Establish a file name for the combination of month and year
    filename <- file.path(path, glue::glue("explanatory_variables_{.y}_{.m}.parquet"))
    
    # Set up a natural inner join for all the tables and save it to a file
    query <- glue::glue("COPY (SELECT * FROM {paste(explanatory_variable_sources$name, collapse = ' NATURAL JOIN ')}) TO '{filename}' (FORMAT 'parquet')")
    
    # Execute the join
    DBI::dbExecute(con, query) 
    
    # Clean up the database connection
    duckdb::dbDisconnect(con)
    
    # Return filename for the list
    filename
  }) 
  
  files
}
