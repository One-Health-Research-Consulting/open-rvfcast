#' Partition files for DuckDB database
#'
#' This function is designed to partition files for the DuckDB database in R. 
#' It takes in the sources of the files, path to store the partitioned files,
#' the template for file naming and the range of years and months. 
#'
#' @author Nathan C. Layman
#'
#' @param sources The sources from where the files will be read.
#' @param path The directory where the partitioned files will be stored. Default is "data/explanatory_variables".
#' @param basename_template The template used to name the partitioned files. Default is "explanatory_variables_{.y}_{.m}".
#' @param years The years for which the files need to be partitioned. Default is 2007:2010.
#' @param months The months for which the files need to be partitioned. Default is 1:12.
#'
#' @return A string vector representing the filepath to each partitioned file.
#'
#' @note The function creates a connection with DuckDB database and loads file from each source. Then performs a join operation, next it partitions files 
#' based on the selected years and months. The partitioned files are saved in Parquet format with the gzip codec.
#'
#' @examples
#' file_partition_duckdb(sources = list("source_path1","source_path2"),
#' path = 'data/explanatory_variables', basename_template = "explanatory_variables_{.y}_{.m}",
#' years = 2007: 2010,  months = 1:12 )  
#'
#' @export
file_partition_duckdb <- function(sources, # A named, nested list of parquet files
                                  model_dates_selected,
                                  local_folder = "data/africa_full_data",
                                  basename_template = "africa_full_data_{model_dates_selected}.parquet",
                                  overwrite = FALSE,
                                  ...) {
  
  # NCL change to branch off of model date for combo
  # This approach does work. Only writing complete datasets
  # 2005 doesn't have any outbreak history so what do we input?
  # Next step is lagged data.
  # JOINING ON model_dates_selected means going back and changing 'base_date' to 'date' in ecmwf_transformed and anomaly
  
  # Check that we're only working on one date at a time
  stopifnot(length(model_dates_selected) == 1)
  
  # Set filename
  save_filename <- file.path(local_folder, glue::glue(basename_template))
  message(paste0("Combining explanatory variables for ", model_dates_selected))
  
  # Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(save_filename)
  }
  
  # Create a connect to a DuckDB database
  con <- duckdb::dbConnect(duckdb::duckdb())
  
  # For each explanatory variable target create a table filtered appropriately
  walk2(names(sources), sources, function(table_name, list_of_files) {
      
    # Prepare the list of files
    parquet_list <- glue::glue("SELECT * FROM '{list_of_files}'")
    
    file_schemas <- map(list_of_files, ~arrow::open_dataset(.x)$schema)
    unified_schema <- all(map_vec(file_schemas, ~.x == file_schemas[[1]]))
    
    parquet_filter <- c()
    if(!is.null(file_schemas[[1]]$date)) parquet_filter <- c(parquet_filter, paste("date = '", model_dates_selected, "'"))
    if(length(parquet_filter)) {
      parquet_filter <- paste("WHERE", paste(parquet_filter, collapse = " AND "))
    } else {
      parquet_filter = ""
    }
  
    parquet_list <- glue::glue("{parquet_list} {parquet_filter}")
      
    # Check if all schemas are identical
    if(unified_schema) {
      # If all schema are identical: union all files
      parquet_list <- paste(parquet_list, collapse = " UNION ALL ")
      
    } else {
      
      # If not: inner join all files
      parquet_list <- glue::glue("({parquet_list})")
      parquet_list <- glue::glue("{parquet_list} AS {tools::file_path_sans_ext(basename(list_of_files))}")
      parquet_list <- paste0("SELECT * FROM ", paste(parquet_list, collapse = " NATURAL JOIN "))
    }
    
    # Set up query to add the table to the database
    query <- glue::glue("CREATE OR REPLACE TABLE {table_name} AS {parquet_list}")
    
    # Execute the query
    add_table_result <- DBI::dbExecute(con, query) 
    message(glue::glue("{table_name} table created with {add_table_result} rows"))
  })  
  
  # Set up a natural inner join for all the tables and output the result to file(s)
  # Ensure that there are NO duplicates and that all rows with NULL value have been dropped
  query <- glue::glue("SELECT DISTINCT * FROM {paste(names(sources), collapse = ' NATURAL JOIN ')}")
  query <- paste0("COPY (",
                  query,
                  glue::glue(" WHERE COLUMNS(*) IS NOT NULL) TO '{save_filename}' (FORMAT PARQUET, COMPRESSION 'GZIP');"))
   
  # Execute the join
  rows_written <- DBI::dbExecute(con, query) 
  message(glue::glue("{rows_written} rows in joined dataset"))
  
  # Clean up the database connection
  duckdb::dbDisconnect(con)
  
  # Return filename for the list
  save_filename
  
}
