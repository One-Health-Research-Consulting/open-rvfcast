#' Partition Files using DuckDB
#'
#' This function reads from various data sources, applies filters based on selected dates,
#' performs some data transformation, and writes out the data to a Parquet file.
#' If an existing file with the same name is found and overwrite is set to FALSE,
#' the function returns without any write operation.
#'
#' @author Nathan C. Layman
#'
#' @param sources A named list of fully qualified file paths to parquet files.
#' @param dates_to_process A character vector of dates to filter data. Only one date is allowed in this vector.
#' @param local_folder A character string indicating the data output directory. Default is 'data/africa_full_data'.
#' @param basename_template A character string that will be used to create the output file name along with the selected date. Default is 'africa_full_data_{dates_to_process}.parquet'.
#' @param overwrite A logical indicating whether to overwrite an existing file if found. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A string containing the filepath to the processed Parquet file.
#'
#' @note In case of an imbalanced schema in source files, the function will perform a Natural Join across
#' the sources using Spark's DataFrame API, else it will perform a Union All operation.
#'
#' @examples
#' file_partition_duckdb(
#'   sources = list(s1 = "data/s1.parquet", s2 = "data/s2.parquet"),
#'   dates_to_process = "2022-01-01",
#'   local_folder = "output",
#'   basename_template = "output_{dates_to_process}.parquet",
#'   overwrite = TRUE
#' )
#'
#' @export
file_partition_duckdb <- function(sources, # A named, nested list of parquet files
                                  dates_to_process,
                                  local_folder = "data/africa_full_data",
                                  basename_template = "africa_full_data_{dates_to_process}.parquet",
                                  overwrite = FALSE,
                                  ...) {
  # NCL change to branch off of model date for combo
  # This approach does work. Only writing complete datasets
  # 2005 doesn't have any outbreak history so what do we input?
  # Next step is lagged data.
  # JOINING ON dates_to_process means going back and changing 'base_date' to 'date' in ecmwf_transformed and anomaly

  # Check that we're only working on one date at a time
  stopifnot(length(dates_to_process) == 1)

  # Set filename
  save_filename <- file.path(local_folder, glue::glue(basename_template))
  message(paste0("Combining explanatory variables for ", dates_to_process))

  # Check if file already exists and can be read
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)

  if (!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(save_filename)
  }

  # Create a connect to a DuckDB database
  con <- duckdb::dbConnect(duckdb::duckdb())

  # For each explanatory variable target create a table filtered appropriately
  purrr::walk2(names(sources), sources, function(table_name, list_of_files) {
    
    ## Select out only the needed date
    filtered_files <- list_of_files[grepl(dates_to_process, list_of_files)]
    if (length(filtered_files) == 0) { filtered_files <- list_of_files }

    file_schemas <- purrr::map(filtered_files, ~ arrow::open_dataset(.x)$schema)
    unified_schema <- all(purrr::map_vec(file_schemas, ~ .x == file_schemas[[1]]))

    parquet_filter <- c()
    if (!is.null(file_schemas[[1]]$date)) parquet_filter <- c(parquet_filter, paste("date = '", dates_to_process, "'"))
    if (length(parquet_filter)) {
      parquet_filter <- paste("WHERE", paste(parquet_filter, collapse = " AND "))
    } else {
      parquet_filter <- ""
    }

    # Check if all schemas are identical
    if (unified_schema) {
      # If all schema are identical: union all files
      files_list <- paste0("'", filtered_files, "'", collapse = ", ")
      parquet_list <- glue::glue("SELECT * FROM read_parquet([{files_list}]) {parquet_filter}")
    } else {
      # If not: inner join all files
      parquet_list <- glue::glue("SELECT * FROM '{filtered_files}' {parquet_filter}")
      parquet_list <- glue::glue("({parquet_list})")
      as_names <- gsub("\\..*", "", basename(filtered_files))
      parquet_list <- glue::glue("{parquet_list} AS {gsub('-', '_', as_names)}")
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

  # Execute the join
  rows_written <- DBI::dbExecute(con, query)
  message(glue::glue("{rows_written} rows in joined dataset"))

  # Clean up the database connection
  duckdb::dbDisconnect(con)

  # Return filename for the list
  save_filename
}

# Example duckdb join query generated above
# COPY (
#   SELECT DISTINCT *
#     FROM forecasts_anomalies
#     NATURAL JOIN weather_anomalies_lagged
#     NATURAL JOIN ndvi_anomalies_lagged
#     NATURAL JOIN weather_anomalies
#     NATURAL JOIN ndvi_anomalies
#     NATURAL JOIN soil_preprocessed
#     NATURAL JOIN aspect_preprocessed
#     NATURAL JOIN slope_preprocessed
#     NATURAL JOIN glw_preprocessed
#     NATURAL JOIN elevation_preprocessed
#     NATURAL JOIN bioclim_preprocessed
#     NATURAL JOIN landcover_preprocessed
#     WHERE COLUMNS(*) IS NOT NULL
# ) TO 'data/africa_full_rvf_model_data/africa_full_data_2005-01-14.parquet'
#     (FORMAT PARQUET, COMPRESSION 'GZIP');
