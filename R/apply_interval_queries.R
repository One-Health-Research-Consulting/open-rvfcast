#' Apply Interval Queries to Multiple Columns
#'
#' This function applies interval-based aggregation queries to multiple columns
#' in a DuckDB table, creating lag variables for each specified column.
#'
#' @author Nathan C. Layman
#'
#' @param con DBI connection object to a DuckDB database
#' @param table_name Character. Name of the table to query and modify
#' @param columns Character vector. Names of columns to apply interval calculations to
#' @param interval Numeric. The length of each interval in days
#' @param num_periods Integer. Number of intervals to generate
#' @param date_col Character. Name of the date column for temporal calculations
#' @param group_cols Character vector. Names of columns to group by (e.g., spatial coordinates)
#' @param agg_function Character. SQL aggregation function (e.g., "AVG", "SUM", "MIN", "MAX")
#' @param suffix Character. Suffix to append to generated column names
#'
#' @return Invisibly returns TRUE if successful
#'
#' @details
#' The function sequentially applies the generate_interval_query function to each column
#' in the specified columns vector. It modifies the original table in-place, adding new
#' columns with the calculated interval statistics.
#'
#' @examples
#' # Apply 30-day average calculations to both temperature and precipitation columns
#' con <- DBI::dbConnect(duckdb::duckdb())
#' apply_interval_queries(
#'   con = con,
#'   table_name = "climate_data",
#'   columns = c("temperature", "precipitation"),
#'   interval = 30,
#'   num_periods = 3
#' )
#'
#' @export
apply_interval_queries <- function(
    con,
    table_name,
    columns,
    interval = 30,
    num_periods = 3,
    date_col = "date",
    group_cols = c("x", "y"),
    agg_function = "AVG",
    suffix = "lag") {

  # Verify connection is valid
  if (!DBI::dbIsValid(con)) {
    stop("Invalid DuckDB connection provided")
  }

  # Verify table exists
  if (!table_name %in% DBI::dbListTables(con)) {
    stop(paste("Table", table_name, "not found in the database"))
  }

  # Check if columns exist in the table
  table_cols <- DBI::dbListFields(con, table_name)
  missing_cols <- columns[!columns %in% table_cols]

  if (length(missing_cols) > 0) {
    stop(paste("The following columns were not found in the table:",
               paste(missing_cols, collapse = ", ")))
  }

  # Apply interval query to each column sequentially
  for (col in columns) {
    message(paste("Processing column:", col))

    # Generate the query
    query <- generate_interval_query(
      interval = interval,
      num_periods = num_periods,
      value_col = col,
      date_col = date_col,
      group_cols = group_cols,
      agg_function = agg_function,
      suffix = suffix,
      table_name = table_name
    )

    # Execute in a transaction
    DBI::dbExecute(con, "BEGIN TRANSACTION")

    # Create a backup table for reference
    temp_table <- paste0("temp_", gsub("[^a-zA-Z0-9]", "_", table_name), "_", gsub("[^a-zA-Z0-9]", "_", col))
    DBI::dbExecute(con, glue::glue("CREATE TEMPORARY TABLE {temp_table} AS SELECT * FROM {table_name}"))

    # Apply the interval query
    result_query <- glue::glue("CREATE OR REPLACE TABLE {table_name} AS WITH source_data AS (SELECT * FROM {temp_table}) {query}")
    tryCatch({
      DBI::dbExecute(con, result_query)
      DBI::dbExecute(con, glue::glue("DROP TABLE {temp_table}"))
      DBI::dbExecute(con, "COMMIT")
      message(paste("Successfully added lag columns for", col))
    }, error = function(e) {
      DBI::dbExecute(con, "ROLLBACK")
      message(paste("Error processing column", col, ":", e$message))
      stop(e)
    })
  }

  # Return invisibly
  invisible(TRUE)
}
