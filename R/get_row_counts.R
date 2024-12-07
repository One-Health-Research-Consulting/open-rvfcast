#' Get Row Counts for all Tables in a Database
#'
#' This function retrieves the row counts for each table in a database. 
#' It uses a database connection to query the database and structure the results in a data frame. 
#'
#' @author Nathan C. Layman
#'
#' @param con A connection object to a database.
#'
#' @return A data frame with two columns: 'table_name' and 'row_count'. Each row represents a table in
#' the connected database and its corresponding row count.
#'
#' @note This function uses the DBI package for querying the database. Therefore, the connection object 
#' 'con' must be compatible with DBI functions. 
#'
#' @examples
#' con <- DBI::dbConnect(...)
#' get_row_counts(con)
#'
#' @export
get_row_counts <- function(con) {
  
# Get the list of table names in the 'main' schema (default)
tables <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'main'")

# Initialize an empty data frame to store results
row_counts <- data.frame(table_name = character(), row_count = integer(), stringsAsFactors = FALSE)

# Loop through each table name and get the row count
for (table in tables$table_name) {
  query <- paste("SELECT COUNT(*) FROM", table)
  count <- DBI::dbGetQuery(con, query)
  
  # Append the result to the data frame
  row_counts <- rbind(row_counts, data.frame(table_name = table, row_count = as.numeric(count), stringsAsFactors = FALSE))
}
row_counts
}
