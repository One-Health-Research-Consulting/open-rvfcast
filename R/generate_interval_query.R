#' Generate SQL Query for Time Interval Statistics
#'
#' Creates a SQL query that computes aggregate statistics (such as average, sum, etc.) 
#' over specified time intervals relative to each date in a dataset. The function 
#' is particularly useful for creating lag variables in time series and spatial-temporal data.
#'
#' @author Nathan C. Layman
#'
#' @param interval Numeric. The length of each interval in days.
#' @param num_periods Integer. Number of intervals to generate.
#' @param value_col Character. Name of the column to calculate statistics on.
#' @param date_col Character. Name of the date column for temporal calculations.
#' @param group_cols Character vector. Names of columns to group by (e.g., spatial coordinates).
#' @param agg_function Character. SQL aggregation function (e.g., "AVG", "SUM", "MIN", "MAX").
#' @param suffix Character. Suffix to append to generated column names.
#' @param table_name Character. Name of the table to query from.
#'
#' @return A character string containing the SQL query.
#'
#' @details
#' The function generates SQL that creates new columns containing aggregate statistics
#' calculated over specified historical time intervals. For example, with interval=30 and
#' num_periods=3, it will create columns with statistics from 1-30 days ago, 31-60 days ago,
#' and 61-90 days ago for each date in the dataset.
#'
#' For each unique combination of group_cols, the function groups data and calculates
#' the specified statistic over each time window.
#'
#' @examples
#' # Generate query for 30-day average of anomaly_ndvi with 3 lag periods
#' avg_query <- generate_interval_query(
#'   interval = 30,
#'   num_periods = 3,
#'   value_col = "anomaly_ndvi"
#' )
#'
#' # Generate query for maximum temperature over 30-day periods
#' max_query <- generate_interval_query(
#'   interval = 30,
#'   num_periods = 3,
#'   value_col = "temperature",
#'   agg_function = "MAX",
#'   suffix = "max"
#' )
#' @export
generate_interval_query <- function(
    interval,
    num_periods,
    value_col = "anomaly_ndvi",
    date_col = "date",
    group_cols = c("x", "y"),
    agg_function = "AVG",  # Options: "AVG", "SUM", "MIN", "MAX", etc.
    suffix = "lag",        # Customizable suffix for column names
    table_name = "data") {

  # Validate the aggregation function
  valid_agg_functions <- c("AVG", "SUM", "MIN", "MAX", "COUNT")
  agg_function <- toupper(agg_function)
  if (!(agg_function %in% valid_agg_functions)) {
    warning(paste("Aggregation function", agg_function, "not in recommended list:", 
                 paste(valid_agg_functions, collapse=", "), 
                 "- but will use it anyway."))
  }

  # Create the interval queries for each period
  interval_queries <- sapply(1:num_periods, function(i) {
    lower_bound <- (i - 1) * interval + 1
    upper_bound <- i * interval

    # Build join conditions
    join_conditions <- paste(sapply(group_cols, function(col) {
      paste0("d2.", col, " = d1.", col)
    }), collapse = " AND ")

    glue::glue(
      "(SELECT {agg_function}(d2.{value_col})
        FROM {table_name} d2
        WHERE {join_conditions}
          AND d2.{date_col} BETWEEN d1.{date_col} - INTERVAL '{upper_bound}' DAY 
                               AND d1.{date_col} - INTERVAL '{lower_bound}' DAY
      ) AS {value_col}_{suffix}_{upper_bound}"
    )
  })

  # Create the query
  query <- glue::glue(
    "SELECT d1.*, {paste(interval_queries, collapse = ',\n')}
     FROM {table_name} d1"
  )

  return(query)
}
