generate_lag_query <- function(
    interval,
    num_periods,
    value_col = "anomaly_ndvi",
    date_col = "date",
    location_cols = c("x", "y"),
    table_name = "data") {

  # Create the lag queries for each period
  lag_queries <- sapply(1:num_periods, function(i) {
    lower_bound <- (i - 1) * interval + 1
    upper_bound <- i * interval

    # Build join conditions
    join_conditions <- paste(sapply(location_cols, function(col) {
      paste0("d2.", col, " = d1.", col)
    }), collapse = " AND ")

    glue::glue(
      "(SELECT AVG(d2.{value_col})
        FROM {table_name} d2
        WHERE {join_conditions}
          AND d2.{date_col} BETWEEN d1.{date_col} - INTERVAL '{upper_bound}' DAY 
                               AND d1.{date_col} - INTERVAL '{lower_bound}' DAY
      ) AS {value_col}_lag_{upper_bound}"
    )
  })

  # Create the query
  query <- glue::glue(
    "SELECT d1.*, {paste(lag_queries, collapse = ',\n')}
     FROM {table_name} d1"
  )

  return(query)
}

# Step 1: Create connection
con <- DBI::dbConnect(duckdb::duckdb())

# Step 2: Add parquet files as table
parquet_files <- ndvi_anomalies
parquet_files_str <- paste0("'", paste(parquet_files, collapse = "', '"), "'")
DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TABLE data AS SELECT * FROM read_parquet([{parquet_files_str}])"))

test <- apply_interval_queries(con,
    table_name = "data",
    columns = c("anomaly_ndvi", "anomaly_scaled_ndvi"),
    interval = 30,
    num_periods = 3,
    date_col = "date",
    group_cols = c("x", "y"),
    agg_function = "AVG",
    suffix = "lag")

# Step 3: Generate and execute query for ndvi, storing results back in the table
ndvi_query <- generate_interval_query(
  interval = 30,
  num_periods = 3,
  value_col = "anomaly_ndvi",
  date_col = "date",
  group_cols = c("x", "y"),
  agg_function = "SUM",
  suffix = "lag",
  table_name = "data"
)

DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TABLE data AS {ndvi_query}"))

# Step 4: Generate and execute query for temperature, appending to existing results
temp_query <- generate_interval_query(
  interval = 30,
  num_periods = 3,
  value_col = "temperature",
  table_name = "data"
)
DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TABLE data AS {temp_query}"))

# Step 5: Perform any additional operations on the data table
# ...

# Step 6: Save final output as parquet
final_result <- DBI::dbGetQuery(con, "SELECT * FROM data")
arrow::write_parquet(final_result, "final_result.parquet")

# Close connection
DBI::dbDisconnect(con)
