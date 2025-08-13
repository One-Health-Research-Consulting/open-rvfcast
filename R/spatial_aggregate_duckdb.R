spatial_aggregate_duckdb <- function(sf_df, 
                                     parquet_file_list, 
                                     predictor_aggregating_functions,
                                     dates_to_process) {
  
  # Check input types
  if (!inherits(sf_df, "sf")) stop("Input `sf_df` must be an SF dataframe.")
  if (!all(sf::st_geometry_type(sf_df) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("SF dataframe must contain POLYGON or MULTIPOLYGON geometries.")
  }
  
  # Note: Switching to duckdb is necessary because there is no statistical mode or most frequent variable (mvf) function
  # in base R. That means methods like arrow_db |> group_by(...) |> summarize(aspect = mvf(aspect)) won't work without
  # first calling collect(). This is super annoying.
  
  arrow_db <- arrow::open_dataset(parquet_file_list) |> filter(date == dates_to_process)
  
  # Check that the setdiff between variable names and predictor_aggregating_functions is zero
  # if not we need to update the predictor_aggregating_function csv file
  predictor_setdiff <- setdiff(predictor_aggregating_functions$var, arrow::schema(arrow_db)$names)
  if(length(predictor_setdiff) != 0) {
    stop(glue::glue("predictor_summary.csv does not match the columns in the provided data. Harmonize [{paste(predictor_setdiff, collapse = ', ')}] before preceeding."))
  }  
  
  # Ensure DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Install and load duckdb's spatial extension
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  
  # Convert SF geometries to WKT for DuckDB
  sf_df <- sf_df %>%
    mutate(geometry_wkt = sf::st_as_text(geometry)) %>%
    sf::st_drop_geometry()
  
  # Write polygons to DuckDB
  DBI::dbWriteTable(con, "polygons", sf_df, overwrite = TRUE)
  
  # Note that arrow objects don't show up in the duckdb catalog
  # And so DBI::dbGetQuery(con, "SHOW TABLES;") won't list the points 
  # table even though it is there. To check that the table was successfully added
  # you can run 'duckdb::duckdb_list_arrow(con)' or add a view via
  # DBI::dbExecute(con, "CREATE VIEW points_view AS SELECT * FROM points")
  # https://github.com/duckdb/duckdb/issues/3948
  
  # Identify grouping columns
  grouping_vars <- predictor_aggregating_functions |> 
    filter(is.na(aggregating_function), !var %in% c("x", "y")) |> 
    pull(var)
  
  sum_vars <- predictor_aggregating_functions |>
    filter(aggregating_function == "SUM") |>
    pull(var)
  
  avg_vars <- predictor_aggregating_functions |>
    filter(aggregating_function == "AVG") |>
    pull(var)
  
  mode_vars <- predictor_aggregating_functions |>
    filter(aggregating_function == "MODE") |>
    pull(var)
  
  # Open the parquet files in arrow then add the data to a table in the duckdb database
  invisible(arrow::to_duckdb(arrow_db, table_name = "points", con = con))
  
  # Build aggregation expressions
  sum_exprs <- paste(glue::glue('SUM("{sum_vars}") AS "{sum_vars}_sum"'), collapse = ",\n")
  avg_exprs <- paste(glue::glue('AVG("{avg_vars}") AS "{avg_vars}_avg"'), collapse = ",\n")
  
  files <- paste(parquet_file_list, collapse = "', '")
  query <- glue::glue("
WITH filtered_points AS (
    SELECT *
    FROM read_parquet(['{files}'])
    WHERE date = '{dates_to_process}'
),
joined AS (
    SELECT 
        polygons.shapeName AS adm, 
        filtered_points.*
    FROM 
        polygons, 
        filtered_points
    WHERE 
        ST_Intersects(
            ST_Point(filtered_points.y, filtered_points.x),
            ST_GeomFromText(polygons.geometry_wkt)
        )
)
SELECT 
    adm, 
    {sum_exprs}, 
    {avg_exprs} 
FROM 
    joined
GROUP BY 
    adm;
  ")
  
  # Execute the query
  result <- DBI::dbGetQuery(con, query)
  
  # Return aggregated results as a dataframe
  return(result)
}
