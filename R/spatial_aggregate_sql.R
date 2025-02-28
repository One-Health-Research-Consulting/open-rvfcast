#' Aggregate Data by Spatial Polygons Using SQL
#'
#' This function performs spatial aggregation of point data within polygons using SQL queries
#' in DuckDB. It supports various aggregation functions (SUM, AVG, MODE) for different variables
#' and calculates results for a specific date.
#'
#' @param con DBI connection object to a DuckDB database
#' @param table_name Character. Name of the source table containing point data with x, y coordinates
#' @param output_table_name Character. Name for the output table (default: table_name + "_agg")
#' @param polygons_sf SF dataframe with POLYGON or MULTIPOLYGON geometries
#' @param predictor_aggregating_functions Dataframe with columns 'var' and 'aggregating_function'
#'        specifying how each variable should be aggregated (AVG, SUM, MODE, or NA for grouping variables)
#' @param model_date_selected Character or Date. Single date to filter points by
#' @param id_column Character. Name of the identifier column in polygons_sf (default: "shapeName")
#'
#' @return Invisibly returns the name of the output table
#'
#' @details 
#' The function loads polygon geometries into DuckDB, performs a spatial join with the point data,
#' and then applies the specified aggregation functions within each polygon. It handles three types
#' of aggregations:
#' 
#' - AVG: Calculates the mean of values within each polygon
#' - SUM: Calculates the sum of values within each polygon
#' - MODE: Finds the most common value within each polygon
#' 
#' The function also adds an area column to the output table, calculated from the polygon geometries.
#' All operations are performed within a database transaction to ensure data integrity.
#'
#' @examples
#' \dontrun{
#' # Connect to DuckDB
#' con <- DBI::dbConnect(duckdb::duckdb())
#' 
#' # Define aggregation functions
#' predictor_agg <- tibble::tibble(
#'   var = c("shapeName", "date", "anomaly_ndvi", "temperature", "precipitation", "doy"),
#'   aggregating_function = c(NA, NA, "AVG", "AVG", "SUM", "MODE")
#' )
#' 
#' # Run spatial aggregation
#' spatial_aggregate_sql(
#'   con = con,
#'   table_name = "ndvi_data",
#'   output_table_name = "admin_level_ndvi",
#'   polygons_sf = admin_boundaries,
#'   predictor_aggregating_functions = predictor_agg,
#'   model_date_selected = "2005-06-24"
#' )
#' }
#'
#' @export
spatial_aggregate_sql <- function(
  con,                           # DuckDB connection
  table_name,                    # Name of the source table
  output_table_name = NULL,      # Name for the output table (default: table_name + "_agg")
  polygons_sf,                   # SF dataframe with polygons
  predictor_aggregating_functions, # Dataframe specifying aggregation functions
  model_date_selected,           # Single date to filter by
  id_column = "shapeName"        # Name of the identifier column in polygons_sf
) {
  
# Input validation
  if (!inherits(polygons_sf, "sf")) {
    stop("Input `polygons_sf` must be an SF dataframe.")
  }
  if (!all(sf::st_geometry_type(polygons_sf) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("polygons_sf must be an sf object and contain POLYGON or MULTIPOLYGON geometries.")
  }
  if (length(model_date_selected) != 1) {
    stop("Only one model date can be processed at a time.")
  }
  
  # Set default output table name if not provided
  if (is.null(output_table_name)) {
    output_table_name <- paste0(table_name, "_agg")
  }
  
  # Load polygons into DuckDB
  polygon_table_name <- "temp_polygons"
  
  # Convert polygons to a table with WKT representations
  polygons_df <- polygons_sf %>%
    dplyr::mutate(geometry_wkt = sf::st_as_text(geometry)) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(!!id_column, geometry_wkt)
  
  # Create polygon table in DuckDB
  DBI::dbWriteTable(con, polygon_table_name, polygons_df, overwrite = TRUE)
  
  # Extract grouping and aggregation variables
  grouping_vars <- predictor_aggregating_functions %>%
    dplyr::filter(is.na(aggregating_function), !var %in% c("x", "y")) %>%
    dplyr::pull(var)
  
  # Separate aggregation functions
  agg_functions <- list(
    SUM = predictor_aggregating_functions %>%
      dplyr::filter(aggregating_function == "SUM") %>%
      dplyr::pull(var),
    AVG = predictor_aggregating_functions %>%
      dplyr::filter(aggregating_function == "AVG") %>%
      dplyr::pull(var),
    MODE = predictor_aggregating_functions %>%
      dplyr::filter(aggregating_function == "MODE") %>%
      dplyr::pull(var)
  )
  
  # Begin transaction
  DBI::dbExecute(con, "BEGIN TRANSACTION")
  
  tryCatch({
    # 1. Register ST extensions (for spatial functions)
    DBI::dbExecute(con, "INSTALL spatial;")
    DBI::dbExecute(con, "LOAD spatial;")
    
    # 2. Create a filtered table with points for the selected date
    date_filter <- format(as.Date(model_date_selected), "%Y-%m-%d")
    DBI::dbExecute(con, glue::glue("
      CREATE TEMPORARY TABLE temp_points AS 
      SELECT * FROM {table_name} 
      WHERE date = '{date_filter}'
    "))
    
    # 3. Create a spatial index view to speed up the join
    DBI::dbExecute(con, glue::glue("
      CREATE TEMPORARY TABLE spatial_join AS
      SELECT 
        p.*,
        poly.{id_column}
      FROM temp_points p
      JOIN {polygon_table_name} poly
      ON ST_Within(
        ST_Point(p.x, p.y), 
        ST_GeomFromText(poly.geometry_wkt)
      )
    "))
    
    # 4. Build the aggregation query
    # Construct the GROUP BY clause
    group_by_clause <- paste(c(id_column, grouping_vars), collapse = ", ")
    
    # Construct the aggregation expressions
    agg_expressions <- c()
    
    # Add SUM aggregations
    if (length(agg_functions$SUM) > 0) {
      sum_exprs <- sapply(agg_functions$SUM, function(col) {
        glue::glue("SUM({col}) AS {col}_sum")
      })
      agg_expressions <- c(agg_expressions, sum_exprs)
    }
    
    # Add AVG aggregations
    if (length(agg_functions$AVG) > 0) {
      avg_exprs <- sapply(agg_functions$AVG, function(col) {
        glue::glue("AVG({col}) AS {col}_avg")
      })
      agg_expressions <- c(agg_expressions, avg_exprs)
    }
    
    # Add MODE aggregations (using string_agg trick)
    if (length(agg_functions$MODE) > 0) {
      mode_exprs <- sapply(agg_functions$MODE, function(col) {
        glue::glue("
          (SELECT mode_val FROM (
            SELECT {col} AS mode_val, COUNT(*) AS mode_count 
            FROM spatial_join sj
            WHERE sj.{id_column} = spatial_join.{id_column}
            GROUP BY {col}
            ORDER BY mode_count DESC
            LIMIT 1
          )) AS {col}_mode")
      })
      agg_expressions <- c(agg_expressions, mode_exprs)
    }
    
    # Combine all aggregation expressions
    agg_clause <- paste(agg_expressions, collapse = ", ")
    
# 5. Execute the aggregation query
# Prepare the column list for the SELECT clause
select_cols <- c(id_column)
if (length(grouping_vars) > 0) {
  select_cols <- c(select_cols, grouping_vars)
}

select_clause <- paste(select_cols, collapse = ", ")

# Now construct the query without risking comma issues
agg_query <- glue::glue("
  CREATE TABLE {output_table_name} AS
  SELECT 
    {select_clause}
    {ifelse(length(agg_expressions) > 0, paste0(', ', paste(agg_expressions, collapse = ', ')), '')}
  FROM spatial_join
  GROUP BY {group_by_clause}
")
    
    DBI::dbExecute(con, agg_query)
    
    # 6. Add area information from polygons
    DBI::dbExecute(con, glue::glue("
      ALTER TABLE {output_table_name} 
      ADD COLUMN area DOUBLE;
      
      UPDATE {output_table_name} 
      SET area = (
        SELECT ST_Area(ST_GeomFromText(geometry_wkt)) 
        FROM {polygon_table_name} p
        WHERE p.{id_column} = {output_table_name}.{id_column}
      );
    "))
    
    # 7. Clean up temporary tables
    DBI::dbExecute(con, "DROP TABLE temp_points;")
    DBI::dbExecute(con, "DROP TABLE spatial_join;")
    DBI::dbExecute(con, glue::glue("DROP TABLE {polygon_table_name};"))
    
    # Commit transaction
    DBI::dbExecute(con, "COMMIT")
    
    message(glue::glue("Spatial aggregation completed successfully. Results stored in table '{output_table_name}'"))
    return(invisible(output_table_name))
    
  }, error = function(e) {
    # Rollback in case of error
    DBI::dbExecute(con, "ROLLBACK")
    message("Error during spatial aggregation: ", e$message)
    stop(e)
  })
}
