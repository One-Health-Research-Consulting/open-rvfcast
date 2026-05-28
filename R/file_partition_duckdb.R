#' Partition Files using DuckDB
#'
#' This function reads from various data sources, applies filters based on selected dates,
#' performs some data transformation, and writes out the data to a Parquet file.
#' If an existing file with the same name is found and overwrite is set to FALSE,
#' the function returns without any write operation.
#'
#' @author Nathan C. Layman
#'
#' @param temporal_sources A single-row tibble with a 'date' column and columns containing file paths for temporal data sources.
#' @param static_sources A named list of fully qualified file paths to static parquet files.
#' @param local_folder A character string indicating the data output directory. Default is 'data/africa_full_data'.
#' @param basename_template A character string that will be used to create the output file name along with the date. Default is 'africa_full_data_{date}.parquet'.
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
#'   temporal_sources = tibble(date = "2022-01-01", forecasts = "file1.parquet"),
#'   static_sources = list(soil = "soil.parquet", elevation = "elevation.parquet"),
#'   local_folder = "output",
#'   basename_template = "output_{date}.parquet",
#'   overwrite = TRUE
#' )
#'
#' @export
file_partition_duckdb <- function(temporal_sources,
                                  static_sources,
                                  local_folder = "data/africa_full_data",
                                  basename_template = "africa_full_data_{date}.parquet",
                                  overwrite = FALSE,
                                  ...) {
  # Extract the date from temporal_sources (single row tibble)
  stopifnot(nrow(temporal_sources) == 1)
  date <- temporal_sources$date

  # Validate temporal source files exist
  # Exclude date and tar_group columns, keep only file path columns
  temporal_files <- as.list(temporal_sources[, !names(temporal_sources) %in% c('date', 'tar_group'), drop = FALSE])
  for (source_name in names(temporal_files)) {
    file_path <- unname(temporal_files[[source_name]])

    # Check if file is NA
    if (length(file_path) == 0 || is.na(file_path)) {
      stop(glue::glue("No {source_name} file available for date {date}"))
    }

    # Check if file exists
    if (!file.exists(file_path)) {
      stop(glue::glue("{source_name} file does not exist: {file_path}"))
    }
  }

  # Set filename
  save_filename <- file.path(local_folder, glue::glue(basename_template))
  message(paste0("Combining explanatory variables for ", date))

  # Check if file already exists and can be read
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  existing_dataset <- error_safe_read_parquet(save_filename)

  if(!is.null(existing_dataset) & !overwrite) {
    # Check if file has data - if zero rows, overwrite anyway
    row_count <- existing_dataset |> count() |> collect() |> pull(n)
    if(row_count > 0) {
      message(glue::glue("{basename(save_filename)} already exists, has rows, and overwrite is not TRUE, skipping"))
      return(save_filename)
    } else {
      message(glue::glue("{basename(save_filename)} exists but has zero rows, overwriting"))
    }
  }

  # Create a connect to a DuckDB database
  con <- duckdb::dbConnect(duckdb::duckdb())

  # Combine temporal and static sources into a single named list
  # Extract temporal file paths from the tibble (excluding date and tar_group columns)
  # Remove names from file paths (targets with format = "file" add names)
  temporal_files <- lapply(as.list(temporal_sources[, !names(temporal_sources) %in% c('date', 'tar_group'), drop = FALSE]), unname)
  all_sources <- c(temporal_files, static_sources)

  # For each explanatory variable target create a table filtered appropriately
  purrr::walk2(names(all_sources), all_sources, function(table_name, list_of_files) {

    ## For temporal files, they should already be filtered to the correct date
    ## For static files, use them as-is
    filtered_files <- unname(list_of_files)

    file_schemas <- purrr::map(filtered_files, ~ arrow::open_dataset(.x)$schema)
    unified_schema <- all(purrr::map_vec(file_schemas, ~ .x == file_schemas[[1]]))

    parquet_filter <- c()
    if (!is.null(file_schemas[[1]]$date)) parquet_filter <- c(parquet_filter, paste("date = '", date, "'"))
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

    # Round x and y to 7 decimal places so that cells from data processed with
    # different versions of continent_raster_template (floating-point drift of
    # ~1e-15) join correctly across all sources
    parquet_list <- glue::glue(
      "SELECT ROUND(x, 7) AS x, ROUND(y, 7) AS y, * EXCLUDE (x, y) FROM ({parquet_list})"
    )

    # Set up query to add the table to the database
    query <- glue::glue("CREATE OR REPLACE TABLE {table_name} AS {parquet_list}")

    # Execute the query
    add_table_result <- DBI::dbExecute(con, query)
    message(glue::glue("{table_name} table created with {add_table_result} rows"))
  })

  # Set up a natural inner join for all the tables and output the result to file(s)
  # Ensure that there are NO duplicates and that all rows with NULL value have been dropped
  query <- glue::glue("SELECT DISTINCT * FROM {paste(names(all_sources), collapse = ' NATURAL JOIN ')}")
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
