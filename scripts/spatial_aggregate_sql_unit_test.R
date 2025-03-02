
test_polygons <- tar_read(rsa_polygon, store = "data_aggregation_targets") |>
  dplyr::slice(1) |>
  sf::st_geometry() # This gets just the geometry

# Get the bounding box of the test_polygon
bbox <- sf::st_bbox(test_polygons)

# Filter the arrow dataset to only include points within the bounding box
test_points <- arrow::open_dataset(tar_read(ndvi_anomalies)) |>
  dplyr::filter(
    x >= bbox["xmin"],
    x <= bbox["xmax"],
    y >= bbox["ymin"],
    y <= bbox["ymax"]
  ) |>
  dplyr::collect()

# 3. Set up a test DuckDB connection and load data
con <- dbConnect(duckdb())
dbWriteTable(con, "test_points", test_points, overwrite = TRUE)

# 4. Define aggregation functions
predictor_agg <- tibble(
  var = c("shapeName", "date", "anomaly_ndvi", "temperature", "precipitation", "doy"),
  aggregating_function = c(NA, NA, "AVG", "AVG", "SUM", "MODE")
)

# 5. Run spatial aggregation with the small test data
tryCatch({
  result_table <- spatial_aggregate_sql(
    con = con,
    table_name = "test_points",
    output_table_name = "test_agg",
    polygons_sf = test_polygons,
    predictor_aggregating_functions = predictor_agg,
    model_date_selected = "2005-06-24"
  )
  
  # Check results
  test_results <- dbGetQuery(con, "SELECT * FROM test_agg")
  print(test_results)
}, error = function(e) {
  message("Error during test: ", e$message)
}, finally = {
  # Clean up
  dbDisconnect(con)
})
