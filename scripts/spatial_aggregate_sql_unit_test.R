library(sf)
library(dplyr)
library(duckdb)
library(tibble)

test_polygons <- tar_read(rsa_polygon, store = "data_aggregation_targets") |> 
  slice(1) |>
  pull(geometry)

test_points <- arrow::open_dataset(tar_read(africa_full_rvf_model_data)) |> filter(date = min(date))

# 1. Create a small test dataset with a few points
test_points <- tibble(
  x = c(-16.37, -16.35, -16.33, -15.77, -15.75),
  y = c(33.03, 33.02, 33.01, 27.93, 27.92),
  date = as.Date(c("2005-06-24", "2005-06-24", "2005-06-24", "2005-06-24", "2005-06-24")),
  doy = c(175, 175, 175, 175, 175),
  anomaly_ndvi = c(0.05, 0.06, 0.07, 0.08, 0.09),
  temperature = c(28, 29, 30, 31, 32),
  precipitation = c(10, 20, 30, 40, 50)
)

# 2. Create small test polygons
# Create two small polygons that will contain our test points
polygon1 <- st_polygon(list(rbind(
  c(-16.4, 33.0), c(-16.3, 33.0), c(-16.3, 33.1), c(-16.4, 33.1), c(-16.4, 33.0)
)))
polygon2 <- st_polygon(list(rbind(
  c(-15.8, 27.9), c(-15.7, 27.9), c(-15.7, 28.0), c(-15.8, 28.0), c(-15.8, 27.9)
)))

# Create the sf object
test_polygons <- st_sf(
  shapeName = c("Region1", "Region2"),
  geometry = st_sfc(polygon1, polygon2, crs = 4326)
)

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
