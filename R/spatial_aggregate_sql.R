# # Step 1. Map over polygon list using dynamic branching
# # Step 2. Send in parquet file list and aggregation spec
# # Step 3. Use arrow to open the parquet files, filter by bounding box of polygon, collect()
# # Step 4. Aggregate collected data aggregation spec grouping appropriately
# # Step 5. Save parquet file of aggregated region
# # Step 6. Return polygon filename

# library(sf)
# library(dplyr)
# library(tidyr)

# # Example tibble of points
# points <- tibble(
#   lat = c(45.5, 46.2, 44.8, 45.5, 45.5, 46.2),
#   lon = c(-116.2, -115.8, -117.1, -116.2, -116.2, -115.8),
#   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-01", "2024-01-01", "2024-01-02")),
#   category = c("A", "A", "B", "A", "B", "A"),  # A grouping column
#   value1 = c(10, 20, 30, 10, 15, 25),
#   value2 = c(100, 200, 300, 100, 150, 250),
#   value3 = c(5, 10, 15, 5, 5, 10)
# )

# # Example dataframe specifying operations
# agg_spec <- tibble(
#   column = c("value1", "value2", "value3", "category", "date"), 
#   operation = c("mean", "sum", "mode", NA, NA)  # NA means grouping
# )

# # Convert to sf object
# points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)

# # Example polygon (assuming it's already in sf format)
# polygon_sf <- st_read("path/to/polygon.shp")  # Replace with actual path

# # Subset points within the polygon
# points_within <- points_sf[st_within(points_sf, polygon_sf, sparse = FALSE), ] %>%
#   st_drop_geometry()  # Drop spatial info for aggregation

# # Function to compute mode
# mode_function <- function(x) {
#   unique_x <- unique(x)
#   unique_x[which.max(tabulate(match(x, unique_x)))]
# }

# # Identify all columns from the data
# all_columns <- colnames(points_within)

# # Identify columns explicitly mentioned in agg_spec
# agg_columns <- agg_spec$column

# # Identify grouping columns:
# # - Columns that are NOT in `agg_spec`
# # - OR columns that are in `agg_spec` but have `NA` as operation
# grouping_cols <- setdiff(all_columns, agg_columns) %>%
#   union(agg_spec %>% filter(is.na(operation)) %>% pull(column))

# # Identify columns by operation type
# mean_cols <- agg_spec %>% filter(operation == "mean") %>% pull(column)
# sum_cols <- agg_spec %>% filter(operation == "sum") %>% pull(column)
# mode_cols <- agg_spec %>% filter(operation == "mode") %>% pull(column)

# # Perform aggregation dynamically
# aggregated <- points_within %>%
#   group_by(across(all_of(grouping_cols))) %>%  # Auto-detected grouping columns
#   summarise(
#     across(all_of(mean_cols), mean, na.rm = TRUE),
#     across(all_of(sum_cols), sum, na.rm = TRUE),
#     across(all_of(mode_cols), mode_function, .names = "mode_{.col}")
#   )

# # View results
# print(aggregated)


# mode_function <- function(x) {
#   freq_table <- table(x)  # Count occurrences
#   max_freq <- max(freq_table)  # Get highest frequency
#   modes <- names(freq_table[freq_table == max_freq])  # Get values with highest frequency
  
#   # Randomly select one mode if there's a tie
#   return(as.numeric(sample(modes, 1)))  
# }
