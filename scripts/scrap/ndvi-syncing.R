sentinel_ndvi_transformed
modis_ndvi_transformed


sent_dataset <- open_dataset(unique(dirname(sentinel_ndvi_transformed))) 
modi_dataset <- open_dataset(unique(dirname(modis_ndvi_transformed))) 
ndvi_dataset <- open_dataset(c(sentinel_ndvi_transformed, modis_ndvi_transformed))


# create lookup table so we know which rows to query, without doing an expansion on the actual data
sent_dates <- sent_dataset |> 
  distinct(start_date, end_date) |> 
  arrange(start_date) |> 
  collect() |> 
  rename(end_date_actual = end_date) |> 
  mutate(end_date = end_date_actual - 1) |> 
  mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y, by = "1 day"))) |> 
  mutate(satellite = "sentinel")

min_sent_start <- min(sent_dates$start_date) # this is when sentinel start

modi_dates <- modi_dataset |> 
  distinct(start_date, end_date) |> 
  arrange(start_date) |> 
  collect() |> 
  rename(end_date_actual = end_date) |> 
  mutate(end_date = end_date_actual - 1) |> 
  mutate(lookup_dates = map2(start_date, end_date, ~seq(.x, .y, by = "1 day"))) |> 
  mutate(satellite = "modis") |> 
  mutate(lookup_dates = map(lookup_dates, ~na.omit(if_else(.>=min_sent_start, NA, .)))) |> 
  filter(map_lgl(lookup_dates, ~length(.) > 0))
