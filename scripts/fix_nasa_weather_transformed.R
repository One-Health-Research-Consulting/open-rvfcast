ttt <- arrow::read_parquet("data/nasa_weather_raw/nasa_weather_raw_2005-01.parquet")
ttt <- arrow::read_parquet("data/nasa_weather_transformed/nasa_weather_transformed_2005-01.parquet")
ttt %>% filter(day == 1) %>% {
  ggplot(., aes(x, y, z = relative_humidity)) +
    geom_tile(aes(fill = relative_humidity)) +
    scale_x_continuous(limits = c(19, 22)) +
    scale_y_continuous(limits = c(19, 22))
}
