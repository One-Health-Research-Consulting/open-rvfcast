get_wahis_outbreak_history_metadata <- function(wahis_outbreak_history) {
  
  dat <- arrow::open_dataset(wahis_outbreak_history)
  
  metadata <- tibble(years = dat |> select(year) |> distinct() |> pull(year, as_vector = T),
                     min = dat |> summarise(across(contains("weight"), ~ min(.x, na.rm = T))) |> collect() |> min(na.rm = T),
                     max = dat |> summarise(across(contains("weight"), ~ max(.x, na.rm = T))) |> collect() |> max(na.rm = T)) |>
    rename(year = years)
  
}