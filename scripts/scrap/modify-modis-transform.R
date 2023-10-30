for(mod in modis_ndvi_transformed){
  new_name <- str_remove(mod, "_to_\\d{4}-\\d{2}-\\d{2}")
  open_dataset(mod) |> 
    select(-end_date) |> 
    write_parquet(new_name, compression = "gzip", compression_level = 5)
  
}
