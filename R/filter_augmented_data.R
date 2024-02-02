#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param augmented_data
#' @param rsa_polygon
#' @return
#' @author Emma Mendelsohn
#' @export
filter_augmented_data <- function(augmented_data, 
                                  rsa_polygon, 
                                  model_dates_selected,
                                  augmented_data_rsa_directory,
                                  overwrite = FALSE) {
  
  # Set filename
  date_selected <- model_dates_selected
  save_filename <- glue::glue("rsa_augmented_data_{date_selected}.gz.parquet")
  message(paste0("Filtering augmented data for ", date_selected))
  
  # Check if file already exists
  existing_files <- list.files(augmented_data_rsa_directory)
  if(save_filename %in% existing_files & !overwrite) {
    message("file already exists, skipping download")
    return(file.path(augmented_data_rsa_directory, save_filename))
  }
  
  dat <- arrow::read_parquet(glue::glue("{augmented_data}/date={model_dates_selected}/part-0.parquet")) |> 
    rast() 
  crs(dat) <- crs(rast())
  mask(dat, rsa_polygon) |> 
    as.data.frame() |> 
    write_parquet(here::here(augmented_data_rsa_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(augmented_data_rsa_directory, save_filename))
}
