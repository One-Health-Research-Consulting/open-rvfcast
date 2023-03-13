#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ecmwf_forecasts_download
#' @param directory
#' @return
#' @author Emma Mendelsohn
#' @export
preprocess_ecmwf_forecasts <- function(ecmwf_forecasts_download,
                                       preprocessed_directory) {
  
  suppressWarnings(dir.create(here::here(preprocessed_directory), recursive = TRUE))
  existing_files <- list.files(preprocessed_directory)
  
  # filename for postprocessed file
  filename <- str_replace(basename(ecmwf_forecasts_download), "\\.grib", "\\.gz.parquet")
  
  # begin processing
  message(paste0("Preprocessing ", ecmwf_forecasts_download))
  
  if(filename %in% existing_files){
    message("file already exists, skipping preprocess")
    return(file.path(preprocessed_directory, filename)) # skip if file exists
  }
  
  file <- here::here(ecmwf_forecasts_download)
  
  # read in with terra
  grib <- terra::rast(file)
  
  # get associated metadata and remove non-df rows
  grib_meta <- system(paste("grib_ls", file), intern = TRUE)
  remove <- c(1, (length(grib_meta)-2):length(grib_meta)) 
  grib_meta <- grib_meta[-remove]
  
  # processing metadata to join with actual data
  meta <- read.table(text = grib_meta, header = TRUE) |>
    as_tibble() |> 
    janitor::clean_names() |> 
    group_by(across(everything())) |> 
    mutate(model_iteration = row_number()) |> 
    ungroup() |> 
    mutate(unique_id = glue::glue("{data_date}_step{step_range}_{data_type}_{short_name}_i{model_iteration}")) |> 
    mutate(data_date = ymd(data_date)) 
  
  # create IDs for columns headers 
   names(grib) <- meta$unique_id
  
  # covert SpatRaster to dataframe for storage
  dat <- as.data.frame(grib, xy = TRUE) |> 
    pivot_longer(-c("x", "y"), names_to = "unique_id") |> 
    left_join(meta, by = "unique_id") |> 
    select(unique_id, data_date, short_name, data_type, step_range, x, y, model_iteration, everything()) |> 
    arrange(data_date, short_name, data_type, step_range, x, y, model_iteration)
  
  write_parquet(dat, here::here(preprocessed_directory, filename), compression = "gzip", compression_level = 5)
  
  return(file.path(preprocessed_directory, filename))
  
}
