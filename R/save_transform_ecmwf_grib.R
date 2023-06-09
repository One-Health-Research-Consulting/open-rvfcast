#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ecmwf_forecasts_downloaded
#' @param transform_directory
#' @param verbose
#' @return
#' @author Emma Mendelsohn
#' @export
save_transform_ecmwf_grib <- function(ecmwf_forecasts_downloaded,
                                      transform_directory =
                                      paste0(str_replace(ecmwf_forecasts_directory,
                                      "gribs", "flat"), "_transformed"),
                                      verbose = TRUE) {
  
  if(verbose) cat(ecmwf_forecasts_downloaded, "\n")
  
  suppressWarnings(dir.create(here::here(transform_directory), recursive = TRUE))
  existing_files <- list.files(transform_directory)
  
  # filename for postprocessed file
   filename <- str_replace(basename(ecmwf_forecasts_downloaded), "\\.grib", "\\.gz.parquet")
  
  if(filename %in% existing_files){
    message("file already exists, skipping preprocess")
    return(file.path(transform_directory, filename)) # skip if file exists
  }
  
  file <- here::here(ecmwf_forecasts_downloaded)
  
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
    mutate(variable_id = as.character(glue::glue("{data_date}_step{step_range}_{data_type}_{short_name}"))) |> 
    mutate(data_date = ymd(data_date))  |> 
    select(-grid_type, -packing_type, -level, -type_of_level, -centre, -edition)
  
  # Calculate per-pixel mean and std for each unique combination, across all models
  grib_index <- as.integer(factor(meta$variable_id, levels = unique(meta$variable_id)))
  grib_means <- terra::tapp(grib, grib_index, "mean", cores = n_workers) |> 
    setNames(unique(meta$variable_id)) |> 
    as.data.frame(xy = TRUE) |> 
    reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "mean") 
  grib_sds <- terra::tapp(grib, grib_index, "sd", cores = n_workers) |> 
    setNames(unique(meta$variable_id)) |> 
    as.data.frame(xy = TRUE) |> 
    reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "std") |> 
    pull(std)
  
  # Link with metadata for export
  dat_out <- grib_means |> 
    mutate(std = grib_sds) |> 
    left_join(distinct(meta), by = "variable_id") |> 
    arrange(variable_id, x, y) |> 
    select(x,y, mean, std, data_date, step_range, data_type, short_name)
  
  write_parquet(dat_out, here::here(transform_directory, filename), compression = "gzip", compression_level = 5)
  
  return(file.path(transform_directory, filename))


}
