#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ecmwf_forecasts_downloaded
#' @param necmwf_forecasts_directory_transformed
#' @param continent_raster_template
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
transform_ecmwf_forecasts <- function(ecmwf_forecasts_downloaded,
                                      ecmwf_forecasts_directory_transformed,
                                      continent_raster_template, 
                                      n_workers = 1,
                                      overwrite = FALSE) {
  
  # Get filename for saving from the raw data
  filename <- tools::file_path_sans_ext(basename(ecmwf_forecasts_downloaded))
  save_filename <- glue::glue("{filename}.gz.parquet")
  
  # Check if file already exists
  existing_files <- list.files(ecmwf_forecasts_directory_transformed)
  message(paste0("Transforming ", save_filename))
  if(save_filename %in% existing_files & !overwrite){
    message("file already exists, skipping transform")
    return(file.path(ecmwf_forecasts_directory_transformed, save_filename))
  }
  
  # Read in with terra
  grib <- rast(ecmwf_forecasts_downloaded)
  
  # Read in continent template raster
  continent_raster_template <- rast(continent_raster_template)
  
  # Get associated metadata and remove non-df rows
  grib_meta <- system(paste("grib_ls", ecmwf_forecasts_downloaded), intern = TRUE)
  remove <- c(1, (length(grib_meta)-2):length(grib_meta)) 
  grib_meta <- grib_meta[-remove]
  
  # Processing metadata to join with actual data
  meta <- read.table(text = grib_meta, header = TRUE) |>
    as_tibble() |> 
    janitor::clean_names() |> 
    mutate(variable_id = as.character(glue::glue("{data_date}_step{step_range}_{data_type}_{short_name}"))) |> 
    mutate(data_date = ymd(data_date))  |> 
    select(-grid_type, -packing_type, -level, -type_of_level, -centre, -edition)
  
  # Calculate per-pixel mean and std for each unique combination, across all models
  # transform to template
  grib_index <- as.integer(factor(meta$variable_id, levels = unique(meta$variable_id)))
  grib_means <- terra::tapp(grib, grib_index, "mean", cores = n_workers) |> 
    setNames(unique(meta$variable_id)) |> 
    transform_raster(template = continent_raster_template) |> 
    as.data.frame(xy = TRUE) |> 
    reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "mean") 
  grib_sds <- terra::tapp(grib, grib_index, "sd", cores = n_workers) |> 
    setNames(unique(meta$variable_id)) |> 
    transform_raster(template = continent_raster_template) |> 
    as.data.frame(xy = TRUE) |> 
    reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "std") |> 
    pull(std)
  
  # Link with metadata for export
  dat_out <- grib_means |> 
    mutate(std = grib_sds) |> 
    left_join(distinct(meta), by = "variable_id") |> 
    arrange(variable_id, x, y) |> 
    select(x,y, mean, std, data_date, step_range, data_type, short_name)
  
  # Save as parquet 
  write_parquet(dat_out, here::here(ecmwf_forecasts_directory_transformed, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(ecmwf_forecasts_directory_transformed, save_filename))
  
  
}
