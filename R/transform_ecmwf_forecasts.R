#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ecmwf_forecasts_downloaded
#' @param ecmwf_forecasts_transformed_directory
#' @param continent_raster_template
#' @param overwrite
#' @return
#' @author Emma Mendelsohn
#' @export
transform_ecmwf_forecasts <- function(ecmwf_forecasts_downloaded,
                                      ecmwf_forecasts_transformed_directory,
                                      continent_raster_template, 
                                      n_workers = 1,
                                      overwrite = FALSE) {
  
  # Get filename for saving from the raw data
  filename <- tools::file_path_sans_ext(basename(ecmwf_forecasts_downloaded))
  save_filename <- glue::glue("{filename}.gz.parquet")
  
  # Check if file already exists
  existing_files <- list.files(ecmwf_forecasts_transformed_directory)
  message(paste0("Transforming ", save_filename))
  if(save_filename %in% existing_files & !overwrite){
    message("file already exists, skipping transform")
    return(file.path(ecmwf_forecasts_transformed_directory, save_filename))
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
  
  # Rename layers with the metadata names
  names(grib) <- meta$variable_id

  # Select only the means columns
  grib_subset <- subset(grib, which(str_detect(names(grib), "fcmean"))) 
  
  # Units conversions
  # 2d = "2m_dewpoint_temperature" = 	2 metre dewpoint temperature K
  # 2t = "2m_temperature" = 	2 metre temperature K
  # tprate = "total_precipitation" =  Total precipitation m/s
  # https://cds.climate.copernicus.eu/cdsapp#!/dataset/seasonal-monthly-single-levels?tab=overview  
  
  # Convert kelvin to celsius (note these columns are mislabeled as C)
  temp_cols <- which(str_ends(names(grib_subset), "2d|2t"))
  grib_temp <- subset(grib_subset, temp_cols)
  grib_temp <- app(grib_temp, function(i) i - 273.15)
  
  # Convert precipitation from m/second to mm/day
  precip_cols <- which(str_ends(names(grib_subset), "tprate"))
  grib_precip <- subset(grib_subset, precip_cols)
  grib_precip <- ifel(grib_precip < 0, 0, grib_precip) # some very small negative values
  grib_precip <- app(grib_precip, function(i) i * 8.64e+7)
  
  # Calculate per-pixel mean for each unique combination, across all models
  # transform to template
  grib_converted <- c(grib_precip, grib_temp)
  grib_index <- as.integer(factor(names(grib_converted), levels = unique(names(grib_converted))))
  grib_means <- terra::tapp(grib_converted, grib_index, "mean", cores = n_workers) |> 
    setNames(unique(names(grib_converted))) |> 
    transform_raster(template = continent_raster_template) |> 
    as.data.frame(xy = TRUE) 
  
  # Using the means, calculate relative humidity from dew point and temp
  dp_cols <- names(grib_means)[str_ends(names(grib_means), "2d")] |> sort() # dew point
  t_cols <- names(grib_means)[str_ends(names(grib_means), "2t")] |> sort() # temp
  assertthat::assert_that(identical(str_remove(dp_cols, "_2d"), str_remove(t_cols, "_2t")))
  
  rel_humidity <- map2_dfc(dp_cols, t_cols, function(dp, t){
    dp <- grib_means[,dp]
    t <- grib_means[, t]
    rh <- 100 * exp((17.625 * dp)/(243.04+dp))/exp((17.625 * t)/(243.04+t))
    assertthat::assert_that(all(rh <= 100 & rh >=0))
    return(rh)
  }) |> set_names(str_replace(dp_cols, "_2d", "_rh"))
  
  # Remove dewpoint temperature and add relative humidity
  grib_means <- grib_means |> 
    select(-dp_cols) |> 
    bind_cols(rel_humidity)|> 
    reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "mean") 
   
  # Not using Sds at the moment
  # grib_sds <- terra::tapp(grib_converted, grib_index, "sd", cores = n_workers) |> 
  #   setNames(unique(names(grib_converted))) |> 
  #   transform_raster(template = continent_raster_template) |> 
  #   as.data.frame(xy = TRUE) |> 
  #   reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id", value.name = "std") |> 
  #   pull(std)
  
  # Add relative humidity to metafile 
  meta_out <- meta |> 
    distinct() |> 
    mutate(short_name = if_else(short_name == "2d", "rh", short_name)) |> 
    mutate(variable_id = str_replace(variable_id, "_2d", "_rh"))
    
  # Link means with metadata for export
  grib_means <- grib_means |> 
    left_join(meta_out, by = "variable_id") |> 
    arrange(variable_id, x, y) |> 
    select(x, y, mean, data_date, step_range, short_name)
  
  # Standardize leads
  n_leads <- grib_means |> 
    group_by(data_date) |> 
    summarize(n = n_distinct(step_range)) |> 
    distinct(n)
  assertthat::are_equal(6, n_leads$n)
  
  grib_means <- grib_means |> 
    group_by(data_date) |> 
    mutate(lead_month = factor(rank(step_range), labels = 1:6)) |> 
    select(-step_range) |> 
    ungroup()

  # Save as parquet 
  write_parquet(grib_means, here::here(ecmwf_forecasts_transformed_directory, save_filename), compression = "gzip", compression_level = 5)
  
  return(file.path(ecmwf_forecasts_transformed_directory, save_filename))
  
  
}
