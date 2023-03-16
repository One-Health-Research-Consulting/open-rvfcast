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
                                       preprocessed_directory,
                                       n_workers = NULL) {
  
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
    mutate(variable_id = as.character(glue::glue("{data_date}_step{step_range}_{data_type}_{short_name}"))) |> 
    mutate(data_date = ymd(data_date))  |> 
    select(-grid_type, -packing_type, -level, -type_of_level, -centre, -edition)
  
  # these are all the actual combos of date, step, data type, and variable
  variable_ids <- meta$variable_id  
  
  # because there are multiple model iterations represented, this makes the id's actually unique, which we need for transformations below
  variable_ids_unique <- make.names(names = variable_ids, unique = TRUE) 
  
  # create lookup to be able to retrieve the variable_ids from variable_ids_unique
  names(variable_ids) <- variable_ids_unique 
  
  # set columns headers on grib data to unique ids
  names(grib) <- variable_ids_unique
  
  # covert grib SpatRaster to dataframe for storage
  dat <-  as.data.frame(grib, xy = TRUE)
  
  # transform to long using reshape2 package (more memory efficient than tidyr or data.table)
  # then replace the unique variable id with the non-unique variable id to facilitate grouping
  dat_long <- dat |> 
    reshape2::melt(id.vars = c("x", "y"), variable.name = "variable_id_unique") |>
    as_tibble() |> 
    mutate(variable_id = variable_ids[variable_id_unique]) |> 
    select(-variable_id_unique)
  
  # group split by variable id to calculate mean and sd by x and y
  # i.e., we're summarizing over model iterations
  dat_split <- dat_long |> 
    group_split(variable_id)
  
  if(is.null(n_workers)) n_workers <- as.integer(Sys.getenv("N_PARALLEL_CORES"))
  
  dat_sum <- bettermc::mclapply(dat_split,
                       mc.silent = FALSE,
                       mc.progress = TRUE,
                       mc.allow.fatal = TRUE,
                       mc.preschedule = FALSE, # mc.preschedule = F is dynamic scheduling
                       mc.cores = n_workers, 
                       function(grp){
                         grp |> 
                           group_by(x, y, variable_id) |> 
                           summarize(mean = mean(value), std = sd(value)) |> 
                           ungroup()
                       })
  
  dat_out <- reduce(dat_sum, bind_rows)
  
  write_parquet(dat_out, here::here(preprocessed_directory, filename), compression = "gzip", compression_level = 5)
  
  return(file.path(preprocessed_directory, filename))
  
}
