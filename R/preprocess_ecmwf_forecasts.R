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
                                       download_directory,
                                       preprocessed_directory) {
  
  suppressWarnings(dir.create(here::here(preprocessed_directory), recursive = TRUE))
  existing_files <- list.files(preprocessed_directory)
  
  # filename for postprocessed file
  filename <- str_replace(basename(ecmwf_forecasts_download), "\\.grib", "\\.csv.gz")
  
  # begin processing
  message(paste0("Preprocessing ", ecmwf_forecasts_download))
  
  if(filename %in% existing_files){
    message("file already exists, skipping preprocess")
    return(file.path(preprocessed_directory, filename)) # skip if file exists
  }
  
  # begin processing
  message(paste0("Processing ", ecmwf_forecasts_download))
  
  file <- here::here(download_directory, ecmwf_forecasts_download)
  
  # read in with terra
  grib <- terra::rast(file)
  
  # get associated metadata and remove non-df rows
  grib_meta <- system(paste("grib_ls", file), intern = TRUE)
  remove <- c(1, (length(grib_meta)-2):length(grib_meta)) 
  grib_meta <- grib_meta[-remove]
  grib_meta <- read.table(text = grib_meta, header = TRUE)
  
  # create IDs for columns headers (NOTE these are non unique because there are multiple models per outcome)
  grib_meta <- as_tibble(grib_meta) |>
    mutate(id = paste(dataDate, stepRange, dataType, shortName, sep = "_"))
  names(grib) <- grib_meta$id
  
  # covert SpatRaster to dataframe for storage
  dat <- as.data.frame(grib, xy = TRUE) |> 
    pivot_longer(-c("x", "y"), names_to = "id")
  
  write_csv(dat, here::here(preprocessed_directory, filename))
  
  return(file.path(preprocessed_directory, filename))
  
}
