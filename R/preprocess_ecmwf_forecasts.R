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
                                       output_filename) {
  
  grib_files <- list.files(ecmwf_forecasts_download, pattern = ".grib", full.names = TRUE)

  out <- map_dfr(grib_files, function(file){
    
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
    as.data.frame(grib, xy = TRUE) |> 
      pivot_longer(-c("x", "y"), names_to = "id")
  
    })
  
  write_csv(out, output_filename)
  
  return(output_filename)
  
}
