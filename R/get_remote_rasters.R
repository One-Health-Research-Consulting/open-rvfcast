#' Downloads and Preprocesses Global Elevation Raster Data
#'
#' This function retrieves global elevation raster data from specified URLs, processes it according to specified resampling and aggregation methods, and saves the resulting processed rasters to a local directory.
#'
#' @author Nathan C. Layman
#'
#' @param urls A vector of URLs from which the raster data will be downloaded.
#' @param output_dir The directory where the processed rasters will be saved.
#' @param output_filename The filename to be assigned to the output rasters.
#' @param continent_raster_template A raster template of the target continent.
#' @param aggregate_method The method to be used for raster aggregation (Optional).
#' @param resample_method The method to be used for raster resampling (Optional).
#' @param overwrite A boolean flag indicating whether existing processed files should be overwritten. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A string containing the filepath to the saved processed rasters.
#'
#' @note This function requires the terra, arrow, and here packages among others.
#'
#' @examples
#' test_urls <- c("http://example.com/test1.tif", "http://example.com/test2.tif")
#' get_remote_rasters(urls = test_urls,
#'                    output_dir = '/path/to/output_dir',
#'                    output_filename = 'test.tif',
#'                    continent_raster_template = continent_template,
#'                    aggregate_method = "mean",
#'                    resample_method = "bilinear",
#'                    overwrite = TRUE)
#'
#' @export
get_remote_rasters <- function(urls, 
                               output_dir, 
                               output_filename, 
                               continent_raster_template,
                               aggregate_method = NULL,
                               resample_method = NULL,
                               factorize = TRUE,
                               overwrite = FALSE,
                               ...) {
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Unwrap terra raster
  continent_raster_template <- terra::unwrap(continent_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  save_filename <- file.path(output_dir, output_filename)
  
  # Check if soil files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message(glue::glue("{output_filename} already exists and can be loaded, skipping download and processing"))
    return(save_filename)
  }
  
  # Test if parquet or not
  if(!grepl("(tif|tiff|nc|asc|parquet|pq|arrow)", tools::file_ext(output_filename))) stop("output filename is not .tif or .parquet")
  
  # Create an empty SpatRaster to accumulate data
  combined_raster <- NULL
  combined_file <- paste0(output_dir, "/", output_filename)
  
  # Get terra friendly extension
  if(!grepl("(tif|tiff|nc|asc)", tools::file_ext(output_filename))) combined_file <- paste0(output_dir, "/", tools::file_path_sans_ext(output_filename), ".tif")
  
  # Start fresh
  unlink(here::here(output_dir, output_filename))
  unlink(combined_file)
  
  # Cycle through all the urls and process each one
  for(i in 1:length(urls)) {
    
    # Download the .rar file to a temporary location
    rar_file <- paste0(output_dir, "/", names(urls)[i], ".rar")
    download.file(url = urls[i][[1]], rar_file, mode = "wb")
    
    # List the contents of the .rar archive
    archive_contents <- archive::archive(rar_file)
    
    # Find the first raster file within the archive
    raster_file <- paste(output_dir, archive_contents$path[grep("\\.(tif|tiff|asc|grd|nc)$", archive_contents$path)][1], sep = "/")

    # Ensure a raster file was found. If not warn and skip.
    if (length(raster_file) == 0) {
      warning(paste("No raster file found in archive:", rar_url))
      next
    }
    
    # Extract the raster file to a temporary directory
    system2("unrar", c("e", "-o+", rar_file, here::here(output_dir), ">/dev/null"))    
    
    # Load the raster data
    unpacked_raster <- terra::rast(raster_file)
  
    # Reproject and crop raster to template
    unpacked_raster <- terra::project(unpacked_raster, continent_raster_template)
    unpacked_raster <- terra::crop(unpacked_raster, continent_raster_template)
    
    # Set the raster layer name to the name associated with the url
    names(unpacked_raster) <- names(urls)[i]
    
    # Write the current raster data to the output file
    if (is.null(combined_raster)) {
      # For the first file, initialize the output file
      terra::writeRaster(unpacked_raster, filename = combined_file)
    } else {
      # For subsequent files, append the data
      terra::writeRaster(unpacked_raster, filename = combined_file, gdal="APPEND_SUBDATASET=YES")
    }
    
    # Remove the temporary files
    unlink(raster_file)
    unlink(rar_file)
    
    # Update combined_raster with the pointer to the output file
    combined_raster <- terra::rast(combined_file)
  }
  
  # Pre-process raster prior to normalization  
  # For example aggregate_method="which.max" identifies the layer with the highest value for each pixel
  if(!is.null(aggregate_method)) combined_raster <- terra::app(combined_raster, fun = aggregate_method, na.rm = TRUE) |> setNames(tools::file_path_sans_ext(output_filename))
  
  # Re-sample raster to match template
  # Can change behavior with 'method' argument.
  # 'Mode' is most common value within cell. 
  # The default is bilinear interpolation for continuous data
  if(is.null(resample_method)) {
    combined_raster <- combined_raster |> terra::project(continent_raster_template)  
  } else {
    combined_raster <- combined_raster |> terra::project(continent_raster_template, method = resample_method)  
  }
  
  # Save as parquet if appropriate
  if(grepl("(parquet|pq|arrow)", tools::file_ext(output_filename))) {
    
    # Convert to dataframe
    dat <- as.data.frame(combined_raster, xy = TRUE) |> as_tibble()
    
    if(factorize) dat <- dat |> mutate(across(-c(x,y), ~as.factor(.x)))
    
    # Save as parquet 
    arrow::write_parquet(dat, here::here(output_dir, output_filename), compression = "gzip", compression_level = 5)
    
  } else {
    terra::writeRaster(combined_raster, filename = combined_file, overwrite = TRUE)
  }
    
  # Return path to saved file
  return(save_filename)
}
