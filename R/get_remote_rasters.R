#' Function to process a list of urls pointint to rasters. Rasters must be transmitted as zip or rar files
#' @param urls 
#' @param output_dir 
#' @param output_filename 
#' @param raster_template 
#' @param aggregate_method 
#' @param resample_method 
get_remote_rasters <- function(urls, 
                               output_dir, 
                               output_filename, 
                               raster_template,
                               aggregate_method = NULL,
                               resample_method = NULL) {
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Unwrap terra raster
  raster_template <- terra::unwrap(raster_template)
  
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
    archive_contents <- archive(rar_file)
    
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
    unpacked_raster <- terra::project(unpacked_raster, raster_template)
    unpacked_raster <- crop(unpacked_raster, raster_template)
    
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
  if(!is.null(aggregate_method)) combined_raster <- app(combined_raster, fun = aggregate_method, na.rm = TRUE)
  
  # Re-sample raster to match template
  # Can change behavior with 'method' argument.
  # 'Mode' is most common value within cell. 
  # The default is bilinear interpolation for continuous data
  if(is.null(resample_method)) {
    combined_raster <- terra::resample(combined_raster, raster_template)  
  } else {
    combined_raster <- terra::resample(combined_raster, raster_template, method = resample_method)  
  }
  
  # Save as parquet if appropriate
  if(grepl("(parquet|pq|arrow)", tools::file_ext(output_filename))) {
    
    # Convert to dataframe
    dat <- as.data.frame(combined_raster, xy = TRUE) |> as_tibble()
    
    # Save as parquet 
    arrow::write_parquet(dat, here::here(output_dir, output_filename), compression = "gzip", compression_level = 5)
    
  } else {
    terra::writeRaster(combined_raster, filename = combined_file, overwrite = TRUE)
  }
    
  # Return path to saved file
  return(paste(output_dir, output_filename, sep = "/"))
}
