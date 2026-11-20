#' Download and Transform Global NDVI Data
#'
#' This function download global NDVI data using given API parameters, performs
#' raster transformation with given raster template and then saves the transformed data
#' on specified location.
#'
#' @author Nathan Layman, Emma Mendelsohn
#'
#' @param sentinel_ndvi_api_parameters Parameters for the Sentinel NDVI API
#' @param continent_raster_template A raster template for the given continent
#' @param sentinel_ndvi_transformed_directory The directory where the transformed sentinel NDVI data will be stored
#' @param overwrite A boolean flag indicating whether to overwrite the existing file if already present. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return Returns the file path of the saved transformed sentinel NDVI data.
#'
#' @note This function creates a new directory if not already exists, downloads Sentinel NDVI data using provided API parameters,
#' makes necessary transformations on the data and then saves it. If overwrite is set to FALSE and file already exist
#' on the target location, then existing file path is returned.
#'
#' @examples
#' transform_sentinel_ndvi(sentinel_ndvi_api_parameters = list(properties = list(title = "some_title")),
#'                         continent_raster_template = "some_template",
#'                         sentinel_ndvi_transformed_directory = "./data",
#'                         overwrite = TRUE)
#'
#' @export
transform_sentinel_ndvi <- function(sentinel_ndvi_api_parameters,
                                    continent_raster_template,
                                    sentinel_ndvi_transformed_directory,
                                    sentinel_ndvi_token_file = "sentinel.token",
                                    basename_template = "transformed_sentinel_NDVI_{start_date}_to_{end_date}.parquet",
                                    overwrite = FALSE,
                                    ...) {

  # Create directory if it does not yet exist
  dir.create(sentinel_ndvi_transformed_directory, recursive = TRUE, showWarnings = FALSE)

  # NA sentinel row means all parquets already existed when get_sentinel_ndvi_api_parameters
  # ran; return an existing file to satisfy format = "file" without downloading anything.
  if (is.na(sentinel_ndvi_api_parameters$id)) {
    existing <- list.files(sentinel_ndvi_transformed_directory, pattern = "\\.parquet$", full.names = TRUE)
    return(existing[1])
  }

  template <- terra::unwrap(continent_raster_template)

  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

  raw_filename <- file.path(sentinel_ndvi_transformed_directory, sentinel_ndvi_api_parameters$name)

  # Extract start and end dates from the raw downloaded file name
  # naming conventions
  # https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-3-synergy/naming-conventions
  # Darned sentinel data has INCLUSIVE startDate and completionDate. Collection finishes at ~noon UTC.
  # Solve by shifting end date back one day. This slightly changes range
  # Do this in sentinel_ndvi_api_parameters step
  start_date <- sentinel_ndvi_api_parameters$start_date
  end_date <-  sentinel_ndvi_api_parameters$end_date

  sentinel_ndvi_filename <- file.path(sentinel_ndvi_transformed_directory, glue::glue(basename_template))

  # Check if glw files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(sentinel_ndvi_filename)) & !overwrite) {
    message(glue::glue("{basename(sentinel_ndvi_filename)} already exists, has rows, and overwrite is not TRUE, skipping"))
    return(sentinel_ndvi_filename)
  }

  # Download raw data
  sentinel_ndvi_downloaded <- download_sentinel_ndvi(sentinel_ndvi_api_parameters, raw_filename, sentinel_ndvi_token_file)

  message(paste0("Transforming ", raw_filename))

  # Re-project to raster to template
  transformed_raster <- transform_raster(raw_raster = read_sentinel_ndvi_nc(sentinel_ndvi_downloaded),
                                         template = terra::rast(continent_raster_template))

  # Convert raster to dataframe
  # Sentinel data is weekly so also expand out so every day has a value
  dat_out <- as.data.frame(transformed_raster, xy = TRUE) |>
    as_tibble() |>
    rename(ndvi = NDVI) |>
    mutate(start_date = start_date,
           end_date = end_date,
           days_count = as.integer(end_date - start_date) + 1) |>
    uncount(days_count, .id = "step") |> # This is a pretty cool trick. Very fast.
    mutate(date = start_date + step - 1,
           doy = as.integer(lubridate::yday(date)),
           month =  as.integer(lubridate::month(date)),
           year =  as.integer(lubridate::year(date)),
           source = "sentinel") |>
    select(-start_date, -end_date, -step)

  # Save as parquet
  arrow::write_parquet(dat_out, sentinel_ndvi_filename, compression = "gzip", compression_level = 5)

  # Clean up download file
  unlink(sentinel_ndvi_downloaded)

  return(sentinel_ndvi_filename)
}

#' Read a Sentinel-3 SY_2_V10 NDVI.nc as True NDVI
#'
#' NDVI is stored as packed bytes (NDVI = DN * scale_factor + add_offset; 2019 onward
#' 0.004 and -0.08). Whether terra/GDAL applies that packing on read has changed between
#' versions, which left the stored parquets on three different scales (raw DN 2019-2025,
#' NDVI x 200 for 2025-09 to 2026-07, true NDVI after). Reading the raw integers and
#' applying the file's own attributes gives true NDVI regardless of version.
#'
#' @param nc_file Character. Path to the product's NDVI.nc.
#'
#' @return SpatRaster of NDVI on the -1 to 1 scale, fill values as NA.
read_sentinel_ndvi_nc <- function(nc_file) {

  # Stored integers with the fill value already set to NA and no packing applied
  raw <- terra::rast(nc_file, raw = TRUE)

  # Packing attributes straight from the NetCDF metadata
  meta   <- terra::describe(nc_file)
  scale  <- as.numeric(sub(".*=", "", grep("NDVI#scale_factor=", meta, value = TRUE)))
  offset <- as.numeric(sub(".*=", "", grep("NDVI#add_offset=", meta, value = TRUE)))
  if (length(scale) != 1 || length(offset) != 1) {
    stop(glue::glue("Could not read NDVI scale_factor/add_offset from {basename(nc_file)}"))
  }

  ndvi <- raw * scale + offset
  names(ndvi) <- "NDVI"

  # Guard against a future change in packing silently producing values off the NDVI scale
  rng <- unlist(terra::global(ndvi, c("min", "max"), na.rm = TRUE))
  if (rng[1] < -1.05 || rng[2] > 1.05) {
    stop(glue::glue("Decoded NDVI in {basename(nc_file)} spans {round(rng[1], 3)} to {round(rng[2], 3)}, outside -1 to 1"))
  }

  ndvi

}
