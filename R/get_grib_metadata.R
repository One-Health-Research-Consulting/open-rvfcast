#' Extract and Format GRIB Metadata without relying on grib_ls
#'
#' This function extracts and prepares the metadata from a GRIB file for further data analysis. It's crucial for any
#' data preprocessing involving GRIB files.
#'
#' @author Nathan C. Layman
#'
#' @param source A string indicating the location/path of the raw GRIB file. This file should exist before the function is called.
#'
#' @return Returns a wide form dataframe of the metadata. Each row represents a band's metadata with their respective details.
#'
#' @note The function uses the 'terra' package to describe the file, 'stringr' for string manipulation, and 'tidyverse' for efficient dataset processing.
#'
#' @examples
#' metadata_df <- get_grib_metadata(source = "./data/grib_file.grb")
#'
#' @export
get_grib_metadata <- function(grib_file) {
  
  # terra::describe(source, options = "json") only works for newer versions of
  # gdal so going with manual reshaping here. For newer gdal use:
  # gdalinfo_text <- terra::describe(source, options = "json") 
  
  gdalinfo_text <- terra::describe(grib_file) 
  
  # Remove all text up to first BAND ^GEOGCRS
  metadata_start_index <- grep("^Band|^BAND", gdalinfo_text)[1]
  metadata_text <- gdalinfo_text[metadata_start_index:length(gdalinfo_text)] 
  metadata_text <- metadata_text[!str_detect(metadata_text, "^Band|BAND|Metadata|METADATA")]
  
  metadata <- map_dfr(metadata_text, ~stringr::str_split(.x[1], "=")[[1]] |>
                             stringr::str_squish() |> setNames(c("name", "value"))) |>
    mutate(band = cumsum(name == "Description")) |>
    # values_fn = first guards against duplicate metadata keys within a band (observed
    # in some GRIB files where e.g. GRIB_REF_TIME appears more than once per layer),
    # which would otherwise produce list columns that propagate to arrow::write_parquet.
    pivot_wider(names_from = "name", values_from = "value", values_fn = first)
  
  metadata
}
