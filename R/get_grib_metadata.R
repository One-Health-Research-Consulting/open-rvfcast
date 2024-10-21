#' Get and parse gdalinfo metadata from a grib file without relying on grib_ls
#'
#' @author Nathan Layman
#'
#' @param raw_file 
#'
#' @return
#' @export
#'
#' @examples
get_grib_metadata <- function(raw_file) {
  
  # options = "json" works in targets but not during live testing
  # I have no idea why I can't get it to work in the console.
  # It's a path problem linking terra to an old version of gdal.
  # Since options="json" only works for newer gdal go with the 
  # conservative option. Even though it would be super nice!
  # gdalinfo_text <- terra::describe(raw_file, options = "json") 
  
  gdalinfo_text <- terra::describe(raw_file) 
  
  # Remove all text up to first BAND ^GEOGCRS
  metadata_start_index <- grep("^Band|^BAND", gdalinfo_text)[1]
  metadata_text <- gdalinfo_text[metadata_start_index:length(gdalinfo_text)] 
  metadata_text <- metadata_text[!str_detect(metadata_text, "^Band|BAND|Metadata|METADATA")]
  
  metadata <- map_dfr(metadata_text, ~stringr::str_split(.x[1], "=")[[1]] |> 
                             stringr::str_squish() |> setNames(c("name", "value"))) |>
    mutate(band = cumsum(name == "Description")) |> 
    pivot_wider(names_from = "name", values_from = "value")
  
  metadata
}
