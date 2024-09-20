#' Get and parse gdalinfo metadata from a grib file without relying on grib_ls
#'
#' @author Nathan Layman
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
get_grib_metadata <- function(raw_file) {
  
  gdalinfo_text <- terra::describe(raw_file, options = "json")
  
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
