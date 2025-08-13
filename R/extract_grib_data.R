#' Extract And Process Data from Grib Files
#'
#' This function is used to extract and process data from gridded binary (grib) files,
#' It applies various transformations and returns the result as a dataframe.
#'
#' @author Nathan C. Layman
#'
#' @param grib_file A character string specifying the path to the grib file.
#' @param template An optional template to be used for resampling.
#' @param ... Additional optional arguments.
#'
#' @return A dataframe containing extracted and processed data from the grib file.
#'
#' @note The raw grib data is transformed using terra package functions. If a template is provided, 
#' the data is resampled using this template.
#'
#' @examples
#' ## Not run: 
#' # Assuming 'sample.grib' is a valid grib file present in the working directory, 
#' # and 'template' is an R object of class terra::SpatRaster.
#' extract_grib_data('sample.grib', template)
#' ## End(Not run)
#'
#' @export
extract_grib_data <- function(grib_file, template = NULL, ...) {
  
  message(glue::glue("processing: {grib_file}"))
    
  grib <- terra::rast(unlist(grib_file))
  
  meta <- get_grib_metadata(grib_file) |> 
    mutate(base_date = as.Date(lubridate::as_datetime(as.numeric(GRIB_REF_TIME))),
           lead_days = as.numeric(GRIB_FORECAST_SECONDS) / (60 * 60 * 24),
           forecast_end_date = as.Date(lubridate::as_datetime(as.numeric(GRIB_VALID_TIME))),
           var = GRIB_COMMENT,
           var_id = GRIB_ELEMENT,
           units = gsub("[^[:alnum:]]", "", GRIB_UNIT)) |>
    select(base_date, lead_days, forecast_end_date, units, var, var_id) |> 
    mutate(lead_months = lubridate::interval(base_date, forecast_end_date) %/% months(1)) |>
    group_by(base_date, lead_months, var) |>
    mutate(group_id = cur_group_id()) |>
    ungroup()
  
  grib_mean <- terra::tapp(grib, meta$group_id, "mean")
  grib_sd <- terra::tapp(grib, meta$group_id, "sd")
  
  meta <- meta |> select(-group_id) |> distinct()
  
  assertthat::are_equal(nrow(meta), dim(grib_mean)[3])
  assertthat::are_equal(nrow(meta), dim(grib_sd)[3])

  # Reshape to XY long form with layer metadata
  grib_data <- map_dfr(1:nrow(meta), function(i) {
     
    long_mean <- transform_raster(grib_mean[[i]], template) |>
      setNames("mean") |>
      terra::as.data.frame(xy = T)
    
    long_sd <- transform_raster(grib_sd[[i]], template) |>
      setNames("sd") |>
      terra::as.data.frame(xy = F)
    
    bind_cols(long_mean, long_sd, meta[i,])
  })
  
  grib_data
}
