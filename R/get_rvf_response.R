#' Retrieve and process RVF response data
#'
#' 'get_rvf_response' function downloads, transforms and saves the Rift Valley Fever (RVF) response data,
#' as an optimized Parquet file in the specified directory. If a file already exists at the target filepath,
#' it is used.
#'
#' @author Nathan C. Layman, Morgan Kain
#'
#' @param wahis_outbreaks Outbreak data to be processed.
#' @param wahis_raster_template Template to be used for raster operations.
#' @param forecast_intervals Intervals for which forecasts are to be made.
#' @param dates_to_process Dates for which predictions are to be made.
#' @param local_folder Local folder where the processed files will be saved. This directory is created if it doesn't exist. Default is 'data/rvf_response'.
#' @param save_filename Desired filename for the processed file. Default is 'rvf_response.parquet'.
#'
#' @return A string containing the filepath to the processed file.
#'
#' @note This function handles data downloading, processing and saving. If a file already exists at the target
#' filepath, it is used and not overwritten.
#'
#' @examples
#' get_rvf_response(wahis_outbreaks,
#'                  wahis_raster_template,
#'                  forecast_intervals,
#'                  dates_to_process,
#'                  local_folder = "data/rvf_response",
#'                  save_filename = "rvf_response.parquet")
#'
#' @export
get_rvf_response <- function(wahis_outbreaks,
                             wahis_raster_template,
                             forecast_intervals,
                             dates_to_process,
                             local_folder = "data/rvf_response",
                             save_filename = "rvf_response",
                             reduced,
                             overwrite = FALSE) {

  save_filename <- paste0(local_folder, "/", save_filename, "_", reduced, ".parquet")
  
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)
  existing_dataset        <- error_safe_read_parquet(save_filename)

  if (!is.null(existing_dataset) && !overwrite) return(save_filename)

  if (!reduced) {
  
  ## Unwrap packed template raster
  wahis_raster_template <- terra::rast(wahis_raster_template)

  ## Convert outbreak locations to a terra vector
  pts <- terra::vect(cbind(wahis_outbreaks$longitude, wahis_outbreaks$latitude), crs = crs(wahis_raster_template))

  # Get cell indices for points
  cell_indices <- cellFromXY(wahis_raster_template, cbind(wahis_outbreaks$longitude, wahis_outbreaks$latitude))

  ## Convert cell indices to standardized lat-lon coordinates based on the template raster cell grid
   ## This will allow the outbreaks to be joined to the other data based on lat / long.
  pt_coords <- xyFromCell(wahis_raster_template, cell_indices) |> as_tibble() |> setNames(c("x", "y"))

  ## Add cell x,y coords to outbreaks tibble.
  wahis_outbreaks_gridded <- wahis_outbreaks |> bind_cols(pt_coords)
  
  } else {
    wahis_outbreaks_gridded <- wahis_outbreaks
  }

  ## For every date in the range, sum cases across every interval
  ## This is an important issue. What exactly are we predicting? Probability
  ## of an outbreak _occurring_ within a forecast window? Or of an outbreak
  ## _starting_ within the forecast window? Going with starting. Much easier.
  rvf_respone <- map_dfr(dates_to_process, function(model_date) {

    map2_dfr(head(forecast_intervals, -1), tail(forecast_intervals, -1), function(interval_start, interval_end) {

      ## Not inclusive exclusive handling of range
      outbreaks <- wahis_outbreaks_gridded |>
        filter(
          start_date >= lubridate::as_datetime(model_date) + days(interval_start)
        , start_date < lubridate::as_datetime(model_date) + days(interval_end)
        )

      if (nrow(outbreaks) > 0) {
        
        if (!reduced) {
        
        outbreaks <- outbreaks |>
          group_by(x, y) |>
          summarize(
            date              = model_date
          , forecast_interval = interval_end
          , forecast_start    = lubridate::as_datetime(model_date) + days(interval_start)
          , forecast_end      = lubridate::as_datetime(model_date) + days(interval_end)
          , cases             = sum(cases, na.rm = TRUE)
          , .groups           = "drop")
        
        } else {
          
        outbreaks <- outbreaks |>
          group_by(h3_id) |>
          summarize(
            date              = model_date
          , forecast_interval = interval_end
          , forecast_start    = lubridate::as_datetime(model_date) + days(interval_start)
          , forecast_end      = lubridate::as_datetime(model_date) + days(interval_end)
          , cases             = sum(cases, na.rm = TRUE)
          , .groups           = "drop")
          
          
        }
        
      }
      
    })

  })

  arrow::write_parquet(rvf_respone, save_filename, compression = "gzip", compression_level = 5)

  save_filename
  
}


## And function to clean the raw version of the data, the output of which is
 ## used in the above function
clean_rvf_outbreaks <- function(output_path, map_dat, reduced) {
  
  ## Event       = Country
  ## report_id   = Collection of outbreaks
  ## outbreak_id = Individual outbreak --> can show up in many report_id
  
  wahis_outbreaks <- readRDS(output_path)
  wahis_outbreaks <- wahis_outbreaks[[2]] |> left_join(wahis_outbreaks[[1]]) 
  
  wahis_outbreaks_processed <- preprocess_wahis_rvf_outbreaks(
    wahis_rvf_outbreaks_raw = wahis_outbreaks
  , country_code_id_col     = "country_iso3c")
  
  ## Clean up, and extract the start date for each outbreak given by the lat, lon pair
  wahis_outbreaks_processed.s <- wahis_outbreaks_processed |> 
    mutate(
      start_date = coalesce(outbreak_start_date, outbreak_end_date)
    , end_date   = coalesce(outbreak_end_date, outbreak_start_date)
    ) |>
    ## Only using these species
    filter(grepl("sheep|cattle|camelidae|goat", species)) |>
    mutate(
      start_date = as_date(start_date)
    , end_date   = as_date(end_date)) |>
    dplyr::select(-c(outbreak_start_date, outbreak_end_date)) |>
    relocate(start_date, end_date, .after = outbreak_nat_ref) |>
    group_by(cases, start_date, latitude, longitude) |>
    ## Select only the relevant columns (full data is saved in the RDS for those interested)
    dplyr::select(report_id, outbreak_id, cases, start_date, end_date, latitude, longitude) |>
    ## First drop true repeats for which there are a few
    distinct() |>
    ## Then for each outbreak_id select the largest numbered report_id
    ## which comes more recent in time and has the most correct information
    ## about end_date
    group_by(outbreak_id) |>
    filter(report_id == max(report_id)) |>
    ungroup() |>
    dplyr::select(-c(report_id, outbreak_id)) |>
    mutate(outbreak_id = seq_len(n())) |>
    arrange(start_date, end_date)
  
  if (reduced) {
    
  ## Join to the hexes to get a general "region" for outbreaks and then sort by 
   ## end date to join "related" "outbreaks" into "Outbreaks"
  wahis_outbreaks_processed.s |>
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(map_dat[[1]]), remove = FALSE) |>
    st_join(map_dat[[1]], left = FALSE) |>
    group_by(shapeName, end_date) |>
    summarize(start_date = min(start_date), cases = sum(cases)) |>
    rename(h3_id = shapeName) |>
    as.data.frame() |>
    dplyr::select(-geometry) |>
    as_tibble()
    
  } else {
    
    wahis_outbreaks_processed.s
    
  }
  
}


## And another internal helper
preprocess_wahis_rvf_outbreaks <- function(wahis_rvf_outbreaks_raw, country_code_id_col) {
  
  wahis_rvf_outbreaks_raw$continent <- countrycode::countrycode(
    wahis_rvf_outbreaks_raw |> pull(get(country_code_id_col))
    , origin = "iso3c", destination = "continent")
  
  wahis_rvf_outbreaks <- wahis_rvf_outbreaks_raw |> 
    filter(continent == "Africa")  |> 
    mutate(iso_code = toupper(get(country_code_id_col))) |> 
    select(-!!country_code_id_col)
  
  return(wahis_rvf_outbreaks)
  
}


