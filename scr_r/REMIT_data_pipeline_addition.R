####
## NOTE: to get data for REMIT (either the "hotspots" model or the mosquito models
## for RSA or TAZ) add this section to the bottom of predictor_data_processing_targets.R
####

## Some notes about this: ------------------------------------------------------------
 ## 1) This will *sort of* work as is, however, you will need to be careful with things
 ## like weather_anomalies which take on dates only associated with what the main pipeline
 ## needs, while pulling together any of the REMIT data requires the full stack of files
  ## *** THUS, you may need to build the temporal manually and do the final join manually 

REMIT_targets <- tar_plan(
  
  tar_target(REMIT_weather_vars, c(
    ## Theoretically can obtain these as well, but have to use nasapower::get_power and for that
    ## have to provide small regions of the map at a time. So if we want these two variables would
    ## have to jump through quite a few hoops which may not be worth it as these covariates have similar
    ## analogs elsewhere in the dataset
    "evapotrans"       = "EVPTRNS"
    , "soil_moisture"    = "GWETPROF"
    , "precipitation"    = "PRECTOTCORR"
    , "spec_humid_2m"    = "QV2M"
    , "rel_humid_2m"     = "RH2M"
    , "air_temp_avg"     = "T2M"
    , "air_temp_max"     = "T2M_MAX"
    , "air_temp_min"     = "T2M_MIN"
    , "air_temp_range"   = "T2M_RANGE"
    , "dewpoint"         = "T2MDEW"
    , "surface_temp_avg" = "TS"
    , "wind_speed_10m"   = "WS10M"
    , "wind_speed_2m"    = "WS2M"
  ))
  
  , tar_target(REMIT_weather_vars_N, c(
    "evapo_land"       = "EVLAND"
    , "evapotrans"       = "EVPTRNS"
    , "soil_moisture"    = "GWETPROF"
    , "precipitation"    = "PRECTOTCORR"
    , "spec_humid_2m"    = "QV2M"
    , "rel_humid_2m"     = "RH2M"
    , "air_temp_avg"     = "T2M"
    , "air_temp_max"     = "T2M_MAX"
    , "air_temp_min"     = "T2M_MIN"
  ))
  
  , tar_target(locs_1_N, read.csv("data/mosquito_loc_data/TAZ_locations.csv"))
  , tar_target(locs_2_N, read.csv("data/mosquito_loc_data/RSA_locations.csv"))
  
  , tar_target(continent_raster_template_1_N,
               create_location_raster_template(continent_raster_template, locs_1_N, buffer_km = 30))
  
  , tar_target(continent_raster_template_2_N,
               create_location_raster_template(continent_raster_template, locs_2_N, buffer_km = 30))
  
  ## Specific months for N
  , tar_target(months_to_process_1_N, seq(from = as.Date("2008-01-01"), to = as.Date("2013-07-31"), by = "month") |> format("%Y-%m"))
  , tar_target(months_to_process_2_N, seq(from = as.Date("2020-10-01"), to = as.Date("2023-06-30"), by = "month") |> format("%Y-%m"))
  
  , tar_target(nasa_weather_transformed_REMIT_1_N, fetch_and_transform_nasa_weather(
    months_to_process_1_N
    , REMIT_weather_vars_N
    , continent_raster_template_1_N
    , local_folder = "data/nasa_weather_transformed_REMIT_1_N"
    , basename_template = "nasa_weather_transformed_{months_to_process}.parquet"
    , endpoint = "https://power-datastore.s3.amazonaws.com/v10/daily/{year}/{month}/power_10_daily_{yyyymmdd}_merra2_lst.nc"
    , overwrite = TRUE
    , NULL)
    , pattern = map(months_to_process_1_N)
    , error = "null"
    , format = "file")
  
  , tar_target(nasa_weather_transformed_REMIT_2_N, fetch_and_transform_nasa_weather(
    months_to_process_2_N
    , REMIT_weather_vars_N
    , continent_raster_template_2_N
    , local_folder = "data/nasa_weather_transformed_REMIT_2_N"
    , basename_template = "nasa_weather_transformed_{months_to_process}.parquet"
    , endpoint = "https://power-datastore.s3.amazonaws.com/v10/daily/{year}/{month}/power_10_daily_{yyyymmdd}_merra2_lst.nc"
    , overwrite = TRUE
    , NULL)
    , pattern = map(months_to_process_2_N)
    , error = "null"
    , format = "file")
  
  , tar_target(nasa_weather_summarized_REMIT_1_N, summarize_REMIT_weather_data(
      dat          = nasa_weather_transformed_REMIT_1_N
    , weather_vars = REMIT_weather_vars_N
    , yrs          = seq(2008, 2013)
    , path_to_out  = "data/nasa_weather_summarized_REMIT_1_N")
    , pattern      = map(REMIT_weather_vars_N)
    , error        = "null"
    , format       = "file")
  
  , tar_target(nasa_weather_summarized_REMIT_2_N, summarize_REMIT_weather_data(
      dat          = nasa_weather_transformed_REMIT_2_N
    , weather_vars = REMIT_weather_vars_N
    , yrs          = seq(2020, 2023)
    , path_to_out  = "data/nasa_weather_summarized_REMIT_2_N")
    , pattern      = map(REMIT_weather_vars_N)
    , error        = "null"
    , format       = "file")
  
  , tar_target(nasa_weather_REMIT_combined_1_N, combine_REMIT_weather_data(
    nasa_weather_summarized_REMIT_1_N
    , "data/nasa_weather_combined_REMIT_1_N"))
  
  , tar_target(nasa_weather_REMIT_combined_2_N, combine_REMIT_weather_data(
    nasa_weather_summarized_REMIT_2_N
    , "data/nasa_weather_combined_REMIT_2_N"))
  
  , tar_target(nasa_weather_REMIT_cleaned_1_N, impute_REMIT_weather_data(
    nasa_weather_REMIT_combined_1_N
    , "data/nasa_weather_combined_REMIT_1_N"))
  
  , tar_target(nasa_weather_REMIT_cleaned_2_N, impute_REMIT_weather_data(
    nasa_weather_REMIT_combined_2_N
    , "data/nasa_weather_combined_REMIT_2_N"))
  
  , tar_target(africa_full_predictor_data_sources_REMIT_1_N, list(
    nasa_weather_REMIT_cleaned = nasa_weather_REMIT_cleaned_1_N
    , bioclim_preprocessed = bioclim_preprocessed
    , soil_preprocessed = soil_preprocessed
    , aspect_preprocessed = aspect_preprocessed
    , slope_preprocessed = slope_preprocessed
    , glw_preprocessed = glw_preprocessed
    , elevation_preprocessed = elevation_preprocessed
    , landcover_preprocessed = landcover_preprocessed))
  
  , tar_target(africa_full_predictor_data_sources_REMIT_2_N, list(
    nasa_weather_REMIT_cleaned = nasa_weather_REMIT_cleaned_2_N
    , bioclim_preprocessed = bioclim_preprocessed
    , soil_preprocessed = soil_preprocessed
    , aspect_preprocessed = aspect_preprocessed
    , slope_preprocessed = slope_preprocessed
    , glw_preprocessed = glw_preprocessed
    , elevation_preprocessed = elevation_preprocessed
    , landcover_preprocessed = landcover_preprocessed))
  
  , tar_target(africa_full_predictor_data_REMIT_1_N, {
    
    joined_df <- map(africa_full_predictor_data_sources_REMIT_1_N %>% unlist()
                     , .f = function(this_file) {
                       arrow::read_parquet(this_file) |> mutate(x = round(x, 5), y = round(y, 5))
                     })
    
    joined_df <- reduce(joined_df, left_join, by = c("x", "y"))
    
    joined_df <- joined_df[complete.cases(joined_df), ]
    
    static_summary_vars_path <- "data/africa_full_predictor_data_REMIT/TAZ_static.parquet"
    
    joined_df |> arrow::write_parquet(static_summary_vars_path, compression = "gzip", compression_level = 5)
    
    ## Temporal data nasa weather
    nasa <- map(nasa_weather_transformed_REMIT_1_N %>% unlist()
                     , .f = function(this_file) {
                       arrow::read_parquet(this_file) |> mutate(x = round(x, 5), y = round(y, 5))
                     })
    
    nasa_joined <- bind_rows(nasa)

    #nasa_vars_path <- "data/africa_full_predictor_data_REMIT/TAZ_nasa_weather.parquet"
    #nasa_joined |> arrow::write_parquet(nasa_vars_path, compression = "gzip", compression_level = 5)
    
    
    ## Temporal data weather anomalies
    anomaly_file_paths <- paste0("data/weather_anomalies/", list.files("data/weather_anomalies"))
    anomaly_file_paths <- anomaly_file_paths[
      grep("2008-01-01", anomaly_file_paths):grep("2013-07-31", anomaly_file_paths)
    ]
    
    nasa_template <- nasa_joined |> group_by(x, y) |> dplyr::slice(1) |> ungroup()

    anomaly_files <- map(anomaly_file_paths
                , .f = function(this_file) {
                  left_join(
                    nasa_template |> dplyr::select(x, y)
                  , arrow::read_parquet(this_file) |> mutate(x = round(x, 5), y = round(y, 5))
                  )
                })
    
    anomaly_files_joined <- anomaly_files |> bind_rows()
    
    #anomaly_vars_path <- "data/africa_full_predictor_data_REMIT/TAZ_weather_anomaly.parquet"
    
    temporal_vars <- nasa_joined |>
      mutate(x = round(x, 5), y = round(y, 5)) |>
      mutate(month = as.numeric(month)) |>
      left_join(
        anomaly_files_joined |> 
          mutate(x = round(x, 5), y = round(y, 5))
        )
    
    temporal_vars <- temporal_vars[complete.cases(temporal_vars), ]
    
    temporal_vars_path <- "data/africa_full_predictor_data_REMIT/TAZ_temporal.parquet"
    
    temporal_vars |> arrow::write_parquet(temporal_vars_path, compression = "gzip", compression_level = 5)
    
    return(
      c(static_summary_vars_path, temporal_vars_path)
    )
    
  })
  
  , tar_target(africa_full_predictor_data_REMIT_2_N, {
    
    joined_df <- map(africa_full_predictor_data_sources_REMIT_2_N %>% unlist()
                     , .f = function(this_file) {
                       arrow::read_parquet(this_file) |> mutate(x = round(x, 5), y = round(y, 5))
                     })
    
    joined_df <- reduce(joined_df, left_join, by = c("x", "y"))
    
    joined_df <- joined_df[complete.cases(joined_df), ]
    
    static_summary_vars_path <- "data/africa_full_predictor_data_REMIT/RSA_static.parquet"
    
    joined_df |> arrow::write_parquet(static_summary_vars_path, compression = "gzip", compression_level = 5)
    
    ## Temporal data nasa weather
    nasa <- map(nasa_weather_transformed_REMIT_2_N %>% unlist()
                , .f = function(this_file) {
                  arrow::read_parquet(this_file) |> mutate(x = round(x, 5), y = round(y, 5))
                })
    
    nasa_joined <- bind_rows(nasa)
    
    #nasa_vars_path <- "data/africa_full_predictor_data_REMIT/TAZ_nasa_weather.parquet"
    #nasa_joined |> arrow::write_parquet(nasa_vars_path, compression = "gzip", compression_level = 5)
    
    ## Temporal data weather anomalies
    anomaly_file_paths <- paste0("data/weather_anomalies/", list.files("data/weather_anomalies"))
    anomaly_file_paths <- anomaly_file_paths[
      grep("2020-10-01", anomaly_file_paths):grep("2023-06-30", anomaly_file_paths)
    ]
    
    nasa_template <- nasa_joined |> group_by(x, y) |> dplyr::slice(1) |> ungroup()
    
    anomaly_files <- map(anomaly_file_paths
                         , .f = function(this_file) {
                           left_join(
                             nasa_template |> dplyr::select(x, y)
                             , arrow::read_parquet(this_file) |> mutate(x = round(x, 5), y = round(y, 5))
                           )
                         })
    
    anomaly_files_joined <- anomaly_files |> bind_rows()
    
    temporal_vars <- nasa_joined |>
      mutate(x = round(x, 5), y = round(y, 5)) |>
      mutate(month = as.numeric(month)) |>
      left_join(
        anomaly_files_joined |> 
          mutate(x = round(x, 5), y = round(y, 5))
      )
    
    temporal_vars <- temporal_vars[complete.cases(temporal_vars), ]
    
    temporal_vars_path <- "data/africa_full_predictor_data_REMIT/RSA_temporal.parquet"
    
    temporal_vars |> arrow::write_parquet(temporal_vars_path, compression = "gzip", compression_level = 5)
    
    return(c(static_summary_vars_path, temporal_vars_path))
    
  })
  
)


#' Create a Subset Raster Template Bounded by Location Points
#'
#' Builds a new raster template (wrapped SpatRaster) with the same resolution as
#' continent_raster_template, cropped to the bounding box of the supplied locations
#' plus a metric buffer applied in all directions. Cell centers in the returned
#' raster are snapped to the nearest cell in continent_raster_template so that
#' x/y coordinates will join cleanly to data derived from the full template.
#'
#' @param continent_raster_template Wrapped SpatRaster. Full-continent template used
#'   as the source for resolution and CRS.
#' @param locs Data frame. Must contain columns "Latitude" and "Longitude" in decimal degrees.
#' @param buffer_km Numeric. Distance in km to expand the bounding box on each side. Default: 10.
#'
#' @return Wrapped SpatRaster whose cell centers are a strict subset of those in
#'   continent_raster_template.
#'
#' @export
create_location_raster_template <- function(continent_raster_template, locs, buffer_km = 10) {
  
  template <- terra::unwrap(continent_raster_template)
  
  ## Build sf points from location coordinates
  pts <- sf::st_as_sf(locs, coords = c("Longitude", "Latitude"), crs = 4326)
  
  ## Project to Cylindrical Equal-Area for accurate metric buffering across Africa
  cea      <- "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
  pts_proj <- sf::st_transform(pts, cea)
  
  ## Buffer all points by the requested distance and union to get combined footprint
  buffered <- sf::st_union(sf::st_buffer(pts_proj, dist = buffer_km * 1000))
  
  ## Reproject the combined bounding box back to WGS84
  bbox <- sf::st_bbox(sf::st_transform(buffered, 4326))
  
  buffer_ext <- terra::ext(
    as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"])
    , as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"])
  )
  
  ## Find which cells of the continent template intersect the buffer extent
  buffer_cells <- terra::cells(template, buffer_ext)
  
  rows <- terra::rowFromCell(template, buffer_cells)
  cols <- terra::colFromCell(template, buffer_cells)
  
  r_range <- range(rows)
  c_range <- range(cols)
  
  subset_template <- template[r_range[1]:r_range[2], c_range[1]:c_range[2], drop = FALSE]
  
  ## The row/col subset has the right cells but terra recomputes xmin/ymax through a
  ## different arithmetic path than the full template, so cell centers can differ by
  ## ~1e-14 degrees and break exact coordinate joins. Fix: find the nearest cell in the
  ## full template for each cell in the subset, retrieve the exact x/y that the full
  ## template produces for those cells, and rebuild the extent from those values.
  subset_centers <- terra::xyFromCell(subset_template, seq_len(terra::ncell(subset_template)))
  nearest_cells  <- terra::cellFromXY(template, subset_centers)
  exact_centers  <- terra::xyFromCell(template, nearest_cells)
  
  xres <- terra::xres(template)
  yres <- terra::yres(template)
  
  terra::ext(subset_template) <- terra::ext(
    min(exact_centers[, "x"]) - xres / 2
    , max(exact_centers[, "x"]) + xres / 2
    , min(exact_centers[, "y"]) - yres / 2
    , max(exact_centers[, "y"]) + yres / 2
  )
  
  #values(subset_template) <- runif(ncell(subset_template))
  #as.data.frame(subset_template, xy = TRUE)
  #plot(subset_template)
  #values(template) <- runif(ncell(template))
  #as.data.frame(subset_template, xy = TRUE) |> left_join(as.data.frame(template, xy = TRUE) |> rename(base = lyr.1))
  
  terra::wrap(subset_template)
  
}

