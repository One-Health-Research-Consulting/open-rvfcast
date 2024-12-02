# Re-record current dependencies for CAPSULE users
if(Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true"))
  capsule::capshot(c("packages.R",
                     list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE),
                     list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)))

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

aws_bucket = Sys.getenv("AWS_BUCKET_ID")

# Targets options
source("_targets_settings.R")

# Convenience function to format .env flags properly for overwrite parameter and target cues
parse_flag <- function(flag, cue = F) {
  flag <- as.logical(Sys.getenv(flag, unset = "FALSE"))
  if(cue) flag <- ifelse(flag, "always", "thorough")
  flag
}

# Every major data target returns a list of parquet file names. Those can then be 
# combined and opened using arrow::open_dataset which allows a lot of operations 
# to be performed on the data without loading it all into memory. See 
# augmented_data target for more details

# Data targets are integrated with but not dependent on AWS. The _AWS and
# _AWS_upload targets fetch and upload parquet files. Before trying
# to download and process the raw data from the primary sources, each data
# target will attempt to fetch the processed parquet file from an AWS S3 bucket.
# If the file can be successfully downloaded and opened with arrow it will move
# on to the next task unless the OVERWRITE_X_DATA environment flag is set to TRUE
# in the .env file. In that case, the target will always download and process 
# data directly from the primary source. The pipeline will still run even if
# the AWS targets fail.

# Static Data Download ----------------------------------------------------
# These data sources don't change with time. 
static_targets <- tar_plan(
  
  # Define country bounding boxes and years to set up download ----------------------------------------------------
  # TODO change from rnaturalearth to rgeoboundaries to get ADM2 districts
  tar_target(country_polygons, create_country_polygons(countries =  c("Libya", "Kenya", "South Africa",
                                                                      "Mauritania", "Niger", "Namibia",
                                                                      "Madagascar", "Eswatini", "Botswana" ,
                                                                      "Mali", "United Republic of Tanzania", 
                                                                      "Chad","Sudan", "Senegal",
                                                                      "Uganda", "South Sudan", "Burundi"),
                                                       states = tibble(state = "Mayotte", country = "France"))),
  tar_target(country_bounding_boxes, get_country_bounding_boxes(country_polygons)),
  
  tar_target(continent_polygon, create_africa_polygon()),
  tar_target(continent_raster_template, wrap(terra::rast(ext(continent_polygon), resolution = 0.1))), 
  
  # nasa power resolution = 0.5; 
  # ecmwf = 1; 
  # sentinel ndvi = 0.01
  # modis ndvi = 0.01
  tar_target(rsa_polygon, rgeoboundaries::geoboundaries("South Africa", "adm2")),
  
  # SOIL -----------------------------------------------------------
  tar_target(soil_directory, create_data_directory(directory_path = "data/soil_dataset")),
  
  # Check if preprocessed soil data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
  tar_target(soil_AWS, AWS_get_folder(soil_directory,
                                      continent_raster_template), # Enforce Dependency
             error = "null",
             cue = tar_cue("always")), # Continue the pipeline even on error
  
  tar_target(soil_preprocessed, preprocess_soil(soil_directory, 
                                                continent_raster_template, 
                                                overwrite = as.logical(Sys.getenv("OVERWRITE_STATIC_DATA", unset = "FALSE")),
                                                soil_AWS), # Enforce dependency
             format = "file",
             repository = "local"),
  
  tar_target(soil_preprocessed_AWS_upload, AWS_put_files(soil_preprocessed, 
                                                         soil_directory),
             error = "null"), # Continue the pipeline even on error
  
  # ASPECT -------------------------------------------------
  tar_target(aspect_urls, c("aspect_zero" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClN_30as.rar",
                            "aspect_fortyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClE_30as.rar", 
                            "aspect_onethirtyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClS_30as.rar",
                            "aspect_twotwentyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClW_30as.rar",
                            "aspect_undef" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClU_30as.rar")),
  
  tar_target(aspect_directory, create_data_directory(directory_path = "data/aspect_dataset")),
  
  # Check if preprocessed aspect data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
  tar_target(aspect_AWS, AWS_get_folder(aspect_directory,
                                        continent_raster_template),
             error = "null",
             cue = tar_cue("always")), # Enforce Dependency
  
  tar_target(aspect_preprocessed, get_remote_rasters(urls = aspect_urls, 
                                                     output_dir = aspect_directory,
                                                     output_filename = "aspect.parquet",
                                                     continent_raster_template,
                                                     aggregate_method = "which.max", # What is the dominant aspect for each point?
                                                     resample_method = "mode", # What is the dominant aspect at the scale of the template raster?
                                                     factorize = TRUE,
                                                     overwrite = as.logical(Sys.getenv("OVERWRITE_STATIC_DATA", unset = "FALSE")),
                                                     aspect_AWS), # Enforce dependency
             format = "file",
             repository = "local"),
  
  tar_target(aspect_preprocessed_AWS_upload, AWS_put_files(aspect_preprocessed,
                                                           aspect_directory),
             error = "null"), # Continue the pipeline even on error
  
  # SLOPE -------------------------------------------------
  tar_target(slope_urls, c("slope_zero" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl1_30as.rar",
                           "slope_pointfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl2_30as.rar",
                           "slope_two" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl3_30as.rar",
                           "slope_five" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl4_30as.rar",
                           "slope_ten" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl5_30as.rar",
                           "slope_fifteen" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl6_30as.rar",
                           "slope_thirty" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl7_30as.rar",
                           "slope_fortyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl8_30as.rar")),
  
  tar_target(slope_directory, create_data_directory(directory_path = "data/slope_dataset")),
  
  # Check if preprocessed slope data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
  tar_target(slope_AWS, AWS_get_folder(aspect_directory,
                                       continent_raster_template), # Enforce Dependency
             error = "null",
             cue = tar_cue("always")), # Continue the pipeline even on error
  
  tar_target(slope_preprocessed, get_remote_rasters(urls = slope_urls, 
                                                    output_dir = slope_directory,
                                                    output_filename = "slope.parquet",
                                                    continent_raster_template,
                                                    aggregate_method = "which.max", # What is the dominant slope for each point?
                                                    resample_method = "mode", # What is the dominant slope at the scale of the template raster?
                                                    factorize = TRUE,
                                                    overwrite = as.logical(Sys.getenv("OVERWRITE_STATIC_DATA", unset = "FALSE")),
                                                    slope_AWS), # Enforce dependency
             format = "file",
             repository = "local"),
  
  tar_target(slope_preprocessed_AWS_upload, AWS_put_files(slope_preprocessed,
                                                          slope_directory),
             error = "null"), # Continue the pipeline even on error
 
   # Gridded Livestock of the world -----------------------------------------------------------
  tar_target(glw_urls, c("glw_cattle" = "https://dataverse.harvard.edu/api/access/datafile/6769710", 
                         "glw_sheep" = "https://dataverse.harvard.edu/api/access/datafile/6769629",
                         "glw_goats" = "https://dataverse.harvard.edu/api/access/datafile/6769692")),
  
  tar_target(glw_directory, 
             create_data_directory(directory_path = "data/glw_dataset")),
  
  # Check if preprocessed glw data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
  tar_target(glw_AWS, AWS_get_folder(glw_directory,
                                     continent_raster_template), # Enforce Dependency
             error = "null",
             cue = tar_cue("always")), # Continue the pipeline even on error
  
  tar_target(glw_preprocessed, 
             preprocess_glw_data(glw_directory, 
                                 glw_urls,
                                 continent_raster_template,
                                 overwrite = as.logical(Sys.getenv("OVERWRITE_STATIC_DATA", unset = "FALSE")),
                                 glw_AWS),
             format = "file",
             repository = "local"), # Enforce dependency
  
  tar_target(glw_preprocessed_AWS_upload, AWS_put_files(glw_preprocessed,
                                                        glw_directory),
             error = "null"), # Continue the pipeline even on error

# ELEVATION -----------------------------------------------------------
tar_target(elevation_directory, 
           create_data_directory(directory_path = "data/elevation_dataset")),

# Check if preprocessed elevation data already exists on AWS and can be loaded.
# If so download from AWS instead of primary source
tar_target(elevation_AWS, AWS_get_folder(elevation_directory,
                                         continent_raster_template), # Enforce Dependency
           error = "null",
           cue = tar_cue("always")), # Continue the pipeline even on error

tar_target(elevation_preprocessed, 
           get_elevation_data(output_dir = elevation_directory, 
                              output_filename = "africa_elevation.parquet",
                              continent_raster_template,
                              overwrite = as.logical(Sys.getenv("OVERWRITE_STATIC_DATA", unset = "FALSE")),
                              elevation_AWS), # Enforce dependency
           format = "file",
           repository = "local"),

tar_target(elevation_preprocessed_AWS_upload, AWS_put_files(elevation_preprocessed,
                                                            elevation_directory),
           error = "null"), # Continue the pipeline even on error

# BIOCLIM -----------------------------------------------------------
tar_target(bioclim_directory, 
           create_data_directory(directory_path = "data/bioclim_dataset")),

# Check if preprocessed bioclim data already exists on AWS and can be loaded.
# If so download from AWS instead of primary source
tar_target(bioclim_AWS, AWS_get_folder(bioclim_directory,
                                         continent_raster_template), # Enforce Dependency
           error = "null",
           cue = tar_cue("always")), # Continue the pipeline even on error

tar_target(bioclim_preprocessed,
           get_bioclim_data(output_dir = bioclim_directory, 
                            output_filename = "bioclim.parquet",
                            continent_raster_template,
                            overwrite = as.logical(Sys.getenv("OVERWRITE_STATIC_DATA", unset = "FALSE"))),
           format = "file",
           repository = "local"),

tar_target(bioclim_preprocessed_AWS_upload, AWS_put_files(bioclim_preprocessed, 
                                                          bioclim_directory),
           error = "null"), # Continue the pipeline even on error

# LANDCOVER -----------------------------------------------------------
tar_target(landcover_types, c("trees", "grassland", "shrubs", "cropland", "built", "bare", "snow", "water", "wetland", "mangroves", "moss")),

tar_target(landcover_directory, 
           create_data_directory(directory_path = "data/landcover_dataset")),

# Check if preprocessed bioclim data already exists on AWS and can be loaded.
# If so download from AWS instead of primary source
tar_target(landcover_AWS, AWS_get_folder(landcover_directory,
                                         continent_raster_template), # Enforce Dependency
           error = "null",
           cue = tar_cue("always")), # Continue the pipeline even on error

tar_target(landcover_preprocessed,
           get_landcover_data(output_dir = landcover_directory, 
                              output_filename = "landcover.parquet",
                              landcover_types,
                              continent_raster_template,
                              overwrite = as.logical(Sys.getenv("OVERWRITE_STATIC_DATA", unset = "FALSE")),
                              landcover_AWS), # Enforce Dependency
           format = "file",
           repository = "local"),

tar_target(landcover_preprocessed_AWS_upload, AWS_put_files(landcover_preprocessed,
                                                            landcover_directory),
           error = "null"), # Continue the pipeline even on error
)


# Dynamic Data Download -----------------------------------------------------------
dynamic_targets <- tar_plan(
  
  # WAHIS -----------------------------------------------------------
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks_preprocessed, 
             preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw)),
  
  tar_target(wahis_rvf_controls_raw, get_wahis_rvf_controls_raw()),
  tar_target(wahis_rvf_controls_preprocessed, 
             preprocess_wahis_rvf_controls(wahis_rvf_controls_raw)),
  
  tar_target(wahis_outbreak_dates, tibble(date = seq(from = min(coalesce(wahis_rvf_outbreaks_preprocessed$outbreak_end_date, wahis_rvf_outbreaks_preprocessed$outbreak_start_date), na.rm = T),
                                                     to = max(coalesce(wahis_rvf_outbreaks_preprocessed$outbreak_end_date, wahis_rvf_outbreaks_preprocessed$outbreak_start_date), na.rm = T),
                                                     by = "day"),
                                          year = year(date),
                                          month = month(date)) |>
               group_by(month, year) |>
               tar_group(),
             iteration = "group"),
  
  tar_target(wahis_outbreaks, wahis_rvf_outbreaks_preprocessed |> 
               mutate(end_date = coalesce(outbreak_end_date, outbreak_start_date), na.rm = T) |>
               select(cases, end_date, latitude, longitude) |>
               distinct() |>
               arrange(end_date) |>
               mutate(outbreak_id = 1:n())),
  
  # OUTBREAK HISTORY -----------------------------------------------------------
  
  # NOTE: next time make these format = "file" and have create_data_directory return 
  # the path to the .gitkeep file. That way if it gets deleted the target automatically
  # re-runs
  tar_target(wahis_outbreak_history_directory, 
             create_data_directory(directory_path = "data/outbreak_history_dataset")),
  
  tar_target(wahis_raster_template, terra::rasterize(terra::vect(continent_polygon), # Take the boundary of Africa
                                                     terra::rast(continent_polygon, # Mask against a raster filled with 1's
                                                                 resolution = 0.1, # Set resolution
                                                                 vals = 1)) |>
               terra::wrap()), # Wrap to avoid problems with targets
  
  tar_target(wahis_distance_matrix, get_outbreak_distance_matrix(wahis_outbreaks, wahis_raster_template)),
  
  # Check if preprocessed wahis_outbreak_history data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
  tar_target(wahis_outbreak_history_AWS, AWS_get_folder(wahis_outbreak_history_directory,
                                                        wahis_outbreak_dates, # Enforce Dependency
                                                        wahis_outbreaks, # Enforce Dependency
                                                        wahis_distance_matrix, # Enforce Dependency
                                                        wahis_raster_template), # Enforce Dependency
             error = "null",
             cue = tar_cue("always")), # Continue the pipeline even on error
  
  # Dynamic branch over year batch over day otherwise too many branches.
  tar_target(wahis_outbreak_history, get_daily_outbreak_history(wahis_outbreak_dates,
                                                                wahis_outbreaks,
                                                                wahis_distance_matrix,
                                                                wahis_raster_template,
                                                                output_dir = wahis_outbreak_history_directory,
                                                                output_filename = "outbreak_history.parquet",
                                                                beta_time = 0.5,
                                                                max_years = 10,
                                                                recent = 3/12,
                                                                overwrite = as.logical(Sys.getenv("OVERWRITE_OUTBREAK_HISTORY", unset = "FALSE")),
                                                                wahis_outbreak_history_AWS), # Enforce Dependency
             pattern = map(wahis_outbreak_dates),
             error = "null", # Keep going if error. It will be caught next time the pipeline is run.
             format = "file", 
             repository = "local"),
  
  tar_target(wahis_outbreak_history_animations_directory, 
             create_data_directory(directory_path = "outputs/wahis_outbreak_history_animations")),
  
  tar_target(wahis_outbreak_history_AWS_upload, AWS_put_files(wahis_outbreak_history,
                                                              wahis_outbreak_history_animations_directory),
             error = "null"), # Continue the pipeline even on error
  
  # NCL: Note need to refactor to account for switching to wide format wahis_outbreak_history
  
  # # Check if preprocessed wahis_outbreak_history data already exists on AWS and can be loaded.
  # # If so download from AWS instead of primary source
  # tar_target(wahis_outbreak_history_animations_AWS, AWS_get_folder(wahis_outbreak_history_animations_directory,
  #                                                                  wahis_outbreak_history), # Enforce Dependency
  #            error = "null",
  #            cue = tar_cue("always")), # Continue the pipeline even on error
  # 
  # # Animate a SpatRaster stack where each layer is a date.
  # # gganimate took 20 minutes per file.
  # # just saving all the frames as separate pngs
  # # and combining with gifski took 50 minutes for all of them.
  # tar_target(wahis_outbreak_history_animations, get_outbreak_history_animation(wahis_outbreak_history,
  #                                                                              wahis_outbreak_history_animations_directory,
  #                                                                              overwrite = as.logical(Sys.getenv("OVERWRITE_OUTBREAK_HISTORY", unset = "FALSE"))), # Just included to enforce dependency with wahis_outbreak_history
  #            pattern = map(wahis_outbreak_history),
  #            error = "null",
  #            repository = "local"), 
  # 
  # tar_target(wahis_outbreak_history_animations_AWS_upload, AWS_put_files(wahis_outbreak_history_animations,
  #                                                                        wahis_outbreak_history_animations_directory),
  #            error = "null"), # Continue the pipeline even on error

  # SENTINEL NDVI -----------------------------------------------------------
  # 2018-present
  # 10 day period
  tar_target(sentinel_ndvi_transformed_directory, 
             create_data_directory(directory_path = "data/sentinel_ndvi_transformed")),
  
  tar_target(get_sentinel_ndvi_AWS, AWS_get_folder(sentinel_ndvi_transformed_directory),
             error = "null",
             cue = tar_cue("always")),
  
  # Should last 10 minutes. If it fails renew the token and try again.
  tar_target(sentinel_ndvi_token_file, get_sentinel_ndvi_token(), cue = tar_cue("always")),
  
  # get API parameters
  tar_target(sentinel_ndvi_api_parameters, get_sentinel_ndvi_api_parameters()), 
  
  # MAX SESSION = 4! Can't parallel this one due to API restrictions
  # Sentinel data is weekly so we also expand out so every day has a value
  # to make it easier to join in. This is a step function see modis NDVI for
  # more details
  tar_target(sentinel_ndvi_transformed, 
             transform_sentinel_ndvi(sentinel_ndvi_api_parameters, 
                                     continent_raster_template,
                                     sentinel_ndvi_transformed_directory,
                                     sentinel_ndvi_token_file,
                                     overwrite = as.logical(Sys.getenv("OVERWRITE_SENTINEL_NDVI", unset = "FALSE")),
                                     get_sentinel_ndvi_AWS),
             pattern = map(sentinel_ndvi_api_parameters),
             error = "null", # Keep going if error. It will be caught next time the pipeline is run.
             format = "file", 
             repository = "local"),
  
  tar_target(sentinel_ndvi_transformed_AWS_upload, AWS_put_files(sentinel_ndvi_transformed,
                                                                 sentinel_ndvi_transformed_directory),
             error = "null"), # Continue the pipeline even on error
  
  # MODIS NDVI -----------------------------------------------------------
  # 2005-present
  # this satellite will be retired soon, so we should use sentinel for present dates 
  # ~10 day period. Note the period of sentinel data does not match modis.
  # Some interpolation would be useful. Currently using step function.
  tar_target(modis_ndvi_transformed_directory, 
             create_data_directory(directory_path = "data/modis_ndvi_transformed")),
  
  # This target reads in an Appears token from the .env file and tests that it 
  # still works. It requests a new token and updates the .env file if not.
  tar_target(modis_ndvi_token, get_modis_ndvi_token(), cue = tar_cue("always")),
  
  # The last day of every years we want to request ndvi data.
  # Ordered by end date so that the current year will request new data ever new
  # day the pipeline is run.
  tar_target(modis_task_end_dates, c(seq(as.Date("2005-12-31"), Sys.Date(), by = "year"), Sys.Date()) |> unique()),
  
  # Set parameters and submit request for full continent
  # Bundle requests take quite a while to finish processing depending on the size.
  # Branching by year. This makes each task faster and lets us process new years without having
  # to re-do previous years. It also ensures that tasks are processed in the order submitted.
  # Set OVERWRITE_MODIS_NDVI to TRUE in the .env file to force re-download and processing of 
  # previous years. The current year will always re-run regardless of this setting.
  # If a year isn't complete (i.e there are missing days) it will re-run that year.
  tar_target(modis_ndvi_task_id_continent, submit_modis_ndvi_task_request_continent(end_date = modis_task_end_dates,
                                                                                    modis_ndvi_token,
                                                                                    bbox_coords = sf::st_bbox(continent_polygon),
                                                                                    modis_ndvi_transformed_directory),
             pattern = map(modis_task_end_dates),
             cue = tar_cue("always")),
  
  # Set up modis_ndvi data requests
  tar_target(modis_ndvi_bundle_request, submit_modis_ndvi_bundle_request(modis_ndvi_token, modis_ndvi_task_id_continent),
             pattern = map(modis_ndvi_task_id_continent)),
  
  # Check if modis_ndvi files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(modis_ndvi_transformed_AWS, AWS_get_folder(modis_ndvi_transformed_directory,
                                                        modis_ndvi_token,
                                                        modis_ndvi_bundle_request,
                                                        continent_raster_template,
                                                        modis_ndvi_transformed_directory),
             error = "null",
             cue = tar_cue("always")),
  
  # Collect branches from modis_ndvi_bundle_request and split into branches 
  # where each branch is a batch of 10 requests
  # MODIS NDVI refers to the highest Normalized Difference 
  # Vegetation Index (NDVI) value recorded within a 16-day period.
  # We're joining that to daily data. One approach would be spline based interpolation 
  # but then it would be tough to figure out what to do with NDVI 
  # when we go to forecast. Right now we're just using step function 
  # interpolation where the NDVI value is constant for the entire 16-day period, 
  # then it steps up or down to the next interval's NDVI value.
  tarchetypes::tar_group_size(name = modis_ndvi_requests, 
                              size = 10,
                              command = modis_ndvi_bundle_request |>
                                arrange(start_date) |>
                                mutate(end_date = lead(start_date) - days(1),
                                       end_date = case_when(
                                         is.na(end_date) ~ start_date + 15,
                                         TRUE ~ end_date
                                       ),
                                       interval = end_date - start_date, 10)),
 
  # Download data, project to the template and save as parquets
  # TODO NAs outside of the continent
  # Not Found HTTP 404 means the bundle request hasn't finished processing
  # transform_modis_ndvi()
  tar_target(modis_ndvi_transformed, 
             map_vec(1:nrow(modis_ndvi_requests), # This map is batching: multiple requests per branch
                     ~transform_modis_ndvi(modis_ndvi_token, 
                                           modis_ndvi_requests[.x,],
                                           continent_raster_template,
                                           modis_ndvi_transformed_directory,
                                           overwrite = as.logical(Sys.getenv("OVERWRITE_MODIS_NDVI", unset = "FALSE")),
                                           modis_ndvi_transformed_AWS)), # Enforce dependency
             pattern = map(modis_ndvi_requests), # This map is branching: multiple branches per bundle
             format = "file",
             repository = "local",
             error = "null"), # Repository local means it isn't stored on AWS just yet.

  # Put modis_ndvi_transformed files on AWS
  tar_target(modis_ndvi_transformed_AWS_upload, AWS_put_files(modis_ndvi_transformed,
                                                              modis_ndvi_transformed_directory),
             error = "null"),
  
  # Combine Sentinel an MODIS ndvi data and interopolate to daily interval
  # Check if modis_ndvi files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(ndvi_transformed_directory, 
             create_data_directory(directory_path = "data/ndvi_transformed")),
  
  tar_target(ndvi_transformed_AWS, AWS_get_folder(ndvi_transformed_directory,
                                                  modis_ndvi_transformed,
                                                  sentinel_ndvi_transformed,
                                                  model_dates_selected),
             error = "null",
             cue = tar_cue("always")),
  
  tar_target(ndvi_years, lubridate::year(modis_task_end_dates)),
  
  # Combine modis and sentinel datasets into a single source for lagging
  tar_target(ndvi_transformed, transform_ndvi(modis_ndvi_transformed,
                                              sentinel_ndvi_transformed,
                                              ndvi_transformed_directory,
                                              model_dates_selected,
                                              overwrite = parse_flag("OVERWRITE_MODIS_NDVI") | parse_flag("OVERWRITE_SENTINEL_NDVI")),
             pattern = map(ndvi_years),
             format = "file",
             repository = "local",
             error = "null"),
  
  # Put ndvi_transformed files on AWS
  tar_target(ndvi_transformed_AWS_upload, AWS_put_files(ndvi_transformed,
                                                        ndvi_transformed_directory),
             error = "null"),

  
  # NASA POWER recorded weather -----------------------------------------------------------
  # RH2M            MERRA-2 Relative Humidity at 2 Meters (%) ;
  # T2M             MERRA-2 Temperature at 2 Meters (C) ;
  # PRECTOTCORR     MERRA-2 Precipitation Corrected (mm/day)  
  tar_target(nasa_weather_transformed_directory, 
             create_data_directory(directory_path = "data/nasa_weather_transformed")),
  
  # Set branching for nasa_weather download
  tar_target(nasa_weather_years, 2005:(year(Sys.time()) - 1)),
  tar_target(nasa_weather_variables, c("RH2M", "T2M", "PRECTOTCORR")),
  tar_target(nasa_weather_coordinates, get_nasa_weather_coordinates(country_bounding_boxes)),
  
  # Check if nasa_weather file already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(nasa_weather_AWS, AWS_get_folder(nasa_weather_transformed_directory,
                                              nasa_weather_coordinates, # Enforce Dependency
                                              nasa_weather_years, # Enforce Dependency
                                              continent_raster_template),
             error = "null",
             cue = tar_cue("always")), # Enforce Dependency
  
  # Process the weather data
  tar_target(nasa_weather_transformed, transform_nasa_weather(nasa_weather_coordinates,
                                                              nasa_weather_years,
                                                              nasa_weather_variables = c("RH2M", "T2M", "PRECTOTCORR"),
                                                              continent_raster_template,
                                                              local_folder = nasa_weather_transformed_directory,
                                                              overwrite = as.logical(Sys.getenv("OVERWRITE_NASA_WEATHER", unset = "FALSE")),
                                                              nasa_weather_AWS), # Enforce Dependency
             pattern = map(nasa_weather_years),
             error = "null",
             format = "file",
             repository = "local"),
  
  # Put nasa_weather files on AWS
  tar_target(nasa_weather_transformed_AWS_upload, AWS_put_files(nasa_weather_transformed,
                                                                nasa_weather_transformed_directory),
             error = "null"),
  
  
  # How many months out are we forecasting?
  tar_target(ecmwf_lead_months, seq(1,6)),
  
  # ECMWF Weather Forecast data -----------------------------------------------------------
  tar_target(ecmwf_forecasts_transformed_directory, 
             create_data_directory(directory_path = "data/ecmwf_forecasts_transformed")),
  
  # set branching for ecmwf download
  # Note: Neet to auto update years here.
  tar_target(ecmwf_forecasts_api_parameters, set_ecmwf_api_parameter(start_year = 2005,
                                                                     bbox_coords = sf::st_bbox(terra::rast(continent_raster_template)),
                                                                     variables = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                                                     # product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                                                     product_types = c("monthly_mean"),
                                                                     lead_months = ecmwf_lead_months),
             cue = tar_cue("always")),
  
  # Check if ecmwf files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(get_ecmwf_forecasts_AWS, AWS_get_folder(ecmwf_forecasts_transformed_directory,
                                                     ecmwf_forecasts_api_parameters, # Enforce Dependency
                                                     continent_raster_template),
             error = "null",
             cue = tar_cue("never")), # Enforce Dependency
  
  # Download ecmwf forecasts, project to the template and save as arrow dataset
  # Note: This target takes a while (mostly because the ECMWF API is slow)
  # and may need to be run more than once if rebuilding data from scratch 
  # because it's also prone to random failures. Expected parquet file size
  # is ~100MB.
  # If this target fails it could be the API is down. Check status at https://status.ecmwf.int/
  # TODO: NAs outside of the continent
  tar_target(ecmwf_forecasts_transformed, 
             transform_ecmwf_forecasts(ecmwf_forecasts_api_parameters,
                                       local_folder = ecmwf_forecasts_transformed_directory,
                                       continent_raster_template,
                                       overwrite = parse_flag("OVERWRITE_ECMWF_FORECASTS"),
                                       get_ecmwf_forecasts_AWS), # Enforce Dependency
             pattern = map(ecmwf_forecasts_api_parameters),
             error = "null",
             format = "file",
             repository = "local",
             cue = tar_cue(parse_flag("OVERWRITE_ECMWF_FORECASTS", cue = T))),
  
  # Next step put modis_ndvi_transformed files on AWS.
  tar_target(ecmwf_forecasts_transformed_AWS_upload, AWS_put_files(ecmwf_forecasts_transformed,
                                                                   ecmwf_forecasts_transformed_directory),
             error = "null"),

)

# Data Processing -----------------------------------------------------------
# Why are we subsetting here again?
data_targets <- tar_plan(
  
  # How far back should we look for lagged responses?
  # These will be the mean conditions 0-30 days back, 30-60 days back, and 60-90 days back
  # Must start with zero.
  tar_target(lag_intervals, c(0, 30, 60, 90)), 
  
  # How far out are we forecasting?
  # 0-30, 30-60, 60-90 days out ect...
  tar_target(forecast_intervals, c(0, 30, 60, 90, 120, 150)), 
  
  # NCL: This function produces a random sampling of n_per_month dates for every month
  # in every year between start_year and end_year. If a new year is added, the 
  # random draws for the previous years won't change unless the seed is updated. 
  tar_target(model_dates_selected, set_model_dates(start_year = 2005, 
                                                   end_year = lubridate::year(Sys.time()), 
                                                   n_per_month = 2, 
                                                   seed = 212)
  ),
  
  # Recorded weather anomalies --------------------------------------------------
  tar_target(weather_historical_means_directory, 
             create_data_directory(directory_path = "data/weather_historical_means")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(weather_historical_means_AWS, AWS_get_folder(weather_historical_means_directory,
                                                          nasa_weather_transformed),
             error = "null",
             cue = tar_cue("always")), # Enforce dependency
  
  tar_target(weather_historical_means, calculate_weather_historical_means(nasa_weather_transformed,
                                                                          weather_historical_means_directory,
                                                                          weather_historical_means_AWS), # Enforce dependency
             format = "file", 
             repository = "local"),  
  
  # Next step put weather_historical_means files on AWS.
  tar_target(weather_historical_means_AWS_upload, AWS_put_files(weather_historical_means,
                                                                weather_historical_means_directory),
             error = "null"),
  
  tar_target(weather_anomalies_directory, 
             create_data_directory(directory_path = "data/weather_anomalies")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(weather_anomalies_AWS, AWS_get_folder(weather_anomalies_directory,
                                                   weather_historical_means, # Enforce dependency
                                                   model_dates_selected, # Enforce dependency
                                                   nasa_weather_transformed),
             error = "null",
             cue = tar_cue("always")), # Enforce dependency
  
  # Weather anomalies are deviations from the historical mean
  tar_target(weather_anomalies, calculate_weather_anomalies(nasa_weather_transformed,
                                                            weather_historical_means,
                                                            weather_anomalies_directory,
                                                            model_dates_selected,
                                                            overwrite = as.logical(Sys.getenv("OVERWRITE_WEATHER_ANOMALIES", unset = "FALSE")),
                                                            weather_anomalies_AWS), # Enforce dependency
             pattern = map(model_dates_selected),
             error = "null",
             format = "file", 
             repository = "local"),  
  
  # Next step put weather_historical_means files on AWS.
  tar_target(weather_anomalies_AWS_upload, AWS_put_files(weather_anomalies,
                                                         weather_anomalies_directory),
             error = "null"),
  
  
  # forecast weather anomalies ----------------------------------------------------------------------
  tar_target(forecasts_anomalies_directory, 
             create_data_directory(directory_path = "data/forecast_anomalies")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(forecasts_anomalies_AWS, AWS_get_folder(forecasts_anomalies_directory,
                                                     weather_historical_means, # Enforce dependency
                                                     model_dates_selected, # Enforce dependency
                                                     ecmwf_forecasts_transformed),
             error = "null",
             cue = tar_cue("always")), # Enforce dependency
  
  # Calculate the scaled and unscaled difference between the forecast mean and the 
  # historical mean across different lead intervals. The lead intervals reflect
  # how far out the forecast is. For example 0-30 days out, 30-60 days out ect.
  # Expected target size is ~40MB. Each branch takes 1-2 minutes in serial
  # on an M1 mac. Expect to take a day to regenerate the data if re-building from
  # scratch.
  # To join to other data: 
  # forecasts_anomalies |>
  #   collect() |>
  #   select(-contains("start")) |> 
  #   pivot_wider(names_from = forecast_intervals_end,
  #               values_from = starts_with("anomaly_forecast"),
  #               names_sep = "_")

  tar_target(forecasts_anomalies, calculate_forecasts_anomalies(ecmwf_forecasts_transformed,
                                                                weather_historical_means,
                                                                forecasts_anomalies_directory,
                                                                model_dates_selected,
                                                                forecast_intervals,
                                                                overwrite = as.logical(Sys.getenv("OVERWRITE_FORECAST_ANOMALIES", unset = "FALSE")),
                                                                ecmwf_forecasts_transformed,# Enforce dependency
                                                                forecasts_anomalies_AWS), # Enforce dependency
             pattern = map(model_dates_selected),
             error = "null",
             format = "file", 
             repository = "local"), 
  
  # Next step put weather_historical_means files on AWS.
  tar_target(forecasts_anomalies_AWS_upload, AWS_put_files(forecasts_anomalies, 
                                                           forecasts_anomalies_directory),
             error = "null"),
  
  tar_target(ndvi_historical_means_directory, 
             create_data_directory(directory_path = "data/ndvi_historical_means")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(ndvi_historical_means_AWS, AWS_get_folder(ndvi_historical_means_directory,
                                                       sentinel_ndvi_transformed, # Enforce dependency
                                                       modis_ndvi_transformed),
             error = "null",
             cue = tar_cue("always")), # Enforce dependency
  
  # NCL NOTE: MAKE NDVI TRANSFORMED THAT DOES THE STEP INTERPOLATION AND COMBINES NDVI SOURCES
  # FEED THAT INTO ANOMALIES. WE NEED IT SEPARATE TO DO LAGS.
  tar_target(ndvi_historical_means, calculate_ndvi_historical_means(sentinel_ndvi_transformed,
                                                                    modis_ndvi_transformed,
                                                                    ndvi_historical_means_directory,
                                                                    ndvi_historical_means_AWS), # Enforce dependency
             format = "file", 
             repository = "local"),  

  
  # Next step put ndvi_historical_means files on AWS.
  tar_target(ndvi_historical_means_AWS_upload, AWS_put_files(ndvi_historical_means,
                                                             ndvi_historical_means_directory),
             error = "null"),
  
  
  tar_target(ndvi_anomalies_directory, 
             create_data_directory(directory_path = "data/ndvi_anomalies")),
  
  # Check if ndvi_anomalies_AWS parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(ndvi_anomalies_AWS, AWS_get_folder(ndvi_anomalies_directory,
                                                ndvi_historical_means, # Enforce dependency
                                                model_dates_selected), # Enforce dependency
             error = "null",
             cue = tar_cue("always")), # Enforce dependency
  
  tar_target(ndvi_anomalies, calculate_ndvi_anomalies(ndvi_transformed,
                                                      ndvi_historical_means,
                                                      ndvi_anomalies_directory,
                                                      model_dates_selected,
                                                      overwrite = as.logical(Sys.getenv("OVERWRITE_NDVI_ANOMALIES", unset = "FALSE")),
                                                      ndvi_anomalies_AWS), # Enforce dependency
             pattern = map(model_dates_selected),
             error = "null",
             format = "file", 
             repository = "local"),  
  
  # Next step put ndvi_historical_means files on AWS.
  tar_target(ndvi_anomalies_AWS_upload, AWS_put_files(ndvi_anomalies,
                                                      ndvi_anomalies_directory),
             error = "null"),
  
  # Get lagged ndvi data
  tar_target(ndvi_transformed_lagged_directory, 
             create_data_directory(directory_path = "data/ndvi_transformed_lagged")),
  
  tar_target(ndvi_transformed_lagged_AWS, AWS_get_folder(ndvi_transformed_lagged_directory,
                                                         ndvi_historical_means, # Enforce dependency
                                                         model_dates_selected), # Enforce dependency
             error = "null",
             cue = tar_cue("always")), # Enforce dependency
  
  tar_target(ndvi_transformed_lagged, lag_data(ndvi_transformed,
                                               lag_intervals,
                                               model_dates_selected,
                                               overwrite = TRUE,
                                               ndvi_transformed_lagged_AWS), # Enforce dependency
             pattern = map(model_dates_selected),
             error = "null",
             format = "file", 
             repository = "local"),  
  
  # Next step put ndvi_historical_means files on AWS.
  tar_target(ndvi_transformed_lagged_AWS_upload, AWS_put_files(ndvi_anomalies,
                                                      ndvi_anomalies_directory),
             error = "null"),
  
  
  tar_target(augmented_data_directory,
             create_data_directory(directory_path = "data/augmented_data")),
  
  # Check if ndvi_anomalies_AWS parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(augmented_data_AWS, AWS_get_folder(augmented_data_directory,
                                                augmented_data_sources),
             error = "null",
             cue = tar_cue("always")), # Enforce dependency

  
  # Notes: This data will be aggregated. Mean monthly anomaly lagged 1, 2, 3 months back
  # Current date, lagged ndvi and weather anomalies, current ndvi and weather anomalies, forecast anomaly
  # over forecast interval. Forecast interval Response is number of outbreaks (or cases?) over forecast interval
  # so count data and poisson model. Then we have multiple forecast intervals we're working on. 0-30, 30-60, 60-90, ect..
  # We can get a rate for each interval and use simulation to determine the probability of no outbreaks across
  # combined interval and error probability no outbreaks in each interval is (1-e^-lambda)
  
  # Combine all static and dynamic data layers by x, y, date, or doy
  # Partition to complete dataset out into separate parquet files by year.
  tar_target(augmented_data, list(response = rvf_outbreaks,
                                  outbreak_history_layers = wahis_outbreak_history, # Good to join
                                  static_layers = c(soil_preprocessed, # Good to join
                                                    aspect_preprocessed, # Good to join
                                                    slope_preprocessed, # Good to join
                                                    glw_preprocessed, # Good to join
                                                    elevation_preprocessed, # Good to join
                                                    bioclim_preprocessed, # Good to join
                                                    landcover_preprocessed), # Good to join
                                  dynamic_layers = c(weather_historical_means, # Good to join
                                                     ndvi_historical_means, # Good to join
                                                     ecmwf_forecasts_transformed, # Good to join
                                                     weather_anomalies, # Add in lagged anomalies lag_anomaly()
                                                     forecasts_anomalies, # Need to add month and year as integer for partitioning
                                                     ndvi_anomalies)) |> # Add in lagged anomalies
               unlist() |>
               arrow::open_dataset(unify_schemas = T) |>
               arrow::write_dataset(partitioning = c("year", "month")), 
             repository = "local"),

  # Next step put combined_anomalies files on AWS.
  tar_target(augmented_data_AWS_upload, AWS_put_files(augmented_data,
                                                      augmented_data_directory),
             error = "null"),

)

# Model -----------------------------------------------------------
model_targets <- tar_plan(
  
  # RSA --------------------------------------------------
  tar_target(augmented_data_rsa_directory,
             create_data_directory(directory_path = "data/augmented_data_rsa")),
  
  # # Switch to parquet based to save memory. Arrow left joins automatically.
  # tar_target(model_data,
  #            left_join(aggregated_data_rsa,
  #                      rvf_outbreaks,
  #                      by = join_by(date, shapeName)) |>
  #              mutate(outbreak_30 = factor(replace_na(outbreak_30, FALSE))) |>
  #              left_join(rsa_polygon_spatial_weights, by = "shapeName") |>
  #              mutate(area = as.numeric(area))
  # ),
  # 
  # tar_target(training_data, training(model_data_split)),
  # tar_target(holdout_data, testing(model_data_split)),
   
  # # Check if combined_anomalies parquet files already exists on AWS and can be loaded
  # # The only important one is the directory. The others are there to enforce dependencies.
  # tar_target(augmented_data_rsa_AWS, AWS_get_folder(augmented_data_rsa_directory,
  #                                                   weather_anomalies, # Enforce dependency
  #                                                   ndvi_anomalies, # Enforce dependency
  #                                                   model_dates_selected),
  #            error = "null",
  #            cue = tar_cue("always")), # Enforce dependency
  #
  # tar_target(aggregated_data_rsa,
  #            aggregate_augmented_data_by_adm(augmented_data,
  #                                            rsa_polygon,
  #                                            model_dates_selected),
  #            pattern = model_dates_selected),
  #
  # tar_target(rsa_polygon_spatial_weights, rsa_polygon |>
  #              mutate(area = sf::st_area(rsa_polygon)) |>
  #              as_tibble() |>
  #              select(shapeName, area)),

  # # Switch to parquet based to save memory. Arrow left joins automatically.
  # tar_target(model_data,
  #            left_join(aggregated_data_rsa,
  #                      rvf_outbreaks,
  #                      by = join_by(date, shapeName)) |>
  #              mutate(outbreak_30 = factor(replace_na(outbreak_30, FALSE))) |>
  #              left_join(rsa_polygon_spatial_weights, by = "shapeName") |>
  #              mutate(area = as.numeric(area))
  # ),
  
  # 
  # # Splitting --------------------------------------------------
  # # Initial train and test (ie holdout)
  # tar_target(split_prop, nrow(model_data[model_data$date <= "2017-12-31",])/nrow(model_data)),
  # tar_target(model_data_split, initial_time_split(model_data, prop = split_prop)), 
  # tar_target(training_data, training(model_data_split)),
  # tar_target(holdout_data, testing(model_data_split)),
  # 
  # # formula/recipe 
  # tar_target(rec, model_recipe(training_data)),
  # tar_target(rec_juiced, juice(prep(rec))),
  # 
  # # xgboost settings
  # tar_target(base_score, sum(training_data$outbreak_30==TRUE)/nrow(training_data)),
  # tar_target(interaction_constraints, '[[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [15]]'), # area is the 16th col in rec_juiced
  # tar_target(monotone_constraints, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)), # enforce positive relationship for area
  # 
  # # tuning
  # tar_target(spec, model_specs(base_score, interaction_constraints, monotone_constraints)),
  # tar_target(grid, model_grid(training_data)),
  # 
  # # workflow
  # tar_target(wf, workflows::workflow(rec, spec)),
  # 
  # # splits
  # tar_target(rolling_n, n_distinct(model_data$shapeName)),
  # tar_target(splits, rolling_origin(training_data, 
  #                                   initial = rolling_n, 
  #                                   assess = rolling_n, 
  #                                   skip = rolling_n - 1)),
  # 
  # # tuning
  # tar_target(tuned, model_tune(wf, splits, grid)),
  
  # final model
  # tar_target(final, {
  #   final_wf <- finalize_workflow(
  #     wf,
  #     tuned[5,]
  #   )
  #   
  #   library(DALEX)
  #   library(ceterisParibus)
  #   
  #   # DALEX Explainer
  #   tuned_model <- final_wf |> fit(training_data)
  #   tuned_model_xg <- extract_fit_parsnip(tuned_model)
  #   training_data_mx <- extract_mold(tuned_model)$predictors %>%
  #     as.matrix()
  #   
  #   y <- extract_mold(tuned_model)$outcomes %>%
  #     mutate(outbreak_30 = as.integer(outbreak_30 == "1")) %>%
  #     pull(outbreak_30)
  #   
  #   explainer <- DALEX::explain(
  #     model = tuned_model_xg,
  #     data = training_data_mx,
  #     y = y,
  #     predict_function = predict_raw,
  #     label = "RVF-EWS",
  #     verbose = TRUE
  #   )
  #   
  #   # CP plots
  #   predictors <- extract_mold(tuned_model)$predictors |> colnames()
  #   holdout_small <- as.data.frame(select_sample(training_data, 20)) |> 
  #     select(all_of(predictors), outbreak_30) |> 
  #     mutate(area = as.numeric(area)) |> 
  #     mutate(outbreak_30 = as.integer(outbreak_30 == "1"))
  # 
  #   
  #   
  # 
  #   cPplot <- ceterisParibus::ceteris_paribus(explainer, 
  #                                             observation = holdout_small |> select(-outbreak_30),
  #                                             y = holdout_small |>  pull(outbreak_30)#,
  #                                             #variables = "area"
  #                                             )
  #   plot(cPplot)+
  #     ceteris_paribus_layer(cPplot, show_rugs = TRUE)
  #   
  #   
  # }),
  
  
  
  #TODO fit final model
  #TODO test that interaction constraints worked - a) extract model object b) cp - 
  # need the conditional effect - area is x, y is effect, should not change when you change other stuff
  # ceteris parabus plots - should be parallel - points can differ but profile should be the same - expectation is that it is linear if doing it on area
  
  
  
)

# Deploy -----------------------------------------------------------
deploy_targets <- tar_plan(
  
)

# Plots -----------------------------------------------------------
plot_targets <- tar_plan(
  
)

# Reports -----------------------------------------------------------
report_targets <- tar_plan(
  
)

# Testing -----------------------------------------------------------
test_targets <- tar_plan(
  
)

# Documentation -----------------------------------------------------------
documentation_targets <- tar_plan(
  # tar_target(readme, rmarkdown::render("README.Rmd"))
  tar_render(readme, path = here::here("README.Rmd"))
)


# List targets -----------------------------------------------------------------
# all_targets() doesn't work with tarchetypes like tar_change().
list(static_targets,
     dynamic_targets,
     data_targets,
     model_targets,
     deploy_targets,
     report_targets,
     test_targets,
     documentation_targets)
