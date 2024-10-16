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

# Targets cue
# By default, the tar_cue is "thorough", which means that when `tar_make()` is called, it will rebuild a target if any of the code has changed
# If the code has not changed, `tar_make()` will skip over the target
# For some targets with many branches (i.e., COMTRADE), it takes a long time for `tar_make()` to check and skip over already-built targets
# For development purposes only, it can be helpful to set these targets to have a tar_cue of tar_cue_upload_aws, which means targets will not check the target for changes after it has been built once

tar_cue_general = "thorough" # CAUTION changing this to never means targets can miss changes to the code. Use only for developing.
tar_cue_upload_aws = "thorough"  # CAUTION changing this to never means targets can miss changes to the code. Use only for developing.

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
  tar_target(continent_bounding_box, sf::st_bbox(continent_polygon)),
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
             error = "null"), # Continue the pipeline even on error
  
  tar_target(soil_preprocessed, preprocess_soil(soil_directory, 
                                                continent_raster_template, 
                                                overwrite = FALSE,
                                                soil_AWS), # Enforce dependency
             format = "file",
             repository = "local"),
  
  tar_target(soil_preprocessed_AWS_upload, AWS_put_files(soil_preprocessed, 
                                                         aspect_directory),
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
             error = "null"), # Enforce Dependency
  
  tar_target(aspect_preprocessed, get_remote_rasters(urls = aspect_urls, 
                                                     output_dir = aspect_directory,
                                                     output_filename = "aspect.parquet",
                                                     continent_raster_template,
                                                     aggregate_method = "which.max", # What is the dominant aspect for each point?
                                                     resample_method = "mode", # What is the dominant aspect at the scale of the template raster?
                                                     overwrite = FALSE,
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
             error = "null"), # Continue the pipeline even on error
  
  tar_target(slope_preprocessed, get_remote_rasters(urls = slope_urls, 
                                                    output_dir = slope_directory,
                                                    output_filename = "slope.parquet",
                                                    continent_raster_template,
                                                    aggregate_method = "which.max", # What is the dominant slope for each point?
                                                    resample_method = "mode", # What is the dominant slope at the scale of the template raster?
                                                    overwrite = FALSE,
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
             error = "null"), # Continue the pipeline even on error
  
  tar_target(glw_preprocessed, 
             preprocess_glw_data(glw_directory, 
                                 glw_urls,
                                 continent_raster_template,
                                 overwrite = TRUE,
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
           error = "null"), # Continue the pipeline even on error

# NCL NEEDS TO SAVE AS PARQUET
tar_target(elevation_preprocessed, 
           get_elevation_data(output_dir = elevation_directory, 
                              output_filename = "africa_elevation.parquet",
                              continent_raster_template,
                              overwrite = FALSE,
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
           error = "null"), # Continue the pipeline even on error

tar_target(bioclim_preprocessed,
           get_bioclim_data(output_dir = bioclim_directory, 
                            output_filename = "bioclim.parquet",
                            continent_raster_template,
                            overwrite = FALSE),
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
           error = "null"), # Continue the pipeline even on error

tar_target(landcover_preprocessed,
           get_landcover_data(output_dir = landcover_directory, 
                              output_filename = "landcover.parquet",
                              landcover_types,
                              continent_raster_template,
                              overwrite = FALSE,
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

  # OUTBREAK HISTORY -----------------------------------------------------------
  tar_target(wahis_outbreak_dates, tibble(date = seq(from = min(coalesce(wahis_rvf_outbreaks_preprocessed$outbreak_end_date, wahis_rvf_outbreaks_preprocessed$outbreak_start_date), na.rm = T),
                                                     to = max(coalesce(wahis_rvf_outbreaks_preprocessed$outbreak_end_date, wahis_rvf_outbreaks_preprocessed$outbreak_start_date), na.rm = T),
                                                     by = "day"),
                                          year = year(date),
                                          month = month(date)) |>
               group_by(year) |>
               tar_group(),
             iteration = "group"),
  
  tar_target(wahis_outbreaks, wahis_rvf_outbreaks_preprocessed |> 
               mutate(end_date = coalesce(outbreak_end_date, outbreak_start_date), na.rm = T) |>
               select(cases, end_date, latitude, longitude) |>
               distinct() |>
               arrange(end_date) |>
               mutate(outbreak_id = 1:n())),
  
  tar_target(wahis_raster_template, terra::rasterize(terra::vect(continent_polygon), # Take the boundary of Africa
                                                     terra::rast(continent_polygon, # Mask against a raster filled with 1's
                                                                 resolution = 0.1, # Set resolution
                                                                 vals = 1)) |>
               terra::wrap()), # Wrap to avoid problems with targets
  
  tar_target(wahis_distance_matrix, get_outbreak_distance_matrix(wahis_outbreaks, wahis_raster_template)),
  
  tar_target(wahis_outbreak_history_directory, 
             create_data_directory(directory_path = "data/outbreak_history_dataset")),
  
  # Check if preprocessed wahis_outbreak_history data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
  tar_target(wahis_outbreak_history_AWS, AWS_get_folder(wahis_outbreak_history_directory,
                                                        wahis_outbreak_dates, # Enforce Dependency
                                                        wahis_outbreaks, # Enforce Dependency
                                                        wahis_distance_matrix, # Enforce Dependency
                                                        wahis_raster_template), # Enforce Dependency
             error = "null"), # Continue the pipeline even on error
  
  # Dynamic branch over year batch over day otherwise too many branches.
  tar_target(wahis_outbreak_history, get_daily_outbreak_history(dates_df = wahis_outbreak_dates,
                                                                wahis_outbreaks,
                                                                wahis_distance_matrix,
                                                                wahis_raster_template,
                                                                output_dir = wahis_outbreak_history_directory,
                                                                output_filename = "outbreak_history.parquet",
                                                                beta_time = 0.5,
                                                                max_years = 10,
                                                                recent = 3/12,
                                                                overwrite = FALSE,
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
  
  # Check if preprocessed wahis_outbreak_history data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
  tar_target(wahis_outbreak_history_animations_AWS, AWS_get_folder(wahis_outbreak_history_animations_directory,
                                                                   wahis_outbreak_history), # Enforce Dependency
             error = "null"), # Continue the pipeline even on error
  
  # Animate a SpatRaster stack where each layer is a date.
  # gganimate took 20 minutes per file.
  # just saving all the frames as separate pngs
  # and combining with gifski took 50 minutes for all of them.
  # get_outbreak_history_animation()
  tar_target(wahis_outbreak_history_animations, get_outbreak_history_animation(wahis_outbreak_history,
                                                                               wahis_outbreak_history_animations_directory), # Just included to enforce dependency with wahis_outbreak_history
             pattern = map(wahis_outbreak_history),
             error = "null",
             repository = "local"), 

  tar_target(wahis_outbreak_history_animations_AWS_upload, AWS_put_files(wahis_outbreak_history_animations,
                                                                         wahis_outbreak_history_animations_directory),
             error = "null"), # Continue the pipeline even on error

  # SENTINEL NDVI -----------------------------------------------------------
  # 2018-present
  # 10 day period
  # tar_target(sentinel_ndvi_raw_directory, 
  #            create_data_directory(directory_path = "data/sentinel_ndvi_raw")),
  tar_target(sentinel_ndvi_transformed_directory, 
             create_data_directory(directory_path = "data/sentinel_ndvi_transformed")),
  
  tar_target(get_sentinel_ndvi_AWS, AWS_get_folder(sentinel_ndvi_transformed_directory)),
  
  # get API parameters
  tar_target(sentinel_ndvi_api_parameters, get_sentinel_ndvi_api_parameters()), 
  
  tar_target(sentinel_ndvi_transformed, 
             transform_sentinel_ndvi(sentinel_ndvi_api_parameters, 
                                     continent_raster_template,
                                     sentinel_ndvi_transformed_directory,
                                     overwrite = FALSE,
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
  # 16 day period
  tar_target(modis_ndvi_transformed_directory, 
             create_data_directory(directory_path = "data/modis_ndvi_transformed")),
  
  # This target reads in an Appears token from the .env file and tests that it 
  # still works. It requests a new token and updates the .env file if not.
  tar_target(modis_ndvi_token, get_modis_ndvi_token(), cue = tar_cue("always")),
  
  # set parameters and submit request for full continent
  tar_target(modis_ndvi_task_id_continent, submit_modis_ndvi_task_request_continent(modis_ndvi_start_year = 2005,
                                                                                    modis_ndvi_token,
                                                                                    bbox_coords = continent_bounding_box)),
  
  tar_target(modis_ndvi_bundle_request_file, file.path(modis_ndvi_transformed_directory, "modis_ndvi_bundle_request.RDS")),
  
  # Set up modis_ndvi data requests
  tar_target(modis_ndvi_bundle_request, submit_modis_ndvi_bundle_request(modis_ndvi_token, 
                                                                         modis_ndvi_task_id_continent, 
                                                                         modis_ndvi_bundle_request_file) |> 
               filter(grepl("NDVI", file_name)),
             cue = tar_cue("always")),
  
  # Check if modis_ndvi files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(modis_ndvi_transformed_AWS, AWS_get_folder(modis_ndvi_transformed_directory,
                                                        modis_ndvi_token,
                                                        modis_ndvi_bundle_request,
                                                        continent_raster_template,
                                                        modis_ndvi_transformed_directory)),
 
  # Download data, project to the template and save as parquets
  # TODO NAs outside of the continent
  tar_target(modis_ndvi_transformed, 
             transform_modis_ndvi(modis_ndvi_token, 
                                  modis_ndvi_bundle_request,
                                  continent_raster_template,
                                  modis_ndvi_transformed_directory,
                                  overwrite = FALSE,
                                  modis_ndvi_transformed_AWS), # Enforce dependency
             pattern = map(modis_ndvi_bundle_request),
             error = "null", # Keep going if error. It will be caught next time the pipeline is run.
             format = "file",
             repository = "local", # Repository local means it isn't stored on AWS just yet.
             cue = tar_cue(tar_cue_general)), 
  
  # Put modis_ndvi_transformed files on AWS
  tar_target(modis_ndvi_transformed_AWS_upload, AWS_put_files(modis_ndvi_transformed,
                                                              modis_ndvi_transformed_directory)),
  
  # NASA POWER recorded weather -----------------------------------------------------------
  # RH2M            MERRA-2 Relative Humidity at 2 Meters (%) ;
  # T2M             MERRA-2 Temperature at 2 Meters (C) ;
  # PRECTOTCORR     MERRA-2 Precipitation Corrected (mm/day)  
  tar_target(nasa_weather_transformed_directory, 
             create_data_directory(directory_path = "data/nasa_weather_transformed")),
  
  # Set branching for nasa_weather download
  tar_target(nasa_weather_years, 2005:2023),
  tar_target(nasa_weather_variables, c("RH2M", "T2M", "PRECTOTCORR")),
  tar_target(nasa_weather_coordinates, get_nasa_weather_coordinates(country_bounding_boxes)),
  
  # Check if nasa_weather file already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(nasa_weather_AWS, AWS_get_folder(nasa_weather_transformed_directory,
                                              nasa_weather_coordinates, # Enforce Dependency
                                              nasa_weather_years, # Enforce Dependency
                                              continent_raster_template)), # Enforce Dependency
  
  tar_target(nasa_weather_transformed, transform_nasa_weather(nasa_weather_coordinates,
                                                              nasa_weather_years,
                                                              continent_raster_template,
                                                              local_folder = nasa_weather_transformed_directory,
                                                              overwrite = FALSE,
                                                              nasa_weather_AWS), # Enforce Dependency
             pattern = map(nasa_weather_years),
             error = "null",
             format = "file",
             repository = "local",
             cue = tar_cue(tar_cue_general)),
  
  # Put nasa_weather files on AWS
  tar_target(nasa_weather_transformed_AWS_upload, AWS_put_files(modis_ndvi_transformed,
                                                                modis_ndvi_transformed_directory)),
  
  
  # ECMWF Weather Forecast data -----------------------------------------------------------
  tar_target(ecmwf_forecasts_transformed_directory, 
             create_data_directory(directory_path = "data/ecmwf_forecasts_transformed")),
  
  # set branching for ecmwf download
  tar_target(ecmwf_forecasts_api_parameters, set_ecmwf_api_parameter(years = 2005:2024,
                                                                     bbox_coords = continent_bounding_box,
                                                                     variables = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                                                     # product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                                                     product_types = c("monthly_mean"),
                                                                     leadtime_months = c("1", "2", "3", "4", "5", "6"))),
  
  # Check if ecmwf files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(get_ecmwf_forecasts_AWS, AWS_get_folder(ecmwf_forecasts_transformed_directory,
                                                     ecmwf_forecasts_api_parameters, # Enforce Dependency
                                                     continent_raster_template)), # Enforce Dependency
  
  # Download ecmwf forecasts, project to the template 
  # and save as arrow dataset
  # TODO NAs outside of the continent
  tar_target(ecmwf_forecasts_transformed, 
             transform_ecmwf_forecasts(ecmwf_forecasts_api_parameters,
                                       local_folder = ecmwf_forecasts_transformed_directory,
                                       continent_raster_template,
                                       get_ecmwf_forecasts_AWS), # Enforce Dependency
             pattern = map(ecmwf_forecasts_api_parameters),
             error = "null",
             format = "file",
             repository = "local",
             cue = tar_cue(tar_cue_general)),
  
  # Next step put modis_ndvi_transformed files on AWS.
  tar_target(ecmwf_forecasts_transformed_AWS_upload, AWS_put_files(ecmwf_forecasts_transformed,
                                                                   ecmwf_forecasts_transformed_directory)),

)

# Data Processing -----------------------------------------------------------
data_targets <- tar_plan(
  
  tar_target(lag_intervals, c(30, 60, 90)), 
  tar_target(lead_intervals, c(30, 60, 90, 120, 150)), 
  tar_target(days_of_year, 1:365),
  tar_target(model_dates_selected, set_model_dates(start_year = 2005, 
                                                   end_year = 2022, 
                                                   n_per_month = 2, 
                                                   lag_intervals, 
                                                   seed = 212) |> 
               filter(select_date) |> pull(date)
  ),
  
  # Recorded weather anomalies --------------------------------------------------
  tar_target(weather_historical_means_directory, 
             create_data_directory(directory_path = "data/weather_historical_means")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(weather_historical_means_AWS, AWS_get_folder(weather_historical_means_directory,
                                                          days_of_year, # Enforce dependency
                                                          lag_intervals, # Enforce dependency
                                                          lead_intervals, # Enforce dependency
                                                          nasa_weather_transformed)), # Enforce dependency
  
  tar_target(weather_historical_means, calculate_weather_historical_means(nasa_weather_transformed_directory,
                                                                          weather_historical_means_directory,
                                                                          days_of_year,
                                                                          lag_intervals,
                                                                          lead_intervals,
                                                                          overwrite = FALSE,
                                                                          nasa_weather_transformed, # Enforce dependency
                                                                          weather_historical_means_AWS), # Enforce dependency
             pattern = map(days_of_year),
             error = "null",
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  

  # Next step put weather_historical_means files on AWS.
  tar_target(weather_historical_means_AWS_upload, AWS_put_files(weather_historical_means,
                                                                weather_historical_means_directory)),
  
  tar_target(weather_anomalies_directory, 
             create_data_directory(directory_path = "data/weather_anomalies")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(weather_anomalies_AWS, AWS_get_folder(weather_anomalies_directory,
                                                   weather_historical_means, # Enforce dependency
                                                   model_dates_selected, # Enforce dependency
                                                   lag_intervals, # Enforce dependency
                                                   nasa_weather_transformed)), # Enforce dependency
             
  tar_target(weather_anomalies, calculate_weather_anomalies(nasa_weather_transformed_directory,
                                                            weather_historical_means,
                                                            weather_anomalies_directory,
                                                            model_dates_selected,
                                                            lag_intervals,
                                                            overwrite = TRUE,
                                                            nasa_weather_transformed, # Enforce dependency
                                                            weather_anomalies_AWS), # Enforce dependency
             pattern = model_dates_selected,
             error = "null",
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # Next step put weather_historical_means files on AWS.
  tar_target(weather_anomalies_AWS_upload, AWS_put_files(weather_anomalies,
                                                         weather_anomalies_directory)),
  
  
  # forecast weather anomalies ----------------------------------------------------------------------
  tar_target(forecasts_anomalies_directory, 
             create_data_directory(directory_path = "data/forecast_anomalies")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(forecasts_anomalies_AWS, AWS_get_folder(forecasts_anomalies_directory,
                                                     weather_historical_means, # Enforce dependency
                                                     model_dates_selected, # Enforce dependency
                                                     lead_intervals, # Enforce dependency
                                                     ecmwf_forecasts_transformed)), # Enforce dependency
  
  tar_target(forecasts_anomalies, calculate_forecasts_anomalies(ecmwf_forecasts_transformed_directory,
                                                                weather_historical_means,
                                                                forecasts_anomalies_directory,
                                                                model_dates_selected,
                                                                lead_intervals,
                                                                overwrite = FALSE,
                                                                ecmwf_forecasts_transformed,# Enforce dependency
                                                                forecasts_anomalies_AWS), # Enforce dependency
             pattern = model_dates_selected,
             error = "null",
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)), 
  
  # Next step put weather_historical_means files on AWS.
  tar_target(forecasts_anomalies_AWS_upload, AWS_put_files(forecasts_anomalies,
                                                      forecasts_anomalies_directory)),
  
  # compare forecast anomalies to actual data
  tar_target(forecasts_validate_directory, 
             create_data_directory(directory_path = "data/forecast_validation")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(forecasts_anomalies_validate_AWS, AWS_get_folder(forecasts_validate_directory,
                                                              forecasts_anomalies, # Enforce dependency
                                                              nasa_weather_transformed, # Enforce dependency
                                                              weather_historical_means, # Enforce dependency
                                                              model_dates_selected, # Enforce dependency
                                                              lead_intervals)), # Enforce dependency
  
  tar_target(forecasts_anomalies_validate, validate_forecasts_anomalies(forecasts_validate_directory,
                                                                        forecasts_anomalies,
                                                                        nasa_weather_transformed,
                                                                        weather_historical_means,
                                                                        model_dates_selected,
                                                                        lead_intervals,
                                                                        overwrite = FALSE,
                                                                        forecasts_anomalies_validate_AWS), # Enforce dependency
             pattern = map(model_dates_selected),
             error = "null",
             format = "file",
             repository = "local",
             cue = tar_cue(tar_cue_general)), 
  
  # Next step put forecasts_anomalies_validate files on AWS.
  tar_target(forecasts_anomalies_validate_AWS_upload, AWS_put_files(forecasts_anomalies_validate,
                                                                    forecasts_validate_directory)),

  
  # ndvi anomalies --------------------------------------------------
  tar_target(ndvi_date_lookup, create_ndvi_date_lookup(sentinel_ndvi_transformed,
                                                       modis_ndvi_transformed)),
  
  tar_target(ndvi_historical_means_directory, 
             create_data_directory(directory_path = "data/ndvi_historical_means")),
  
  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(ndvi_historical_means_AWS, AWS_get_folder(ndvi_historical_means_directory,
                                                       ndvi_date_lookup, # Enforce dependency
                                                       days_of_year, # Enforce dependency
                                                       lag_intervals)), # Enforce dependency
  
  tar_target(ndvi_historical_means, calculate_ndvi_historical_means(ndvi_historical_means_directory,
                                                                    ndvi_date_lookup,
                                                                    days_of_year,
                                                                    lag_intervals,
                                                                    overwrite = FALSE,
                                                                    ndvi_historical_means_AWS), # Enforce dependency
             pattern = map(days_of_year),
             error = "null",
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # Next step put ndvi_historical_means files on AWS.
  tar_target(ndvi_historical_means_AWS_upload, AWS_put_files(ndvi_historical_means,
                                                             ndvi_historical_means_directory)),
  
  
  tar_target(ndvi_anomalies_directory, 
             create_data_directory(directory_path = "data/ndvi_anomalies")),
  
  # Check if ndvi_anomalies_AWS parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(ndvi_anomalies_AWS, AWS_get_folder(ndvi_anomalies_directory,
                                                ndvi_date_lookup, # Enforce dependency
                                                ndvi_historical_means, # Enforce dependency
                                                model_dates_selected, # Enforce dependency
                                                lag_intervals)), # Enforce dependency
  
  tar_target(ndvi_anomalies, calculate_ndvi_anomalies(ndvi_date_lookup,
                                                      ndvi_historical_means,
                                                      ndvi_anomalies_directory,
                                                      model_dates_selected,
                                                      lag_intervals,
                                                      overwrite = TRUE,
                                                      ndvi_anomalies_AWS), # Enforce dependency
             pattern = map(model_dates_selected),
             error = "null",
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # Next step put ndvi_historical_means files on AWS.
  tar_target(ndvi_anomalies_AWS_upload, AWS_put_files(ndvi_anomalies,
                                                      ndvi_anomalies_directory)),
  
  
  # Combine all anomalies --------------------------------------------------
  tar_target(combined_anomalies_directory, 
             create_data_directory(directory_path = "data/combined_anomolies")),
  
  # Check if combined_anomalies parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
  tar_target(combined_anomalies_AWS, AWS_get_folder(combined_anomalies_directory,
                                                    weather_anomalies, # Enforce dependency
                                                    ndvi_anomalies, # Enforce dependency
                                                    model_dates_selected)), # Enforce dependency
  
  tar_target(combined_anomalies, combine_anomolies(weather_anomalies,
                                                   forecasts_anomalies,
                                                   ndvi_anomalies,
                                                   combined_anomalies_directory,
                                                   combined_anomalies_AWS),
             format = "file", 
             repository = "local"), # Enforce dependency
  
  # Next step put combined_anomalies files on AWS.
  tar_target(combined_anomalies_AWS_upload, AWS_put_files(combined_anomalies,
                                                          combined_anomalies_directory)),
  
)

# Model -----------------------------------------------------------
model_targets <- tar_plan(
  
  # RSA --------------------------------------------------
  tar_target(augmented_data_rsa_directory, 
             create_data_directory(directory_path = "data/augmented_data_rsa")),
  
  # tar_target(aggregated_data_rsa,
  #            aggregate_augmented_data_by_adm(augmented_data, 
  #                                            rsa_polygon, 
  #                                            model_dates_selected),
  #            pattern = model_dates_selected,
  #            cue = tar_cue("thorough")
  # ),
  
  # tar_target(rsa_polygon_spatial_weights, rsa_polygon |> 
  #              mutate(area = st_area(rsa_polygon)) |> 
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
