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
  tar_target(continent_raster_template,
             wrap(terra::rast(ext(continent_polygon), resolution = 0.1))), 
  # nasa power resolution = 0.5; 
  # ecmwf = 1; 
  # sentinel ndvi = 0.01
  # modis ndvi = 0.01
  tar_target(rsa_polygon, rgeoboundaries::geoboundaries("South Africa", "adm2")),
  
  # SOIL -----------------------------------------------------------
  tar_target(soil_directory_raw, 
             create_data_directory(directory_path = "data/soil")),
  tar_target(soil_downloaded, soil_download(soil_directory_raw),
             format = "file", 
             repository = "local"),
  tar_target(soil_directory_dataset, 
             create_data_directory(directory_path = "data/soil_dataset")),
  tar_target(soil_preprocessed, 
             preprocess_soil(soil_directory_dataset, soil_directory_raw, continent_raster_template, soil_downloaded)),
  
  # ASPECT -------------------------------------------------
  tar_target(aspect_urls, c("aspect_zero" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClN_30as.rar",
                            "aspect_fortyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClE_30as.rar", 
                            "aspect_onethirtyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClS_30as.rar",
                            "aspect_twotwentyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClW_30as.rar",
                            "aspect_undef" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClU_30as.rar")),
  
  tar_target(aspect_preprocessed, get_remote_rasters(urls = aspect_urls, 
                                                     output_dir = "data/aspect_dataset",
                                                     output_filename = "aspect.parquet",
                                                     raster_template = continent_raster_template,
                                                     aggregate_method = "which.max", # What is the dominant aspect for each point?
                                                     resample_method = "mode"), # What is the domminant aspect at the scale of the template raster?
    format = "file", 
    repository = "local"),
  
  # SLOPE -------------------------------------------------
  tar_target(slope_urls, c("slope_zero" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl1_30as.rar",
                           "slope_pointfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl2_30as.rar",
                           "slope_two" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl3_30as.rar",
                           "slope_five" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl4_30as.rar",
                           "slope_ten" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl5_30as.rar",
                           "slope_fifteen" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl6_30as.rar",
                           "slope_thirty" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl7_30as.rar",
                           "slope_fortyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl8_30as.rar")),
  
  tar_target(slope_preprocessed, get_remote_rasters(urls = slope_urls, 
                                                    output_dir = "data/slope_dataset",
                                                    output_filename = "slope.parquet",
                                                    raster_template = continent_raster_template,
                                                    aggregate_method = "which.max", # What is the dominant slope for each point?
                                                    resample_method = "mode"), # What is the domminant slope at the scale of the template raster?
             format = "file", 
             repository = "local"),
 
   # Gridded Livestock of the world -----------------------------------------------------------
  tar_target(glw_directory_raw, 
             create_data_directory(directory_path = "data/glw")),
  tar_target(glw_downloaded, get_glw_data(glw_directory_raw),
             format = "file", 
             repository = "local"),
  tar_target(glw_directory_dataset, 
             create_data_directory(directory_path = "data/glw_dataset")),
  tar_target(glw_preprocessed, 
             preprocess_glw_data(glw_directory_dataset, glw_directory_raw, glw_downloaded, continent_raster_template)),

# ELEVATION -----------------------------------------------------------
tar_target(elevation_preprocessed, 
           get_elevation_data(output_dir = "data/elevation_dataset", 
                              output_filename = "africa_elevation.parquet",
                              raster_template = continent_raster_template),
           format = "file", 
           repository = "local"),

# BIOCLIM -----------------------------------------------------------
tar_target(bioclim_preprocessed,
           get_bioclim_data(output_dir = "data/bioclim_dataset", 
                            output_filename = "bioclim.parquet",
                            raster_template = continent_raster_template),
           format = "file", 
           repository = "local"),

# LANDCOVER -----------------------------------------------------------
tar_target(landcover_preprocessed,
           get_landcover_data(output_dir = "data/landcover_dataset", 
                              output_filename = "landcover.parquet",
                              raster_template = continent_raster_template),
           format = "file", 
           repository = "local"),
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
  
  # Map over year batch over day otherwise too many branches.
  tar_target(wahis_outbreak_history, get_daily_outbreak_history(dates_df = wahis_outbreak_dates,
                                                                wahis_rvf_outbreaks_preprocessed,
                                                                continent_raster_template,
                                                                continent_polygon,
                                                                country_polygons),
             head(wahis_outbreak_dates, 1),
             iteration = "vector",
             format = "file", 
             repository = "local"),
  
  tar_target(wahis_outbreak_history_recent_animation, get_outbreak_history_animation(input_files = c("data/outbreak_history_dataset/outbreak_history_recent_2007.tif"),
                                                                                     output_dir = "outputs",
                                                                                     output_filename = "outbreak_history_recent_2007.gif",
                                                                                     wahis_outbreak_history)), # Just included to enforce dependency with wahis_outbreak_history

  tar_target(wahis_outbreak_history_old_animation, get_outbreak_history_animation(input_files = c("data/outbreak_history_dataset/outbreak_history_old_2007.tif"),
                                                                                  output_dir = "outputs",
                                                                                  output_filename = "outbreak_history_old_2007.gif",
                                                                                  wahis_outbreak_history)), # Just included to enforce dependency with wahis_outbreak_history
  
  # SENTINEL NDVI -----------------------------------------------------------
  # 2018-present
  # 10 day period
  tar_target(sentinel_ndvi_raw_directory, 
             create_data_directory(directory_path = "data/sentinel_ndvi_raw")),
  tar_target(sentinel_ndvi_transformed_directory, 
             create_data_directory(directory_path = "data/sentinel_ndvi_transformed")),
  
  # get API parameters
  tar_target(sentinel_ndvi_api_parameters, get_sentinel_ndvi_api_parameters()), 
  
  # download files from source (locally)
  tar_target(sentinel_ndvi_downloaded, download_sentinel_ndvi(sentinel_ndvi_api_parameters,
                                                              download_directory = sentinel_ndvi_raw_directory,
                                                              overwrite = FALSE),
             pattern = sentinel_ndvi_api_parameters, 
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),
  
  # save raw to AWS bucket
  tar_target(sentinel_ndvi_raw_upload_aws_s3, {sentinel_ndvi_downloaded;
    aws_s3_upload_single_type(directory_path = sentinel_ndvi_raw_directory,
                              bucket =  aws_bucket ,
                              key = sentinel_ndvi_raw_directory, 
                              check = TRUE)}, 
    cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data
  
  # project to the template and save as parquets (these can now be queried for analysis)
  # this maintains the branches, saves separate files split by date
  # TODO NAs outside of the continent
  tar_target(sentinel_ndvi_transformed, 
             transform_sentinel_ndvi(sentinel_ndvi_downloaded, 
                                     continent_raster_template,
                                     sentinel_ndvi_transformed_directory,
                                     overwrite = FALSE),
             pattern = sentinel_ndvi_downloaded,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)), 
  
  # save transformed to AWS bucket
  tar_target(sentinel_ndvi_transformed_upload_aws_s3, 
             aws_s3_upload(path = sentinel_ndvi_transformed,
                           bucket =  aws_bucket,
                           key = sentinel_ndvi_transformed, 
                           check = TRUE), 
             pattern = sentinel_ndvi_transformed,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data
  
  # MODIS NDVI -----------------------------------------------------------
  # 2005-present
  # this satellite will be retired soon, so we should use sentinel for present dates 
  # 16 day period
  tar_target(modis_ndvi_raw_directory, 
             create_data_directory(directory_path = "data/modis_ndvi_raw")),
  tar_target(modis_ndvi_transformed_directory, 
             create_data_directory(directory_path = "data/modis_ndvi_transformed")),
  
  # get authorization token
  # this expires after 48 hours
  tar_target(modis_ndvi_token, get_modis_ndvi_token()),
  
  # set modis ndvi dates
  tar_target(modis_ndvi_start_year, 2005),
  tar_target(modis_ndvi_end_year, 2023),
  
  # set parameters and submit request for full continent
  tar_target(modis_ndvi_task_id_continent, submit_modis_ndvi_task_request_continent(modis_ndvi_start_year,
                                                                                    modis_ndvi_end_year,
                                                                                    modis_ndvi_token,
                                                                                    bbox_coords = continent_bounding_box)),
  # check if the request is posted, then get bundle
  # this uses a while loop to check every 30 seconds if the request is complete - it takes about 10 minutes
  # this function could be refactored to check time of modis_ndvi_task_request and pause for some time before submitting bundle request
  tar_target(modis_ndvi_bundle_request, submit_modis_ndvi_bundle_request(modis_ndvi_token, 
                                                                         modis_ndvi_task_id_continent, 
                                                                         timeout = 1500) |> rowwise() |> tar_group(),
             iteration = "group"
  ),
  
  # download files from source (locally)
  tar_target(modis_ndvi_downloaded, download_modis_ndvi(modis_ndvi_token,
                                                        modis_ndvi_bundle_request,
                                                        download_directory = modis_ndvi_raw_directory,
                                                        overwrite = FALSE),
             pattern = modis_ndvi_bundle_request, 
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),
  
  # save raw to AWS bucket
  tar_target(modis_ndvi_raw_upload_aws_s3, {modis_ndvi_downloaded;
    aws_s3_upload_single_type(directory_path = modis_ndvi_raw_directory,
                              bucket =  aws_bucket ,
                              key = modis_ndvi_raw_directory, 
                              check = TRUE)}, 
    cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data
  
  # remove the "quality" files
  tar_target(modis_ndvi_downloaded_subset, modis_ndvi_downloaded[str_detect(basename(modis_ndvi_downloaded), "NDVI")]),
  
  # project to the template and save as parquets (these can now be queried for analysis)
  # this maintains the branches, saves separate files split by date
  # TODO NAs outside of the continent
  tar_target(modis_ndvi_transformed, 
             transform_modis_ndvi(modis_ndvi_downloaded_subset, 
                                  continent_raster_template,
                                  modis_ndvi_transformed_directory,
                                  overwrite = FALSE),
             pattern = modis_ndvi_downloaded_subset,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)), 
  
  # save transformed to AWS bucket
  tar_target(modis_ndvi_transformed_upload_aws_s3,
             aws_s3_upload(path = modis_ndvi_transformed,
                           bucket =  aws_bucket,
                           key = modis_ndvi_transformed, 
                           check = TRUE), 
             pattern = modis_ndvi_transformed,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data 
  
  # NASA POWER recorded weather -----------------------------------------------------------
  # RH2M            MERRA-2 Relative Humidity at 2 Meters (%) ;
  # T2M             MERRA-2 Temperature at 2 Meters (C) ;
  # PRECTOTCORR     MERRA-2 Precipitation Corrected (mm/day)  
  tar_target(nasa_weather_raw_directory, 
             create_data_directory(directory_path = "data/nasa_weather_raw")),
  tar_target(nasa_weather_pre_transformed_directory, 
             create_data_directory(directory_path = "data/nasa_weather_pre_transformed")),
  tar_target(nasa_weather_transformed_directory, 
             create_data_directory(directory_path = "data/nasa_weather_transformed")),
  
  # set branching for nasa download
  tar_target(nasa_weather_years, 2005:2023),
  tar_target(nasa_weather_variables, c("RH2M", "T2M", "PRECTOTCORR")),
  tar_target(nasa_weather_coordinates, get_nasa_weather_coordinates(country_bounding_boxes)),
  
  #  download raw files
  tar_target(nasa_weather_downloaded,
             download_nasa_weather(nasa_weather_coordinates,
                                   nasa_weather_years,
                                   nasa_weather_variables,
                                   download_directory = nasa_weather_raw_directory,
                                   overwrite = FALSE),
             pattern = crossing(nasa_weather_years, nasa_weather_coordinates),
             format = "file",
             repository = "local",
             cue = tar_cue(tar_cue_general)),
  
  # save raw to AWS bucket
  tar_target(nasa_weather_raw_upload_aws_s3,  {nasa_weather_downloaded;
    aws_s3_upload_single_type(directory_path = nasa_weather_raw_directory,
                              bucket =  aws_bucket,
                              key = nasa_weather_raw_directory, 
                              check = TRUE)}, 
    cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data
  
  
  # remove dupes due to having overlapping country bounding boxes
  # save as arrow dataset, grouped by year
  tar_target(nasa_weather_pre_transformed, preprocess_nasa_weather(nasa_weather_downloaded,
                                                                   nasa_weather_pre_transformed_directory),
             repository = "local",
             cue = tar_cue(tar_cue_general)), 
  
  # project to the template and save as arrow dataset
  # TODO NAs outside of the continent
  tar_target(nasa_weather_transformed, 
             transform_nasa_weather(nasa_weather_pre_transformed,
                                    nasa_weather_transformed_directory, 
                                    continent_raster_template,
                                    overwrite = FALSE),
             pattern = nasa_weather_pre_transformed,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # save transformed to AWS bucket
  tar_target(nasa_weather_transformed_upload_aws_s3,  
             aws_s3_upload(path = nasa_weather_transformed,
                           bucket =  aws_bucket,
                           key = nasa_weather_transformed,
                           check = TRUE), 
             pattern = nasa_weather_transformed,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data
  
  # ECMWF Weather Forecast data -----------------------------------------------------------
  tar_target(ecmwf_forecasts_raw_directory, 
             create_data_directory(directory_path = "data/ecmwf_forecasts_raw")),
  tar_target(ecmwf_forecasts_transformed_directory, 
             create_data_directory(directory_path = "data/ecmwf_forecasts_transformed")),
  
  # set branching for ecmwf download
  tar_target(ecmwf_forecasts_api_parameters, set_ecmwf_api_parameter(years = 2005:2023,
                                                                     bbox_coords = continent_bounding_box,
                                                                     variables = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                                                     product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                                                     leadtime_months = c("1", "2", "3", "4", "5", "6"))),
  
  #  download files
  tar_target(ecmwf_forecasts_downloaded,
             download_ecmwf_forecasts(ecmwf_forecasts_api_parameters,
                                      download_directory = ecmwf_forecasts_raw_directory,
                                      overwrite = FALSE),
             pattern = ecmwf_forecasts_api_parameters,
             format = "file",
             repository = "local",
             cue = tar_cue(tar_cue_general)),
  
  # save raw to AWS bucket
  tar_target(ecmwf_forecasts_raw_upload_aws_s3,  {ecmwf_forecasts_downloaded;
    aws_s3_upload_single_type(directory_path = ecmwf_forecasts_raw_directory,
                              bucket =  aws_bucket ,
                              key = ecmwf_forecasts_raw_directory,
                              check = TRUE)},
    cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data
  
  # project to the template and save as arrow dataset
  # TODO NAs outside of the continent
  tar_target(ecmwf_forecasts_transformed, 
             transform_ecmwf_forecasts(ecmwf_forecasts_downloaded,
                                       ecmwf_forecasts_transformed_directory, 
                                       continent_raster_template,
                                       n_workers = 2,
                                       overwrite = FALSE),
             pattern = ecmwf_forecasts_downloaded,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # save transformed to AWS bucket
  # using aws.s3::put_object for multipart functionality
  tar_target(ecmwf_forecasts_transformed_upload_aws_s3, 
             aws.s3::put_object(file = ecmwf_forecasts_transformed, 
                                object = ecmwf_forecasts_transformed,
                                bucket = aws_bucket, 
                                multipart = TRUE,
                                verbose = TRUE,
                                show_progress = TRUE),
             pattern = ecmwf_forecasts_transformed,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data 

  

  # # cache locally
  # # Note the tar_read. When using AWS this does not read into R but instead initiates a download of the file into the scratch folder for later processing.
  # # Format file here means if we delete or change the local cache it will force a re-download.
  # tar_target(nasa_recorded_weather_local, {suppressWarnings(dir.create(here::here("data/nasa_parquets"), recursive = TRUE))
  #   cache_aws_branched_target(tmp_path = tar_read(nasa_recorded_weather_download),
  #                             ext = ".gz.parquet") 
  # },
  # repository = "local", 
  # format = "file"
  # ),


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
  
  # recorded weather anomalies --------------------------------------------------
  tar_target(weather_historical_means_directory, 
             create_data_directory(directory_path = "data/weather_historical_means")),
  
  tar_target(weather_historical_means, calculate_weather_historical_means(nasa_weather_transformed, # enforce dependency
                                                                          nasa_weather_transformed_directory,
                                                                          weather_historical_means_directory,
                                                                          days_of_year,
                                                                          lag_intervals,
                                                                          lead_intervals,
                                                                          overwrite = FALSE),
             pattern = days_of_year,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # save historical means to AWS bucket
  tar_target(weather_historical_means_upload_aws_s3, 
             aws_s3_upload(path = weather_historical_means,
                           bucket =  aws_bucket,
                           key = weather_historical_means, 
                           check = TRUE), 
             pattern = weather_historical_means,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data  
  
  
  tar_target(weather_anomalies_directory, 
             create_data_directory(directory_path = "data/weather_anomalies")),
  
  tar_target(weather_anomalies, calculate_weather_anomalies(nasa_weather_transformed,
                                                            nasa_weather_transformed_directory,
                                                            weather_historical_means,
                                                            weather_anomalies_directory,
                                                            model_dates_selected,
                                                            lag_intervals,
                                                            overwrite = TRUE),
             pattern = model_dates_selected,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # save anomalies to AWS bucket
  tar_target(weather_anomalies_upload_aws_s3, 
             aws_s3_upload(path = weather_anomalies,
                           bucket =  aws_bucket,
                           key = weather_anomalies, 
                           check = TRUE), 
             pattern = weather_anomalies,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data  
  
  
  # forecast weather anomalies ----------------------------------------------------------------------
  tar_target(forecasts_anomalies_directory, 
             create_data_directory(directory_path = "data/forecast_anomalies")),
  
  tar_target(forecasts_anomalies, calculate_forecasts_anomalies(ecmwf_forecasts_transformed,
                                                                ecmwf_forecasts_transformed_directory,
                                                                weather_historical_means,
                                                                forecasts_anomalies_directory,
                                                                model_dates_selected,
                                                                lead_intervals,
                                                                overwrite = FALSE),
             pattern = model_dates_selected,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)), 
  
  
  
  # save anomalies to AWS bucket
  tar_target(forecasts_anomalies_upload_aws_s3, 
             aws_s3_upload(path = forecasts_anomalies,
                           bucket =  aws_bucket,
                           key = forecasts_anomalies, 
                           check = TRUE), 
             pattern = forecasts_anomalies,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data  
  
  # compare forecast anomalies to actual data
  tar_target(forecasts_validate_directory, 
             create_data_directory(directory_path = "data/forecast_validation")),
  
  tar_target(forecasts_anomalies_validate, validate_forecasts_anomalies(forecasts_validate_directory,
                                                                        forecasts_anomalies,
                                                                        nasa_weather_transformed,
                                                                        weather_historical_means,
                                                                        model_dates_selected,
                                                                        lead_intervals,
                                                                        overwrite = FALSE),
             pattern = model_dates_selected,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)), 
  
  # save validation to AWS bucket
  tar_target(forecasts_anomalies_validate_upload_aws_s3, 
             aws_s3_upload(path = forecasts_anomalies_validate,
                           bucket =  aws_bucket,
                           key = forecasts_anomalies_validate, 
                           check = TRUE), 
             pattern = forecasts_anomalies_validate,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data  
  
  # ndvi anomalies --------------------------------------------------
  tar_target(ndvi_date_lookup, 
             create_ndvi_date_lookup(sentinel_ndvi_transformed,
                                     sentinel_ndvi_transformed_directory,
                                     modis_ndvi_transformed,
                                     modis_ndvi_transformed_directory)),
  
  
  tar_target(ndvi_historical_means_directory, 
             create_data_directory(directory_path = "data/ndvi_historical_means")),
  
  tar_target(ndvi_historical_means, calculate_ndvi_historical_means(ndvi_historical_means_directory,
                                                                    ndvi_date_lookup,
                                                                    days_of_year,
                                                                    lag_intervals,
                                                                    overwrite = FALSE),
             pattern = days_of_year,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  # save historical means to AWS bucket
  tar_target(ndvi_historical_means_upload_aws_s3, 
             aws_s3_upload(path = ndvi_historical_means,
                           bucket =  aws_bucket,
                           key = ndvi_historical_means, 
                           check = TRUE), 
             pattern = ndvi_historical_means,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data  
  
  
  tar_target(ndvi_anomalies_directory, 
             create_data_directory(directory_path = "data/ndvi_anomalies")),
  
  tar_target(ndvi_anomalies, calculate_ndvi_anomalies(ndvi_date_lookup,
                                                      ndvi_historical_means,
                                                      ndvi_anomalies_directory,
                                                      model_dates_selected,
                                                      lag_intervals,
                                                      overwrite = FALSE),
             pattern = model_dates_selected,
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  
  # save anomalies to AWS bucket
  tar_target(ndvi_anomalies_upload_aws_s3, 
             aws_s3_upload(path = ndvi_anomalies,
                           bucket =  aws_bucket,
                           key = ndvi_anomalies, 
                           check = TRUE), 
             pattern = ndvi_anomalies,
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data  
  
  # all anomalies --------------------------------------------------
  tar_target(augmented_data_directory, 
             create_data_directory(directory_path = "data/augmented_data")),
  
  tar_target(augmented_data, 
             augment_data(weather_anomalies, 
                          forecasts_anomalies, 
                          ndvi_anomalies, 
                          augmented_data_directory),
             format = "file", 
             repository = "local",
             cue = tar_cue(tar_cue_general)),  
  
  tar_target(augmented_data_upload_aws_s3,
             aws_s3_upload(path = augmented_data,
                           bucket =  aws_bucket,
                           key = augmented_data,
                           check = TRUE),
             cue = tar_cue(tar_cue_upload_aws)), # only run this if you need to upload new data
  
)

# Model -----------------------------------------------------------
model_targets <- tar_plan(
  
  # RSA --------------------------------------------------
  tar_target(augmented_data_rsa_directory, 
             create_data_directory(directory_path = "data/augmented_data_rsa")),
  
  tar_target(aggregated_data_rsa,
             aggregate_augmented_data_by_adm(augmented_data, 
                                             rsa_polygon, 
                                             model_dates_selected),
             pattern = model_dates_selected
  ),
  
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
  tar_render(readme, path = "README.Rmd")
)


# List targets -----------------------------------------------------------------
all_targets()
