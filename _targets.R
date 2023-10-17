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

tar_option_set(resources = tar_resources(
  aws = tar_resources_aws(bucket = Sys.getenv("AWS_BUCKET_ID"), prefix = "_targets"),
  qs = tar_resources_qs(preset = "fast")),
  repository = "aws",
  format = "qs",
  error = "null", # allow branches to error without stopping the pipeline
  workspace_on_error = TRUE # allows interactive session for failed branches
)

# future::plan(future::multisession, workers = 16)

# Static Data Download ----------------------------------------------------
static_targets <- tar_plan(
  
  # Define country bounding boxes and years to set up download ----------------------------------------------------
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
  
)

# Dynamic Data Download -----------------------------------------------------------
dynamic_targets <- tar_plan(
  
  # WAHIS -----------------------------------------------------------
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks_preprocessed, 
             preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw)),
  
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
             repository = "local"),
  
  # save raw to AWS bucket
  tar_target(sentinel_ndvi_raw_upload_aws_s3, {sentinel_ndvi_downloaded;
    aws_s3_upload_single_type(directory_path = sentinel_ndvi_raw_directory,
                              bucket =  aws_bucket ,
                              key = sentinel_ndvi_raw_directory, 
                              check = TRUE)}, 
    cue = tar_cue("never")), # only run this if you need to upload new data
  
  # project to the template and save as parquets (these can now be queried for analysis)
  # this maintains the branches, saves separate files split by date
  tar_target(sentinel_ndvi_transformed, 
             transform_sentinel_ndvi(sentinel_ndvi_downloaded, 
                                     continent_raster_template,
                                     sentinel_ndvi_transformed_directory,
                                     overwrite = FALSE),
             pattern = sentinel_ndvi_downloaded,
             format = "file", 
             repository = "local"), 
  
  # save transformed to AWS bucket
  tar_target(sentinel_ndvi_transformed_upload_aws_s3, 
             aws_s3_upload(path = sentinel_ndvi_transformed,
                           bucket =  aws_bucket,
                           key = sentinel_ndvi_transformed, 
                           check = TRUE), 
             pattern = sentinel_ndvi_transformed,
             cue = tar_cue("never")), # only run this if you need to upload new data
  
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
             repository = "local"),
  
  # save raw to AWS bucket
  tar_target(modis_ndvi_raw_upload_aws_s3, {modis_ndvi_downloaded;
    aws_s3_upload_single_type(directory_path = modis_ndvi_raw_directory,
                              bucket =  aws_bucket ,
                              key = modis_ndvi_raw_directory, 
                              check = TRUE)}, 
    cue = tar_cue("never")), # only run this if you need to upload new data
  
  # remove the "quality" files
  tar_target(modis_ndvi_downloaded_subset, modis_ndvi_downloaded[str_detect(basename(modis_ndvi_downloaded), "NDVI")]),
  
  # project to the template and save as parquets (these can now be queried for analysis)
  # this maintains the branches, saves separate files split by date
  tar_target(modis_ndvi_transformed, 
             transform_modis_ndvi(modis_ndvi_downloaded_subset, 
                                  continent_raster_template,
                                  modis_ndvi_transformed_directory,
                                  overwrite = FALSE),
             pattern = modis_ndvi_downloaded_subset,
             format = "file", 
             repository = "local"), 
  
  # save transformed to AWS bucket
  tar_target(modis_ndvi_transformed_upload_aws_s3,
             aws_s3_upload(path = modis_ndvi_transformed,
                           bucket =  aws_bucket,
                           key = modis_ndvi_transformed, 
                           check = TRUE), 
             pattern = modis_ndvi_transformed,
             cue = tar_cue("never")), # only run this if you need to upload new data 
  
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
             repository = "local"),
  
  # save raw to AWS bucket
  tar_target(nasa_weather_raw_upload_aws_s3,  {nasa_weather_downloaded;
    aws_s3_upload_single_type(directory_path = nasa_weather_raw_directory,
                              bucket =  aws_bucket,
                              key = nasa_weather_raw_directory, 
                              check = TRUE)}, 
    cue = tar_cue("never")), # only run this if you need to upload new data
  
  
  # remove dupes due to having overlapping country bounding boxes
  # save as arrow dataset, grouped by year
  tar_target(nasa_weather_pre_transformed, preprocess_nasa_weather(nasa_weather_downloaded,
                                                                   nasa_weather_pre_transformed_directory),
             repository = "local"), 
  
  # project to the template and save as arrow dataset
  tar_target(nasa_weather_transformed, 
             transform_nasa_weather(nasa_weather_pre_transformed,
                                    nasa_weather_transformed_directory, 
                                    continent_raster_template,
                                    overwrite = FALSE),
             pattern = nasa_weather_pre_transformed,
             format = "file", 
             repository = "local"),  
  
  # save transformed to AWS bucket
  tar_target(nasa_weather_transformed_upload_aws_s3,  
             aws_s3_upload(path = nasa_weather_transformed,
                           bucket =  aws_bucket,
                           key = nasa_weather_transformed,
                           check = TRUE), 
             pattern = nasa_weather_transformed,
             cue = tar_cue("never")), # only run this if you need to upload new data
  
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
             repository = "local"),
  
  # save raw to AWS bucket
  tar_target(ecmwf_forecasts_raw_upload_aws_s3,  {ecmwf_forecasts_downloaded;
    aws_s3_upload_single_type(directory_path = ecmwf_forecasts_raw_directory,
                              bucket =  aws_bucket ,
                              key = ecmwf_forecasts_raw_directory,
                              check = TRUE)},
    cue = tar_cue("never")), # only run this if you need to upload new data
  
  # project to the template and save as arrow dataset
  tar_target(ecmwf_forecasts_transformed, 
             transform_ecmwf_forecasts(ecmwf_forecasts_downloaded,
                                       ecmwf_forecasts_transformed_directory, 
                                       continent_raster_template,
                                       n_workers = 2,
                                       overwrite = FALSE),
             pattern = ecmwf_forecasts_downloaded,
             format = "file", 
             repository = "local"),  
  
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
             cue = tar_cue("never")), # only run this if you need to upload new data 
  
)

# Data Processing -----------------------------------------------------------
data_targets <- tar_plan(
  
  tar_target(lag_intervals, c(30, 60, 90)), 
  tar_target(days_of_year, 1:366),
  tar_target(model_dates, set_model_dates(start_year = 2005, end_year = 2022, n_per_month = 2, lag_intervals, seed = 212)),
  tar_target(model_dates_selected, model_dates |> filter(select_date) |> pull(date)),
  
  # weather data
  tar_target(weather_historical_means_directory, 
             create_data_directory(directory_path = "data/weather_historical_means")),
  
  tar_target(weather_historical_means, calculate_weather_historical_means(nasa_weather_transformed, # enforce dependency
                                                                          nasa_weather_transformed_directory,
                                                                          weather_historical_means_directory,
                                                                          days_of_year,
                                                                          overwrite = FALSE),
             pattern = days_of_year,
             format = "file", 
             repository = "local"),  
  
  tar_target(weather_anomalies_directory, 
             create_data_directory(directory_path = "data/weather_anomalies")),
  
  tar_target(weather_anomalies, calculate_weather_anomalies(nasa_weather_transformed,
                                                            nasa_weather_transformed_directory,
                                                            weather_historical_means,
                                                            weather_anomalies_directory,
                                                            model_dates,
                                                            model_dates_selected,
                                                            lag_intervals,
                                                            overwrite = FALSE),
             pattern = model_dates_selected,
             format = "file", 
             repository = "local"),  
  
  # save anomalies to AWS bucket
  tar_target(weather_anomalies_upload_aws_s3, 
             aws_s3_upload(path = weather_anomalies,
                           bucket =  aws_bucket,
                           key = weather_anomalies, 
                           check = TRUE), 
             pattern = weather_anomalies,
             cue = tar_cue("never")), # only run this if you need to upload new data  
  
)

# Model -----------------------------------------------------------
model_targets <- tar_plan(
  
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
