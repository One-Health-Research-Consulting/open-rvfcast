# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

aws_bucket = Sys.getenv("AWS_BUCKET_ID")

# Targets options
tar_option_set(resources = tar_resources(
  aws = tar_resources_aws(bucket = aws_bucket, prefix = "open-rvfcast/_targets"),
  qs = tar_resources_qs(preset = "fast")),
  repository = "aws",
  format = "qs",
  error = "null", # allow branches to error without stopping the pipeline
  workspace_on_error = TRUE # allows interactive session for failed branches
)

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
  tar_target(continent_bounding_box, sf::st_bbox(continent_polygon))
  
)

# Dynamic Data Download -----------------------------------------------------------
dynamic_targets <- tar_plan(
  
  # WAHIS -----------------------------------------------------------
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks_preprocessed, 
             preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw)),
  
  # SENTINEL NDVI -----------------------------------------------------------
  # 2018-present
  
  tar_target(sentinel_ndvi_directory, "data/sentinel_ndvi_rasters"),
  
  # get API parameters
  tar_target(sentinel_ndvi_api_parameters, get_sentinel_ndvi_api_parameters(), cue = tar_cue("thorough")), 
  
  # download files from source (locally)
  tar_target(sentinel_ndvi_downloaded, download_sentinel_ndvi(sentinel_ndvi_api_parameters,
                                                              download_directory = sentinel_ndvi_directory),
             pattern = sentinel_ndvi_api_parameters, 
             format = "file", 
             repository = "local",
             cue = tar_cue("thorough")),
  
  # save to AWS bucket
  tar_target(sentinel_ndvi_upload_aws_s3, {sentinel_ndvi_downloaded; # enforce dependency
    aws_s3_upload(path = sentinel_ndvi_directory,
                  bucket =  aws_bucket ,
                  key = sentinel_ndvi_directory, 
                  prefix = "open-rvfcast/",
                  check = TRUE)}, 
    cue = tar_cue("thorough")), 
  
  # user can download from AWS (instead of going through the source)
  # tar_target(sentinel_ndvi_download_aws_s3, aws_s3_download(path = sentinel_ndvi_directory,
  #                                                           bucket = aws_bucket ,
  #                                                           key = paste0("open-rvfcast/", sentinel_ndvi_directory), 
  #                                                           check = TRUE),
  #            cue = tar_cue("never")), 
  
  # MODIS NDVI -----------------------------------------------------------
  # 2005-present
  # this satellite will be retired soon, so we should use sentinel for present dates 
  
  tar_target(modis_ndvi_directory, "data/modis_ndvi_rasters"),
  
  # set branching for modis
  tar_target(modis_ndvi_years, 2005:2018),
  
  # download files
  tar_target(modis_ndvi_downloaded, download_modis_ndvi(continent_bounding_box,
                                                        modis_ndvi_years,
                                                        download_directory = modis_ndvi_directory),
             pattern = modis_ndvi_years, 
             format = "file" , 
             repository = "local",
             cue = tar_cue("thorough")),
  
  # save to AWS bucket
  tar_target(modis_ndvi_upload_aws_s3, {modis_ndvi_downloaded; # enforce dependency
    aws_s3_upload(path = modis_ndvi_directory,
                  bucket = aws_bucket ,
                  key = modis_ndvi_directory, 
                  prefix = "open-rvfcast/",
                  check = TRUE)}, 
    cue = tar_cue("thorough")), 
  
  # user can download from AWS (instead of going through the source)
  # tar_target(modis_ndvi_download_aws_s3, aws_s3_download(path = modis_ndvi_directory,
  #                                                        bucket = aws_bucket ,
  #                                                        key = paste0("open-rvfcast/", modis_ndvi_directory), 
  #                                                        check = TRUE),
  #            cue = tar_cue("never")), 
  
  
  # NASA POWER recorded weather -----------------------------------------------------------
  
  tar_target(nasa_weather_directory, "data/nasa_weather_parquets"),
  
  # set branching for nasa
  tar_target(nasa_weather_years, 2005:2023),
  tar_target(nasa_weather_variables, c("RH2M", "T2M", "PRECTOTCORR")),
  tar_target(nasa_weather_coordinates, get_nasa_weather_coordinates(country_bounding_boxes)),
  
  #  download files
  tar_target(nasa_weather_downloaded,
             download_nasa_weather(nasa_weather_coordinates,
                                   nasa_weather_years,
                                   nasa_weather_variables,
                                   download_directory = nasa_weather_directory),
             pattern = crossing(nasa_weather_years, nasa_weather_coordinates),
             format = "file",
             repository = "local",
             cue = tar_cue("thorough")
  ),
  
  # save to AWS bucket
  tar_target(nasa_weather_upload_aws_s3,  {nasa_weather_downloaded; # enforce dependency
    aws_s3_upload(path = nasa_weather_directory,
                  bucket =  aws_bucket ,
                  key = nasa_weather_directory, 
                  prefix = "open-rvfcast/",
                  check = TRUE)}, 
    cue = tar_cue("thorough")), 
  
  # user can download from AWS (instead of going through the source)
  # tar_target(nasa_weather_download_aws_s3, aws_s3_download(path = nasa_weather_directory,
  #                                                           bucket = aws_bucket ,
  #                                                           key = paste0("open-rvfcast/", nasa_weather_directory), 
  #                                                           check = TRUE),
  #            cue = tar_cue("never")), 
  
  # ECMWF Weather Forecast data -----------------------------------------------------------
  
  tar_target(ecmwf_forecasts_directory, "data/ecmwf_forecasts_gribs"),
  
  # set branching for ecmwf
  tar_target(ecmwf_api_parameters, set_ecmwf_api_parameter(years = 2005:2023,
                                                           bbox_coords = continent_bounding_box,
                                                           variables = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                                           product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                                           leadtime_months = c("1", "2", "3", "4", "5", "6"))),
  
  #  download files
  tar_target(ecmwf_forecasts_downloaded,
             download_ecmwf_forecasts(ecmwf_api_parameters,
                                      download_directory = ecmwf_forecasts_directory),
             pattern = ecmwf_api_parameters,
             format = "file",
             repository = "local",
             cue = tar_cue("thorough")
  ),
  
  # save to AWS bucket
  tar_target(ecmwf_forecasts_upload_aws_s3,  {ecmwf_forecasts_downloaded; # enforce dependency
    aws_s3_upload(path = ecmwf_forecasts_directory,
                  bucket =  aws_bucket ,
                  key = ecmwf_forecasts_directory, 
                  prefix = "open-rvfcast/",
                  check = TRUE)}, 
    cue = tar_cue("thorough")), 
  
)

# Data Processing -----------------------------------------------------------
data_targets <- tar_plan(
  
  # convert ecmwf to raster
  
  # convert terra power to raster
  
  
  # tar_target(ecmwf_forecasts_preprocessed,
  #            preprocess_ecmwf_forecasts(ecmwf_forecasts_download,
  #                                       preprocessed_directory =  "data/ecmwf_parquets"),
  #            pattern = map(ecmwf_forecasts_download), 
  #            iteration = "list",
  #            format = "file" 
  # ),
  
  
  # resampling
  
  # merge data together
  
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

# List targets -----------------------------------------------------------------

list(
  static_targets,
  dynamic_targets,
  data_targets,
  model_targets,
  deploy_targets,
  plot_targets,
  report_targets,
  test_targets
)
