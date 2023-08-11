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
             wrap(terra::rast(ext(continent_polygon), 
                              resolution = 0.5))), #TODO change to 0.1 (might cause error in transform, leaving at 0.5 for now)
  # nasa power resolution = 0.5; enmwf = ; ndvi = 
  # tar_target(continent_raster_template_plot, create_raster_template_plot(rast(continent_raster_template), continent_polygon))
  
)

# Dynamic Data Download -----------------------------------------------------------
dynamic_targets <- tar_plan(
  
  # WAHIS -----------------------------------------------------------
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks_preprocessed, 
             preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw)),
  
  # SENTINEL NDVI -----------------------------------------------------------
  # 2018-present
  
  tar_target(sentinel_ndvi_directory_raw, 
             create_data_directory(directory_path = "data/sentinel_ndvi_raw")),
  tar_target(sentinel_ndvi_directory_dataset, 
             create_data_directory(directory_path = "data/sentinel_ndvi_dataset")),
  
  # get API parameters
  tar_target(sentinel_ndvi_api_parameters, get_sentinel_ndvi_api_parameters(), cue = tar_cue("thorough")), 
  
  # download files from source (locally)
  tar_target(sentinel_ndvi_downloaded, download_sentinel_ndvi(sentinel_ndvi_api_parameters,
                                                              download_directory = sentinel_ndvi_directory_raw,
                                                              overwrite = FALSE),
             pattern = sentinel_ndvi_api_parameters, 
             format = "file", 
             repository = "local",
             cue = tar_cue("thorough")),
  
  # save raw to AWS bucket
  tar_target(sentinel_ndvi_raw_upload_aws_s3, {sentinel_ndvi_downloaded; # enforce dependency
    aws_s3_upload(path = sentinel_ndvi_directory_raw,
                  bucket =  aws_bucket ,
                  key = sentinel_ndvi_directory_raw, 
                  check = TRUE)}, 
    cue = tar_cue("thorough")), 
  
  # project to the template and save as parquets (these can now be queried for analysis)
  tar_target(sentinel_ndvi_dataset, 
             create_sentinel_ndvi_dataset(sentinel_ndvi_downloaded, 
                                          continent_raster_template,
                                          sentinel_ndvi_directory_dataset,
                                          overwrite = FALSE),
             pattern = sentinel_ndvi_downloaded,
             format = "file", 
             repository = "local",
             cue = tar_cue("thorough")), 
  
  # save transformed to AWS bucket
  tar_target(sentinel_ndvi_dataset_upload_aws_s3,  {sentinel_ndvi_dataset; # enforce dependency
    aws_s3_upload(path = sentinel_ndvi_directory_dataset,
                  bucket =  aws_bucket,
                  key = sentinel_ndvi_directory_dataset, 
                  check = TRUE)}, 
    cue = tar_cue("thorough")), 
  
  # MODIS NDVI -----------------------------------------------------------
  # 2005-present
  # this satellite will be retired soon, so we should use sentinel for present dates 
  
  # tar_target(modis_ndvi_directory, "data/modis_ndvi_rasters"),
  
  # set branching for modis
  # tar_target(modis_ndvi_years, 2005:2018),
  
  # download files
  # TODO refactor so that it can skip files without calling the stec api
  # tar_target(modis_ndvi_downloaded, download_modis_ndvi(continent_bounding_box,
  #                                                       modis_ndvi_years,
  #                                                       download_directory = modis_ndvi_directory),
  #            pattern = modis_ndvi_years, 
  #            format = "file" , 
  #            repository = "local",
  #            cue = tar_cue("thorough")),
  
  # save to AWS bucket
  # tar_target(modis_ndvi_upload_aws_s3, {modis_ndvi_downloaded; # enforce dependency
  #   aws_s3_upload(path = modis_ndvi_directory,
  #                 bucket = aws_bucket ,
  #                 key = modis_ndvi_directory, 
  #                 check = TRUE)}, 
  #   cue = tar_cue("thorough")), 
  
  # TODO what are the units (differs between sentinel and modis)
  # TODO transform needs to handle the internal batching from modis (tar_rep?)
  
  
  # NASA POWER recorded weather -----------------------------------------------------------
  # RH2M            MERRA-2 Relative Humidity at 2 Meters (%) ;
  # T2M             MERRA-2 Temperature at 2 Meters (C) ;
  # PRECTOTCORR     MERRA-2 Precipitation Corrected (mm/day)  
  
  tar_target(nasa_weather_directory_raw, 
             create_data_directory(directory_path = "data/nasa_weather_raw")),
  tar_target(nasa_weather_directory_dataset, 
             create_data_directory(directory_path = "data/nasa_weather_dataset")),
  
  # set branching for nasa
  tar_target(nasa_weather_years, 2005:2023),
  tar_target(nasa_weather_variables, c("RH2M", "T2M", "PRECTOTCORR")),
  tar_target(nasa_weather_coordinates, get_nasa_weather_coordinates(country_bounding_boxes)),
  
  #  download raw files
  tar_target(nasa_weather_downloaded,
             download_nasa_weather(nasa_weather_coordinates,
                                   nasa_weather_years,
                                   nasa_weather_variables,
                                   download_directory = nasa_weather_directory_raw,
                                   overwrite = FALSE),
             pattern = crossing(nasa_weather_years, nasa_weather_coordinates),
             iteration = "vector",
             format = "file",
             repository = "local",
             cue = tar_cue("thorough")
  ),
  
  # save raw to AWS bucket
  tar_target(nasa_weather_raw_upload_aws_s3,  {nasa_weather_downloaded; # enforce dependency
    aws_s3_upload(path = nasa_weather_directory_raw,
                  bucket =  aws_bucket ,
                  key = nasa_weather_directory_raw, 
                  check = TRUE)}, 
    cue = tar_cue("thorough")), 
  
  # remove dupes due to having overlapping country bounding boxes
  # project to the template and save as arrow dataset
  tar_target(nasa_weather_dataset, 
             create_nasa_weather_dataset(nasa_weather_downloaded,
                                         nasa_weather_directory_dataset, 
                                         continent_raster_template,
                                         overwrite = FALSE),
             format = "file", 
             repository = "local",
             cue = tar_cue("thorough")),  
  
  # save dataset to AWS bucket
  tar_target(nasa_weather_dataset_upload_aws_s3,  {nasa_weather_dataset; # enforce dependency
    aws_s3_upload(path = nasa_weather_directory_dataset,
                  bucket =  aws_bucket,
                  key = nasa_weather_directory_dataset, 
                  check = TRUE)}, 
    cue = tar_cue("thorough")),    
  
  # ECMWF Weather Forecast data -----------------------------------------------------------
  
  # tar_target(ecmwf_forecasts_directory, "data/ecmwf_forecasts_gribs"),
  # 
  # # set branching for ecmwf
  # tar_target(ecmwf_api_parameters, set_ecmwf_api_parameter(years = 2005:2023,
  #                                                          bbox_coords = continent_bounding_box,
  #                                                          variables = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
  #                                                          product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
  #                                                          leadtime_months = c("1", "2", "3", "4", "5", "6"))),
  # 
  # #  download files
  # tar_target(ecmwf_forecasts_downloaded,
  #            download_ecmwf_forecasts(ecmwf_api_parameters,
  #                                     download_directory = ecmwf_forecasts_directory),
  #            pattern = ecmwf_api_parameters,
  #            format = "file",
  #            repository = "local",
  #            cue = tar_cue("thorough")
  # ),
  # 
  # # save to AWS bucket
  # tar_target(ecmwf_forecasts_upload_aws_s3,  {ecmwf_forecasts_downloaded; # enforce dependency
  #   aws_s3_upload(path = ecmwf_forecasts_directory,
  #                 bucket =  aws_bucket ,
  #                 key = ecmwf_forecasts_directory, 
  #                 check = TRUE)}, 
  #   cue = tar_cue("thorough")), 
  
  # transform
  # make raster stacks of all the data, transform, convert back to parquets
  # tar_target(ecmwf_forecasts_flat_transformed,
  #            save_transform_ecmwf_grib(ecmwf_forecasts_downloaded,
  #                                      transform_directory = paste0(str_replace(ecmwf_forecasts_directory, "gribs", "flat"), "_transformed"),
  #                                      verbose = TRUE),
  #            pattern = map(ecmwf_forecasts_downloaded),
  #            iteration = "list",
  #            format = "file", 
  #            repository = "local",
  #            cue = tar_cue("thorough")),  
  
  
)

# Data Processing -----------------------------------------------------------
data_targets <- tar_plan(
  
  # tar_target(model_dates_random_select, random_select_model_dates(start_year = 2005, end_year = 2022, n_per_month = 2, seed = 212)),
  
  # TODO take nasa_weather_directory_dataset and do full lag calcs in this function using duckdb, then collect into memory
  # tar_target(weather_data, process_weather_data(nasa_weather_directory_dataset, nasa_weather_dataset)),
  # tar_target(ndvi_data, process_ndvi_data(sentinel_ndvi_directory_dataset, sentinel_ndvi_dataset, model_dates_random_select))
  
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
