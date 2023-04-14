# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

# Targets options
tar_option_set(resources = tar_resources(
  aws = tar_resources_aws(bucket = Sys.getenv("AWS_BUCKET_ID"), prefix = "open-rvfcast"),
  qs = tar_resources_qs(preset = "fast")),
  repository = "aws",
  format = "qs",
  error = "null", # allow branches to error without stopping the pipeline
  workspace_on_error = TRUE # allows interactive session for failed branches
)

# How many parallel processes for tar_make_future? (for within branch parallelization, set .env var N_PARALLEL_CORES)
# future::plan(future.callr::callr, workers = 4)

# Static Data Download ----------------------------------------------------
static_targets <- tar_plan(
  
  # Define country bounding boxes and years to set up download ----------------------------------------------------
  tar_target(country_polygons, create_country_polygons(countries =  c("Libya", "Kenya", "South Africa",
                                                                      "Mauritania", "Niger", "Namibia",
                                                                      "Madagascar", "Eswatini", "Botswana" ,
                                                                      "Mali", "United Republic of Tanzania", 
                                                                      "Chad","Sudan", "Senegal"),
                                                       states = tibble(state = "Mayotte", country = "France"))),
  tar_target(country_bounding_boxes, get_country_bounding_boxes(country_polygons)),
  tar_target(country_bounding_boxes_years, expand_grid(country_bounding_boxes, year = 2005:2023))
)

# Dynamic Data Download -----------------------------------------------------------
dynamic_targets <- tar_plan(
  
  # WAHIS -----------------------------------------------------------
  # TODO refactor with flatfiles
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks_preprocessed, 
             preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw, country_regions)),
  
  # SENTINEL NDVI -----------------------------------------------------------
  # 2018-present
  # S3A and S3B satellites?
  # They are overlapping orbits, offset by 140 deg, which is useful for realtime images
  # but not necessary for our timestep 
  # pretty sure it's okay to select just one, but we can confirm with Assaf
  
  # get API parameters
  # files are for full Africa
  tar_target(sentinel_ndvi_api_parameters, get_sentinel_ndvi_api_parameters()), 
  
  # download files
  tar_target(sentinel_ndvi_downloaded, download_sentinel_ndvi(sentinel_ndvi_api_parameters,
                                                              download_directory = "data/sentinel_ndvi_rasters"),
             pattern = sentinel_ndvi_api_parameters, 
             format = "file", 
             repository = "local"),
  
  # MODIS NDVI -----------------------------------------------------------
  # 2005-present
  # this satellite will be retired soon, so we should use sentinel for present dates 
  
  # set country/year branching for modis
  tar_target(modis_country_bounding_boxes_years, country_bounding_boxes_years |> 
               filter(year <= 2018)), 
  
  # download files
  tar_target(modis_ndvi_downloaded, download_modis_ndvi(modis_country_bounding_boxes_years,
                                                        download_directory = "data/modis_ndvi_rasters"),
             pattern = tail(modis_country_bounding_boxes_years, 1), 
             format = "file" , 
             repository = "local"),
  
  # NASA POWER recorded weather -----------------------------------------------------------
  # TODO this needs to be refactored to pull terra data
  
  # get API parameters
  tar_target(nasa_api_parameters, 
             set_nasa_api_parameter(bounding_boxes, 
                                    start_year = 2005,
                                    variables  = c("RH2M", "T2M", "PRECTOTCORR")) |> 
               group_by(year, region) |> 
               tar_group(),
             iteration = "group"), 
  
  # download files
  # here we save downloads as parquets - no preprocessing required
  tar_target(nasa_recorded_weather_download, 
             download_nasa_recorded_weather(parameters = nasa_api_parameters,
                                            download_directory = "data/nasa_parquets"),
             pattern = map(nasa_api_parameters), 
             iteration = "list",
             format = "file" 
  ),
  
  # cache locally
  tar_target(nasa_recorded_weather_local, {suppressWarnings(dir.create(here::here("data/nasa_parquets"), recursive = TRUE))
    cache_aws_branched_target(tmp_path = tar_read(nasa_recorded_weather_download),
                              ext = ".gz.parquet") 
  },
  repository = "local", 
  format = "file"
  ),
  
  # ECMWF Weather Forecast data -----------------------------------------------------------
  # TODO refactoring based on Noam's PR
  # TODO download from 2005-present
  
  # tar_target(ecmwf_api_parameters, set_ecmwf_api_parameter(bounding_boxes) |> 
  #              rowwise() |> 
  #              tar_group(),
  #            iteration = "group"), 
  # 
  # tar_target(ecmwf_forecasts_download, 
  #            download_ecmwf_forecasts(parameters = ecmwf_api_parameters,
  #                                     variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
  #                                     product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
  #                                     leadtime_month = c("1", "2", "3", "4", "5", "6"),
  #                                     download_directory = "data/ecmwf_gribs"),
  #            pattern = map(ecmwf_api_parameters), 
  #            iteration = "list"
  # ),
  # 
  # 
  # tar_target(ecmwf_forecasts_preprocessed,
  #            preprocess_ecmwf_forecasts(ecmwf_forecasts_download,
  #                                       preprocessed_directory =  "data/ecmwf_parquets"),
  #            pattern = map(ecmwf_forecasts_download), 
  #            iteration = "list",
  #            format = "file" 
  # ),
  # 
  # 
  # # cache locally
  # tar_target(ecmwf_forecasts_preprocessed_local, {suppressWarnings(dir.create(here::here("data/ecmwf_parquets"), recursive = TRUE))
  #   cache_aws_branched_target(tmp_path = tar_read(ecmwf_forecasts_preprocessed),
  #                             ext = ".gz.parquet") # setting cleanup to false doesn't work - targets will still remove the non-cache files
  # },
  # repository = "local", 
  # format = "file"
  # ),
  
  
)

# Data Processing -----------------------------------------------------------
data_targets <- tar_plan(
  
  # resampling
  
  # merge data together
  
)

# Model -----------------------------------------------------------
model_targets <- tar_plan(
  
  # model_data = prep_model_data(case_data, rast(static_stack)),
  # spatial_grid = create_spatial_grid(model_data),
  # blocked_data = create_blocked_model_data(model_data, spatial_grid),
  # divided_data = divide_data(blocked_data),
  # holdout_data = assessment(divided_data),
  # training_data = analysis(divided_data),
  # training_splits = create_training_splits(training_data),
  # model_workflow = build_workflow(training_data),
  # tuned_parameters = cv_tune_parameters(model_workflow, training_splits, grid_size = 10, n_cores = 4),
  # tuned_model = fit_full_model(model_workflow, training_data, tuned_parameters),
  # tar_file(tuned_model_file, {f <- "tuned_model_file.rds"; saveRDS(tuned_model, f, compress = "xz"); f}),
  # holdout_data_predictions = predict_rvf(holdout_data, tuned_model),
  # confusion_matrix = get_confusion_matrix(holdout_data_predictions),
  # model_performance = summary(confusion_matrix),
  # dalex_explainer = get_dalex_explainer(tuned_model)
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
