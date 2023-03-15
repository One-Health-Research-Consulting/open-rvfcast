# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

# Targets options
tar_option_set(resources = tar_resources(
  aws = tar_resources_aws(bucket = Sys.getenv("AWS_BUCKET_ID"), prefix = "open-rvfcast"),
  qs = tar_resources_qs(preset = "fast")),
  repository = "aws",
  format = "qs"
)

# How many parallel processes?
n_workers <- 4

# Data Source Download -----------------------------------------------------------
source_targets <- tar_plan(
  
  tar_target(country_regions, define_country_regions()),
  tar_target(bounding_boxes, define_bounding_boxes(country_regions)),
  
  ## wahis
  # TODO can refactor to download with dynamic branching
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks_preprocessed, 
             preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw, country_regions)),
  
  ## ecmwf
  tar_target(ecmwf_api_parameters, set_ecmwf_api_parameter(bounding_boxes) |> 
               rowwise() |> 
               tar_group(),
             iteration = "group"), 
  
  tar_target(ecmwf_forecasts_download, 
             download_ecmwf_forecasts(parameters = ecmwf_api_parameters,
                                      variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                      product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                      leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                      download_directory = "data/ecmwf_gribs"),
             pattern = map(ecmwf_api_parameters), 
             iteration = "list"
  ),
  
  
  tar_target(ecmwf_forecasts_preprocessed,
             preprocess_ecmwf_forecasts(ecmwf_forecasts_download,
                                        preprocessed_directory =  "data/ecmwf_parquets"),
             pattern = map(ecmwf_forecasts_download), 
             iteration = "list",
             format = "file" 
  ),
  
  
  # cache locally
  # Note the tar_read. When using AWS this does not read into R but instead initiates a download of the file into the scratch folder for later processing.
  # Format file here means if we delete or change the local cache it will force a re-download.
  tar_target(ecmwf_forecasts_preprocessed_local, {suppressWarnings(dir.create(here::here("data/ecmwf_parquets"), recursive = TRUE))
    cache_aws_branched_target(tmp_path = tar_read(ecmwf_forecasts_preprocessed),
                              ext = ".gz.parquet",
                              cleanup = FALSE) # setting cleanup to false doesn't work - targets will still remove the non-cache files
  },
  repository = "local", 
  format = "file"
  ),
  
  ## NASA Power
  tar_target(nasa_api_parameters, set_nasa_api_parameter(bounding_boxes) |> 
               rowwise() |> 
               tar_group(),
             iteration = "group"), 
  
  # here we save downloads as parquets - no preprocessing required
  tar_target(nasa_recorded_download, 
             download_nasa_recorded_weather(parameters = nasa_api_parameters,
                                            variable  = c("RH2M", "T2M", "PRECTOTCORR"),
                                            timestep = "daily",
                                            download_directory = "data/nasa_parquets"),
             pattern = map(nasa_api_parameters), 
             iteration = "list",
             format = "file" 
  )
  
)

# Data Processing -----------------------------------------------------------
data_targets <- tar_plan(
  
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
  source_targets,
  data_targets,
  model_targets,
  deploy_targets,
  plot_targets,
  report_targets,
  test_targets
)
