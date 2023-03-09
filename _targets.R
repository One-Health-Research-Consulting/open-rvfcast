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
nproc <- 4

# Short term task tracking

# TODO current
# alternate recorded weather data source

# TODO priority 1
# Plan how downloads will work on github actions, with caching and updates with new data
# Figure out creds for ecmwf
# Server error - local cache transer is not working (https://unix.stackexchange.com/questions/79132/invalid-cross-device-link-while-hardlinking-in-the-same-file-system)

# TODO priority 2
# encmwf: get spatial bound for all of Africa for ecmwf download
# encmwf: fix sys 51 API call (currently failing)
# wahis: refactor to download with dynamic branching

# Data Source Download -----------------------------------------------------------
source_targets <- tar_plan(
  
  ## wahis
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks_preprocessed, preprocess_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw)),

  ## ecmwf
  tar_target(ecmwf_api_parameters, set_ecmwf_api_parameter() |> 
               filter(system != 51) |> # temp until download bug is fixed
               slice(1:2) |>  # temp for faster testing
               rowwise() |> 
               tar_group(),
             iteration = "group"), 
  
  tar_target(ecmwf_forecasts_download, download_ecmwf_forecasts(parameters = ecmwf_api_parameters,
                                                                user_id = "173186",
                                                                variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                                                product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                                                leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                                                spatial_bound = c(-21, 15, -35, 37), # N, W, S, E
                                                                download_directory = "data/ecmwf_gribs"),
             pattern = map(ecmwf_api_parameters), 
             iteration = "list"),
  
  
  tar_target(ecmwf_forecasts_preprocessed,
             preprocess_ecmwf_forecasts(ecmwf_forecasts_download,
                                        download_directory = "data/ecmwf_gribs",
                                        preprocessed_directory =  "data/ecmwf_csvs"),
             pattern = map(ecmwf_forecasts_download), 
             iteration = "list",
             format = "file" 
  ),
  
  # cache locally
  # Note the tar_read. When using AWS this does not read into R but instead initiates a download of the file into the scratch folder for later processing.
  # Format file here means if we delete or change the local cache it will force a re-download.
  tar_target(ecmwf_forecasts_preprocessed_local, {suppressWarnings(dir.create(here::here("data/ecmwf_csvs"), recursive = TRUE))
    cache_aws_branched_target(tmp_path = tar_read(ecmwf_forecasts_preprocessed),
                              ext = ".csv.gz")},
    repository = "local", 
    format = "file"
  ),
  
  # SERVER ERROR
  # Warning messages:
  #   1: In file.rename(from = scratch, to = stage) :
  #   cannot rename file '/tmp/RtmpVdd2Fa/scratch/targets_aws_file_570ca545a66c0' to 'data/ecmwf_csvs/ecmwf_seasonal_forecast_4_2017_to_2017.csv.gz', reason 'Invalid cross-device link'
  
)

# Data Processing -----------------------------------------------------------
data_targets <- tar_plan(
  
  # Data cleaning - weather
  # Assembly into time series
  ## Each row is a pixel for a date (maybe on a weekly basis?, pixel based on coarser resolution of forecasts? or on finer resolution of recorded data)
  ## trailing 90 days of recorded weather data (aggregated by month)
  ## forecasted 90 days (weight based on time since last seasonal forecast)
  
  # Data cleaning - RVF
  ## overlay with weather data 
  ## add variable for spatial autocorrelation?
  
)

# Model -----------------------------------------------------------
model_targets <- tar_plan(
  
  # I like this workflow from rvf-ews1
  
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
  
  # Regular updating of data - append to parquet file or duckdb
  ## ecmwf forecast data = monthly (updated on the 13th)
  ## recorded data = daily ?
  
  # Use fixed version of the model to generate new predictions
  
)

# Plots -----------------------------------------------------------
plot_targets <- tar_plan(
  
  # I like to pregenerate plots to feed into reports
  
)

# Reports -----------------------------------------------------------
report_targets <- tar_plan(
  
  # Data eval look at how past weather forecasts compare to historical recorded data
  # Static model diagnostics and interpretation (test data performance, mcmc chains, VIP)
  # Real time performance. How are our predictions compared to actual?
  
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
