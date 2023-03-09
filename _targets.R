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
# map out model and deploy
# alternate weather data source

# TODO priority 1
# Plan how downloads will work on github actions, with caching and updates with new data
# Figure out creds for ecmwf
# Server error - local cache transer is not working (https://unix.stackexchange.com/questions/79132/invalid-cross-device-link-while-hardlinking-in-the-same-file-system)

# TODO priority 2
# encmwf: get spatial bound for all of Africa for ecmwf download
# encmwf: fix sys 51 API call (currently failing)
# wahis: refactor to download with dynamic branching

# Data Download -----------------------------------------------------------
wahis <- tar_plan(
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()),
  tar_target(wahis_rvf_outbreaks, clean_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw)) 
)

ecmwf <- tar_plan(
  
  tar_target(ecmwf_api_parameters, set_ecmwf_api_parameter() |> 
               filter(system != 51) |>  # NEED TO BUG FIX
               slice(1:2) |>  # TMP
               rowwise() |> 
               tar_group(),
             iteration = "group"), 
  
  # use dynamic mapping to download, transform, and cache
  tar_target(ecmwf_forecasts_download, download_ecmwf_forecasts(parameters = ecmwf_api_parameters,
                                                                user_id = "173186",
                                                                variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                                                product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                                                leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                                                spatial_bound = c(-21, 15, -35, 37), # N, W, S, E
                                                                download_directory = "data/ecmwf_gribs"),
             pattern = map(ecmwf_api_parameters), 
             iteration = "list"),
  
  
  # convert grib files to compressed csvs
  tar_target(ecmwf_forecasts_preprocessed,
             preprocess_ecmwf_forecasts(ecmwf_forecasts_download,
                                        download_directory = "data/ecmwf_gribs",
                                        preprocessed_directory =  "data/ecmwf_csvs"),
             pattern = map(ecmwf_forecasts_download), 
             iteration = "list",
             format = "file" 
  ),
  
  # cache locally
  # ------
  # Note the tar_read. When using AWS this does not read
  # into R but instead initiates a download of the file into
  # the scratch folder for later processing.
  # Format file here means if we delete or change the local cache it
  # will force a re-download.
  tar_target(ecmwf_forecasts_local, {suppressWarnings(dir.create(here::here("data/ecmwf_csvs"), recursive = TRUE))
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

# List targets -----------------------------------------------------------------

list(
  wahis,
  ecmwf
)
