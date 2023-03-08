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
# alternate weather data source
# map out model and deploy

# TODO priority 1
# Plan how downloads will work on github actions, with caching and updates with new data
# Figure out creds for ecmwf
# Figure out dynamic branching with cacheing

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
                                                                download_directory = here::here("data", "ecmwf_gribs")),
             pattern = map(ecmwf_api_parameters), 
             iteration = "list"),
  
  
  # convert grib files to parquet
  tar_target(ecmwf_forecasts_preprocessed,
             preprocess_ecmwf_forecasts(ecmwf_forecasts_download,
                                        download_directory = here::here("data", "ecmwf_gribs"),
                                        preprocessed_directory =  here::here("data", "ecmwf_csvs")),
             pattern = map(ecmwf_forecasts_download), 
             iteration = "list",
             format = "file" 
             ),

  # Note the tar_read. When using AWS this does not read
  # into R but instead initiates a download of the file into
  # the scratch folder for later processing.
  # Format file here means if we delete or change the local cache it
  # will force a re-download.
  tar_target(ecmwf_forecasts_local, 
             tar_read(ecmwf_forecasts_preprocessed), 
             #TODO tar_read is not working with mapping, instead multiplies number of endpoints by number of branches
             # maybe that means we can skip branching here, just use the list?
             
             # cache_aws_target(tmp_path = tar_read(ecmwf_forecasts_preprocessed),
             #                  ext = ".csv.gz"), 
             pattern = map(ecmwf_forecasts_preprocessed)#, 
             #iteration = "list",
             #repository = "local", 
             #format = "file"
             ),
  
)

# List targets -----------------------------------------------------------------

list(
  wahis,
  ecmwf
)
