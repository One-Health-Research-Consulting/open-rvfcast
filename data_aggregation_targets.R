# This repository uses targets projects.
# To switch to the data acquisition adn cleaning pipeline run:
# `Sys.setenv(TAR_PROJECT = "data")`

# Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true")) {
  capsule::capshot(c(
    "packages.R",
    list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE),
    list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)
  ))
}

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source(f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

# Targets options
source("_targets_settings.R")

data_aggregating_targets <- tar_plan(

  tar_target(africa_full_rvf_model_data_AWS,
    AWS_get_folder(africa_full_rvf_model_data_directory),
    error = "null",
    cue = tar_cue("always")
  ),

  tar_target(rsa_polygon, rgeoboundaries::geoboundaries("South Africa", "adm2"))

)

#   # Aggregate data down to a specified set of sf multipolygons.
#   # This involves aggregating a bunch of different types of data.
#   # We specified the aggregating function for each variable by hand
#   tar_target(predictor_aggregating_functions, read_csv("data/predictor_summary.csv")),
#   tar_target(
#     RSA_rvf_model_data_directory,
#     create_data_directory(directory_path = "data/RSA_rvf_model_data")
#   ),

#   # Check if RSA_rvf_model_data parquet files already exists on AWS and can be loaded
#   # The only important one is the directory. The others are there to enforce dependencies.
#   tar_target(RSA_rvf_model_data_AWS, AWS_get_folder(
#     RSA_rvf_model_data_directory,
#     africa_full_rvf_model_data
#   ),
#   error = "null",
#   cue = tar_cue("always")
#   ), # cue is what to do when flag == "TRUE"

#   tar_target(RSA_rvf_model_data,
#     spatial_aggregate_arrow(africa_full_rvf_model_data,
#       rsa_polygon,
#       predictor_aggregating_functions,
#       model_dates_selected,
#       local_folder = "data/RSA_rvf_model_data",
#       basename_template = "RSA_rvf_model_data_{model_dates_selected}.parquet",
#       overwrite = parse_flag("OVERWRITE_AFRICA_FULL_RVF_MODEL_DATA"),
#       RSA_rvf_model_data_AWS
#     ), # Enforce dependency
#     pattern = map(model_dates_selected),
#     format = "file",
#     repository = "local"
#   ),

#   # Next step put combined_anomalies files on AWS.
#   tar_target(RSA_rvf_model_data_AWS_upload, AWS_put_files(
#     RSA_rvf_model_data,
#     RSA_rvf_model_data_directory
#   ),
#   error = "null"
#   ),
# )

# List targets -----------------------------------------------------------------
# all_targets() doesn't work with tarchetypes like tar_change().
list(
  data_aggregating_targets
)
