# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

nproc <- 4

# Targets options
tar_option_set(resources = tar_resources(
  aws = tar_resources_aws(bucket = Sys.getenv("AWS_BUCKET_ID"), prefix = "open-rvfcast"),
  qs = tar_resources_qs(preset = "fast")),
  repository = "aws",
  format = "qs"
)

# Wahis download
wahis <- tar_plan(
  tar_target(wahis_rvf_outbreaks_raw, get_wahis_rvf_outbreaks_raw()) # TODO: setup scheduled run to get new outbreaks (after last outbreak)
  #tar_target(wahis_rvf_outbreaks, clean_wahis_rvf_outbreaks(wahis_rvf_outbreaks_raw)) 
  )

# List targets -----------------------------------------------------------------

list(
  wahis
  )
