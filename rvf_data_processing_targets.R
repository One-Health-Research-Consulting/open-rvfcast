# This repository uses targets projects.
# To switch to the data acquisition and cleaning pipeline run:
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

data_import_targets <- tar_plan(
  tar_target(continent_polygon, create_africa_polygon()),
  tar_target(wahis_raster_template, terra::rasterize(
    terra::vect(continent_polygon), # Take the boundary of Africa
    terra::rast(continent_polygon, # Mask against a raster filled with 1's
      resolution = 0.1, # Set resolution
      vals = 1
    )
  ) |> terra::wrap()), # Wrap to avoid problems with targets

  # Import base predictors from the predictor processing project
  tar_target(
    base_predictors_directory,
    create_data_directory(directory_path = "data/africa_full_predictor_data")
  ),

  # Download predictor files from AWS if they don't already exist
  tar_target(base_predictors_AWS,
    AWS_get_folder(base_predictors_directory,
      skip_fetch = FALSE,
      sync_with_remote = FALSE
    ),
    error = "continue",
    cue = tar_cue("always")
  ),

  # Read all parquet files in the directory using Arrow
  tar_target(
    base_predictors,
    {
      print(glue::glue("{length(base_predictors_AWS)} files downloaded from AWS"))

      list.files(base_predictors_directory,
        pattern = "\\.parquet$",
        full.names = TRUE
      )
    }
  ),

  # Import RVF outbreak data
  tar_target(rvf_outbreaks, get_wahis_rvf_outbreaks() |>
    mutate(
      start_date = coalesce(outbreak_start_date, outbreak_end_date),
      end_date = coalesce(outbreak_end_date, outbreak_start_date)
    ) |>
    select(cases, start_date, end_date, latitude, longitude) |>
    distinct() |>
    arrange(end_date) |>
    mutate(outbreak_id = seq_len(n()))),

  tar_target(
    rvf_response_directory,
    create_data_directory(directory_path = "data/rvf_response")
  ),

  tar_target(rvf_response,
    get_rvf_response(rvf_outbreaks,
      wahis_raster_template,
      forecast_intervals,
      predictor_dates,
      local_folder = rvf_response_directory
    ),
    format = "file",
    repository = "local"
  )
)

rvf_processing_targets <- tar_plan()


# Join response to processed predictors
data_integration_targets <- tar_plan(
  tar_target(rvf_model_data, join(rvf_response, base_predictors)),
)

# Subset and aggregate data down to desired spatial scale
# and save output
aggregation_targets <- tar_plan(

  # Import any other necessary datasets
  tar_target(zaf_districts, rgeoboundaries::geoboundaries("South Africa", "adm2")),

  # tar_target(aggregation_spec, ),

  tar_target(
    rvf_zaf_data_directory,
    create_data_directory(directory_path = "data/rvf_zaf_data")
  ),
  tar_target(aggregate_model_data, aggregate_to_polygon(data,
    zaf_districts,
    aggregation_spec,
    local_folder = rvf_zaf_data_directory,
    basename_template = "rvf_zaf_{shapeName}.parquet",
  ),
  pattern = map(zaf_districts)
  )
)

list(
  data_import_targets,
  rvf_processing_targets,
  data_integration_targets,
  aggregation_targets
)
