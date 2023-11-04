tar_option_set(
  error = Sys.getenv("TARGETS_ERROR", unset = "stop"), # allow branches to error without stopping the pipeline
  workspace_on_error = TRUE, # allows interactive session for failed branches
  format = "qs",              # Use qs instead of rds for fast serialization
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")
  ),
  # Settings to limit memory usage,
  # See https://books.ropensci.org/targets/performance.html#memory
  memory = "transient",  # Discard targets after loading to clear memory
  garbage_collection = TRUE # Clean up memory before building next target
)

# Set up a process controller if multiple cores are requested
if (Sys.getenv("NPROC", unset = "1") != "1") {
  tar_option_set(
    controller = crew::crew_controller_local(
      name = "local",
      workers = as.integer(Sys.getenv("NPROC", unset = "1"))
    )
  )
}

# Use shared S3 cache if available.
# See .Rprofile for switching cache targetsstore based on this
# Also controls the location of updated parqet data sets
if(nzchar(Sys.getenv("AWS_BUCKET_ID")) && Sys.getenv("TAR_PROJECT") != "sandbox") {
  tar_option_set(
    repository = "aws",
    format = "qs",
    resources = tar_resources(
      aws = tar_resources_aws(
        prefix = "_targets",
        bucket = Sys.getenv("AWS_BUCKET_ID"),
        region = Sys.getenv("AWS_REGION")
      ),
      qs = tar_resources_qs(preset = "fast")
    ),
    storage = "worker",
    retrieval = "worker"
  )
}