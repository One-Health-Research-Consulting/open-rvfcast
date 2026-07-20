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
, storage = "worker"
, retrieval = "worker"
)

# Set up a process controller if multiple cores are requested
if (Sys.getenv("NPROC", unset = "1") != "1") {
  tar_option_set(
    controller = crew::crew_controller_local(
      name = "local"
    , reset_globals = FALSE
    , workers = as.integer(Sys.getenv("NPROC", unset = "1"))
      # Timeouts raised well above worker startup + sync-check time so a slow-but-alive
      # worker (fresh R + full tidymodels/xgboost load every task under memory pressure)
      # is not mis-classified as a crash.
    , seconds_timeout  = 600
    , seconds_interval = 5
    , seconds_launch   = 600
      # Tolerate transient worker deaths (e.g. an occasional OOM kill when heavy tuning
      # branches coincide) by retrying the branch many times before aborting tar_make,
      # instead of the default of 5.
    , crashes_max = 30L
      # Run gc() on the worker after each task to shed peak memory between branches.
    , garbage_collection = TRUE
      # One task per worker keeps memory from accumulating across branches.
    , tasks_max = 1L
      # Persist per-worker stdout/stderr so the actual cause of any crash (OOM kill vs.
      # segfault) is captured rather than lost.
    , options_local = crew::crew_options_local(log_directory = "logs/crew")
    )
  )
}

# # Use shared S3 cache if available.
# # See .Rprofile for switching cache targetsstore based on this
# # Also controls the location of updated parqet data sets
# if(nzchar(Sys.getenv("AWS_BUCKET_ID")) && Sys.getenv("TAR_PROJECT") != "sandbox") {
#   tar_option_set(
#     repository = "aws",
#     format = "qs",
#     resources = tar_resources(
#       aws = tar_resources_aws(
#         prefix = "_targets",
#         bucket = Sys.getenv("AWS_BUCKET_ID"),
#         region = Sys.getenv("AWS_REGION")
#       ),
#       qs = tar_resources_qs(preset = "fast")
#     ),
#     storage = "worker",
#     retrieval = "worker"
#   )
# }
