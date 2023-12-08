#' Collect all targets and lists of targets in the environment
all_targets <- function(env = parent.env(environment()), type = "tar_target") {
  rfn <- function(obj) inherits(obj, type) || (is.list(obj) && all(vapply(obj, rfn, logical(1))))
  objs <- ls(env)
  out <- list()
  for(o in objs) {
    obj <- get(o, envir = env)
    if (rfn(obj)) {
      out[[length(out) + 1]] <- obj
    }
  }
  return(out)
}

# TODO: convenience functions for reading targets from S3
# Need to figure out the best way for these to work with both regular and 
# file-type targets, which should emerge from working with them in the sandbox
# tar_read_s3 <- function(target_name, bucket = Sys.getenv("AWS_BUCKET_ID"),
#                         prefix = "_targets") {
#   ## if target_name does not exist, convert the symbol to character
#   if (!exists(target_name)) {
#     target_name <- as.character(substitute(target_name))
#   }
#     
# }
#   
# tar_load_s3 <- function(target_name, ...) {
#   
# }

#' Get NAs
col_na <- function(df) purrr::map_lgl(df, ~any(is.na(.)))
