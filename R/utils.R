#' Collect all targets and lists of targets in the environment
all_targets <- function(env = parent.env(environment()), type = "tar_target", add_list_names = TRUE) {
  
  # Function to determine if an object is a type (a target), or a list on only that type
  rfn <- function(obj) inherits(obj, type) || (is.list(obj) && all(vapply(obj, rfn, logical(1))))
  # Get the names everything in the environment (e.g. sourced in the _targets file)
  objs <- ls(env)
  out <- list()
  
  for(o in objs) {
    obj <- get(o, envir = env) # Get each top-level object in turn
    if (rfn(obj)) { # For targets and lists of targets
      out[[length(out) + 1]] <- obj # Add them to the output
      
      # If the object is a list of targets, add a vector of those names to the environment
      # So that one can call `tar_make(list_name)` to make all the targets in that list
      if(add_list_names && is.list(obj)) {
        target_names <- vapply(obj, \(x) x$settings$name, character(1))
        assign(o, target_names, envir = env)
      }
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
