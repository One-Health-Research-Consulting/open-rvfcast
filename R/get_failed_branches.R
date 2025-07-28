library(targets)
library(dplyr)

get_failed_branches <- function(target_name) {
  # Get metadata and subset to failed branches
  failed_meta <- tar_meta(starts_with(target_name)) |> filter(!is.na(error) & error != "")
  
  if(nrow(failed_meta) == 0) {
    return(tibble(
      failed_branch = character(0),
      slice_index = integer(0), 
      input_branch = character(0),
      error_message = character(0)
    ))
  }
  
  # Get branch mappings using do.call to properly evaluate the target name
  branches_df <- do.call("tar_branches", list(as.name(target_name))) %>%
    rownames_to_column("slice_index") %>%
    mutate(slice_index = as.integer(slice_index))
  
  # Join failed metadata with branch mappings
  failed_meta %>%
    left_join(branches_df, by = c("name" = names(branches_df)[2])) %>%
    select(
      failed_branch = name,
      slice_index,
      input_branch = all_of(names(branches_df)[3]),
      error_message = error
    )
}

# Usage:
failed_branches <- get_failed_branches("ecmwf_forecasts_transformed")
