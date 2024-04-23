#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param holdout_data
#' @param mask_lookup
#' @return
#' @author Emma Mendelsohn
#' @export
get_holdout_masks <- function(holdout_data, mask_lookup) {

  date_masks <- holdout_data |> 
    select(date, shapeName) |> 
    left_join(mask_lookup$masked_dates_90_days_lookup, by = join_by(date)) |> 
    unnest(mask) |> 
    select(date = mask, shapeName)
  
  shape_masks <- holdout_data |> 
    select(date, shapeName) |> 
    left_join(mask_lookup$masked_shapes_adjacent_lookup, by = join_by("shapeName" == "shape")) |> 
    unnest(mask) |> 
    select(date, shapeName = mask)
  
  masks <- bind_rows(date_masks, shape_masks) |> 
    distinct()
  
  return(masks)
  
  # take 1 - mask shape and date (note this incorrectly misses having the district itself masked for 3 months)
 # holdout_data |> 
 #    select(date, shapeName) |> 
 #    left_join(mask_lookup$masked_dates_90_days_lookup, by = join_by(date)) |> 
 #    rename(date_mask = mask) |> 
 #    left_join(mask_lookup$masked_shapes_adjacent_lookup, by = join_by("shapeName" == "shape")) |> 
 #    rename(shape_mask = mask)  |> 
 #    select(-date, -shapeName) |> 
 #    unnest(shape_mask) |> 
 #    unnest(date_mask)

}
