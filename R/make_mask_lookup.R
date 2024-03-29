#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_data
#' @return
#' @author Emma Mendelsohn
#' @export
make_mask_lookup <- function(model_dates_selected, rsa_polygon) {
  
  masked_dates_90_days_lookup <- map_dfr(model_dates_selected, function(date){
    diffs <- model_dates_selected - date
    tibble(date = date, mask = list(model_dates_selected[diffs > 0 & diffs <= 90]))
  })
  
  masked_shapes_adjacent_lookup <- map_dfr(1:nrow(rsa_polygon), function(i){
    select_shape <- rsa_polygon[i,]
    touches <- st_touches(select_shape, rsa_polygon)
    touches_shapes <- rsa_polygon[unlist(touches),]
    tibble(shape = select_shape$shapeName, 
           mask = list(touches_shapes$shapeName))
  })
  
  list(masked_dates_90_days_lookup = masked_dates_90_days_lookup,
       masked_shapes_adjacent_lookup = masked_shapes_adjacent_lookup
       )
  
}
