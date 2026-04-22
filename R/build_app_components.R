#' Create the needed data files for the visualization app
#'
#'
#' @title build_app_components

#' @param predictions target examined_fits_within_pan
#' @param outpath path to save file for app
#' @return path to saved file
#' @author Morgan Kain
#' @export

build_app_components <- function(predictions, outpath) {

  predictions  <- predictions[!grepl("No_aggregation", predictions)]
  files_tibble <- tibble(
    double_agg   = predictions[grepl("Double", predictions)]
  , temporal_agg = predictions[grepl("Temporal", predictions)]
  , spatial_agg  = predictions[grepl("Spatial", predictions)]
  )
  
 double_agg   <- purrr::map(files_tibble$double_agg  , .f = function(i) { qread(i) }) %>% bind_rows()
 temporal_agg <- purrr::map(files_tibble$temporal_agg, .f = function(i) { qread(i) }) %>% bind_rows()
 spatial_agg  <- purrr::map(files_tibble$spatial_agg , .f = function(i) { qread(i) }) %>% bind_rows()
 
 tibble(
   double_agg   = double_agg %>% list()
 , temporal_agg = temporal_agg %>% list()
 , spatial_agg  = spatial_agg %>% list()
 ) %>% qsave(., paste(outpath, "/data_for_app.qs", sep = ""))
    
}
