#' Create the needed data files for the visualization app
#'
#'
#' @title build_app_components

#' @param predictions target examined_fits_within_pan
#' @param shapvals shap values
#' @param outpath path to save file for app
#' @return path to saved file
#' @author Morgan Kain
#' @export

build_app_components <- function(predictions, shapvals, outpath) {

  predictions.map <- predictions[grepl("predictions", predictions)]
  predictions.cal <- predictions[grepl("calcurves", predictions)]

  files_tibble.map <- tibble(
    no_agg       = predictions.map[grepl("No_aggregation", predictions.map)]
  , double_agg   = predictions.map[grepl("Double", predictions.map)]
  , temporal_agg = predictions.map[grepl("Temporal", predictions.map)]
  , spatial_agg  = predictions.map[grepl("Spatial", predictions.map)]
  )

  files_tibble.cal <- tibble(
    no_agg       = predictions.cal[grepl("No_aggregation", predictions.cal)]
  , double_agg   = predictions.cal[grepl("Double", predictions.cal)]
  , temporal_agg = predictions.cal[grepl("Temporal", predictions.cal)]
  , spatial_agg  = predictions.cal[grepl("Spatial", predictions.cal)]
  )

  no_agg.map       <- purrr::map(files_tibble.map$no_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  double_agg.map   <- purrr::map(files_tibble.map$double_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  temporal_agg.map <- purrr::map(files_tibble.map$temporal_agg, .f = function(i) {
   qread(i)
  }) |> bind_rows()
  spatial_agg.map  <- purrr::map(files_tibble.map$spatial_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()

  no_agg.cal       <- purrr::map(files_tibble.cal$no_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  double_agg.cal   <- purrr::map(files_tibble.cal$double_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  temporal_agg.cal <- purrr::map(files_tibble.cal$temporal_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()
  spatial_agg.cal  <- purrr::map(files_tibble.cal$spatial_agg, .f = function(i) {
    qread(i)
  }) |> bind_rows()

  map.tib <- tibble(
    no_agg       = no_agg.map |> list()
  , double_agg   = double_agg.map |> list()
  , temporal_agg = temporal_agg.map |> list()
  , spatial_agg  = spatial_agg.map |> list()
  )

  cal.tib <- tibble(
    no_agg       = no_agg.cal |> list()
  , double_agg   = double_agg.cal |> list()
  , temporal_agg = temporal_agg.cal |> list()
  , spatial_agg  = spatial_agg.cal |> list()
  )

  shap.tib <- shapvals |>
      dplyr::select(shap_summary) |>
      rename(shap.tib = shap_summary)

  tibble(
    map.tib  = map.tib |> list()
  , cal.tib  = cal.tib |> list()
  ) |>
  bind_cols(shap.tib) |>
  qsave(paste(outpath, "/data_for_app.qs", sep = ""))

  paste(outpath, "/data_for_app.qs", sep = "")

}
