################################################################################
## "top-level" functions for compare_to_fao.R
################################################################################

#' Summarize FAO RVF decision support tool (DST) rasters to H3 hexes
#'
#' The FAO RVF DST maps are ~250 m categorical rasters:
#'   0 = no data / no risk
#'   1 = risk current month only
#'   2 = risk last month only
#'   3 = persistent risk (current and last month)
#' Each H3 hex is rasterized onto the FAO grid and then the pixels of each class 
#' are counted within each hex for each month. The result is the share of
#' each hex's area in each class
#'
#' @title summarize_fao_dst_to_hexes
#' @param raster_files Paths to FAO DST GeoTIFFs named like RVF.<mapset>.<YYYY-MM>.tif
#' @param region_hexes The region_hexes target (sf of hexes with shapeName, or a list holding one)
#' @param cache_dir Folder for the hex zone raster and the per-month hex summaries
#' @param min_flag_frac Minimum share of a hex's area in classes 1-3 for the hex to be
#'   given a risk category (otherwise category 0)
#' @param overwrite Recount months already summarized in cache_dir
#' @return Tibble with one row per hex and FAO month: shapeName, fao_month (first day of
#'   the month), n_cells, frac_current_only (class 1), frac_last_only (class 2),
#'   frac_persistent (class 3), frac_current (classes 1 + 3), frac_any (classes 1-3) and
#'   fao_class (0-3, the risk class covering the most hex area when frac_any >= min_flag_frac)
#' @author Morgan Kain
#' @export
summarize_fao_dst_to_hexes <- function(
    raster_files
  , region_hexes
  , cache_dir     = "data/fao_rvf_rasters/hex_summaries"
  , min_flag_frac = 0.05
  , overwrite     = FALSE
) {
  
  ## Hex polygons, accepting either the sf or the list that wraps it in the targets pipeline
  hex_sf <- if (inherits(region_hexes, "sf")) region_hexes else region_hexes[[1]]
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  ## One zone raster for all months
  zones <- get_fao_hex_zones(raster_files[1], hex_sf, cache_dir)
  
  ## Count classes per hex for each month, reusing cached months unless overwrite = TRUE
  counts <- purrr::map_dfr(raster_files, function(f) {
    month_tag  <- sub("^.*\\.(\\d{4}-\\d{2})\\.tif$", "\\1", basename(f))
    cache_file <- file.path(cache_dir, paste0(sub("\\.tif$", "", basename(f)), "_hex_counts.Rds"))
    
    if (file.exists(cache_file) && !overwrite) return(readRDS(cache_file))
    message("Counting FAO DST classes by hex: ", basename(f))
    
    out <- count_fao_classes_by_hex(f, zones$raster, zones$lookup) |>
      dplyr::mutate(fao_month = as.Date(paste0(month_tag, "-01")), .after = shapeName)
    
    saveRDS(out, cache_file)
    out
    
  })
  
  ## Area shares per class and the single hex category
  counts |>
    dplyr::mutate(
      frac_current_only = n_1 / n_cells
    , frac_last_only    = n_2 / n_cells
    , frac_persistent   = n_3 / n_cells
    , frac_current      = (n_1 + n_3) / n_cells
    , frac_any          = (n_1 + n_2 + n_3) / n_cells
    , fao_class         = dplyr::case_when(
      frac_any < min_flag_frac ~ 0L
    , n_3 >= n_1 & n_3 >= n_2  ~ 3L
    , n_1 >= n_2               ~ 1L
    , TRUE                     ~ 2L)
    ) |>
    dplyr::select(shapeName, fao_month, n_cells, dplyr::starts_with("frac_"), fao_class)
  
}


#' Compare hex-level RVF forecasts with the FAO RVF DST monthly risk maps
#'
#' Puts the two products on a common hex -by- calendar-month grid and scores both 
#' against the outbreaks observed in that hex and month.
#'
#' Time alignment (month_alignment = "issue_month", the default): window k (1 = days
#' 1-30, 2 = days 31-60, ...) issued from a base date in month M - k is compared with the
#' FAO map for month M. E.g., FAO June 2021 is compared with the first window issued in May
#' 2021, the second window issued in April 2021, and so on. 
#' 
#' With month_alignment = "midpoint" each window is instead assigned to the calendar 
#' month holding its midpoint. 
#' 
#' Forecasts for the same hex, target month and window are averaged (about two base dates
#' per issue month). The FAO map for month M is compared with target month M +
#' fao_month_offset. FAO describes the map as risk for the current month from
#' near-real-time climate data, so the default offset is 0, making the FAO map close to a
#' nowcast.
#'
#' "High" risk: an FAO hex is high when at least min_flag_frac of its area is at
#' current-month risk (fao_class 1 or 3; see summarize_fao_dst_to_hexes()). A model hex is
#' high when its probability is at or above a threshold per window. By default that
#' threshold flags the same total share of hex-months (over all compared months) as FAO
#' flags, so both products spend the same alert budget but can spread it over months
#' differently. Set model_high_threshold to use a fixed probability instead.
#'
#' Outputs (tables as csv, figures as png, everything as results.qs) go to out_dir:
#'   - skill: ROC-AUC and PR-AUC against hex-month outbreaks for our forecasts at each lead
#'     and for the FAO area shares, pooled and by month, on the same hex-months
#'   - monthly_metrics: one row per target month and product (FAO, and the model at each
#'     window) with the extent of high risk, the share of outbreak hexes in high or top
#'     hexes, and the mean percentile rank of outbreak hexes (see fao_comparison_column_guide())
#'   - monthly_model_vs_fao: the model at each window against FAO side by side for each
#'     target month, with which product did better that month
#'   - column_guide: what each column of the two monthly tables means
#'   - agreement_correlation: Spearman correlation between our forecasts and FAO frac_current
#'   - agreement_by_class: distribution of our forecasts within each FAO hex category
#'   - monthly_comparison: time series of the monthly metrics and which product did better
#'   - maps of both products with outbreak hexes outlined
#'
#' @title compare_to_fao_dst
#' @param predictions Path to saved predictions (the ex_fits.all_probs_raw target) or a
#'   tibble with date, shapeName, forecast_interval and prob_pred (e.g., the output of
#'   read_harvest_predictions())
#' @param fao_hex Output of summarize_fao_dst_to_hexes()
#' @param hexed_outbreaks_path Outbreaks placed in their hexes (export_hexed_outbreaks())
#' @param region_hexes The region_hexes target, for maps
#' @param forecast_horizon Upper bounds (days) of the forecast windows; the first starts at day 1
#' @param fao_month_offset Months added to the FAO month to give the target month compared
#' @param month_alignment "issue_month" or "midpoint" (see Time alignment above)
#' @param top_share Share of hexes counted as the top-ranked hexes in each month
#' @param model_high_threshold Fixed model probability for "high"; NULL matches FAO's alert budget
#' @param map_lead Lead window (forecast_interval) drawn in the maps
#' @param min_events_for_auc Minimum outbreak hex-months before AUCs are computed
#' @param out_dir Output folder; NULL to skip saving
#' @return List of tables, plots and the joined hex-month data
#' @author Morgan Kain
#' @export
compare_to_fao_dst <- function(
    predictions
  , fao_hex
  , hexed_outbreaks_path = "data/rvf_outbreaks_hexed.Rds"
  , region_hexes         = NULL
  , forecast_horizon     = c(30, 60, 90, 120, 150)
  , fao_month_offset     = 0
  , month_alignment      = c("issue_month", "midpoint")
  , top_share            = 0.25
  , model_high_threshold = NULL
  , map_lead             = 30
  , min_events_for_auc   = 5
  , out_dir              = file.path("outputs/fao_dst_comparison", Sys.Date())
) {
  
  month_alignment <- match.arg(month_alignment)
  
  ## Our forecasts averaged to hex -by- target month -by- lead window
  model_month <- read_hex_predictions(predictions) |>
    forecasts_to_target_months(make_forecast_windows(forecast_horizon), month_alignment)
  
  ## FAO maps keyed to the target month they are compared with
  fao_month <- fao_hex |>
    dplyr::mutate(target_month = lubridate::add_with_rollback(fao_month, lubridate::period(fao_month_offset, "month")))
  
  ## Outbreak label for every hex -by- calendar month
  outbreak_month <- readRDS(hexed_outbreaks_path) |>
    dplyr::filter(counts_toward_label == 1) |>
    dplyr::mutate(target_month = lubridate::floor_date(start_date, "month")) |>
    dplyr::summarize(n_outbreaks = dplyr::n(), n_cases = sum(cases), .by = c(shapeName, target_month))
  
  ## Common hex-months only: months with an FAO map and our forecasts, hexes in both
  joined <- model_month |>
    dplyr::inner_join(fao_month, by = c("shapeName", "target_month")) |>
    dplyr::left_join(outbreak_month, by = c("shapeName", "target_month")) |>
    dplyr::mutate(
      n_outbreaks = tidyr::replace_na(n_outbreaks, 0L)
      , outbreak    = as.integer(n_outbreaks > 0)
      , fao_flag    = as.integer(fao_class %in% c(1L, 3L))
      , fao_label   = factor(fao_class, levels = 0:3, labels = c("0 none", "1 current only", "2 last month only", "3 persistent"))
    )
  if (!nrow(joined)) stop("No hex-months in common between the predictions and the FAO maps")
  
  ## Coverage of the comparison: hex-months and outbreaks per target month and lead
  coverage <- joined |>
    dplyr::summarize(
      n_hexes = dplyr::n_distinct(shapeName), n_outbreak_hexes = sum(outbreak)
      , n_fao_flagged = sum(fao_flag), mean_forecasts_averaged = mean(n_forecasts)
      , .by = c(target_month, fao_month, forecast_interval)) |>
    dplyr::arrange(target_month, forecast_interval)
  
  ## Per-month metrics for each product, and the model against FAO side by side
  monthly_metrics <- summarize_monthly_metrics(joined, top_share, model_high_threshold)
  
  tables <- list(
    coverage             = coverage
  , monthly_metrics      = monthly_metrics
  , monthly_model_vs_fao = compare_monthly_metrics(monthly_metrics)
  , column_guide         = fao_comparison_column_guide(top_share)
  , skill                = score_fao_comparison(joined, min_events_for_auc))
  
  tables <- c(tables, summarize_fao_agreement(joined))
  
  plots <- plot_fao_comparison(joined, tables, region_hexes, map_lead)
  
  out <- list(tables = tables, plots = plots, data = joined)
  if (!is.null(out_dir)) save_fao_comparison(out, out_dir)
  
  out
  
}

