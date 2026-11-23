################################################################################
## Internal called (helper) functions for compare_to_fao.R
################################################################################

####
## Functions to assist download and summarization of FAO RVF DST rasters ------------
####

## List every raster in a mapset as a data frame (month, url, bytes), following the API's paging
rvf_list_rasters <- function(mapset = "VAXCRM-MEA", api_path) {
  
  items <- list()
  page  <- 0
  
  repeat {
    resp <- request(paste0(api_path, "/", mapset, "/rasters")) |>
      req_url_query(page = page, size = 100, sort = "code") |>
      req_user_agent("rvf-dst-downloader/1.0") |>
      req_retry(max_tries = 3) |>
      req_perform() |>
      resp_body_json(simplifyVector = FALSE)
    
    items <- c(items, resp$response$items)
    
    if (!isTRUE(resp$response$hasNext)) break
    
    page <- page + 1
  }
  
  df <- data.frame(
    month = vapply(items, function(i) i$dimensions[[1]]$member$code, character(1))
    , url   = vapply(items, function(i) i$downloadUrl, character(1))
    , bytes = vapply(items, function(i) {
      b <- i$size$bytes
      if (is.null(b)) NA_real_ else as.numeric(b)
    }, numeric(1)),
    stringsAsFactors = FALSE)
  
  df[order(df$month), , drop = FALSE]
  
}

## Download one raster; skips it if a file of the expected size already exists
rvf_download_one <- function(url, bytes, outdir) {
  
  dest <- file.path(outdir, basename(url))
  
  if (file.exists(dest) && (is.na(bytes) || file.size(dest) == bytes)) {
    return(list(path = dest, status = "exists"))
  }
  
  tmp <- paste0(dest, ".part")
  
  request(url) |>
    req_user_agent("rvf-dst-downloader/1.0") |>
    req_timeout(600) |>
    req_retry(max_tries = 3, backoff = function(i) 5 * i) |>
    req_perform(path = tmp)
  
  if (!is.na(bytes) && file.size(tmp) != bytes) {
    unlink(tmp)
    stop(sprintf("size mismatch for %s: got %s, expected %s", url, file.size(tmp), bytes))
  }
  
  file.rename(tmp, dest)
  list(path = dest, status = "downloaded")
  
}

## Print pixel counts per class (terra processes large rasters in blocks)
rvf_summarize <- function(path) {
  
  r       <- terra::rast(path)
  f       <- terra::freq(r)
  f$class <- fao_classes[as.character(f$value)]
  
  print(f[, c("value", "class", "count")], row.names = FALSE)
  invisible(f)
  
}

## Select months, download, optionally summarize.
rvf_fetch <- function(mapset = "VAXCRM-MEA", api_path
                    , month = NULL, start = NULL, end = NULL,
                      outdir = "data/fao_rvf_rasters", summarize = FALSE, list_only = FALSE) {
  
  df <- rvf_list_rasters(
    mapset
  , api_path = api_path)
  
  if (!is.null(month)) df <- df[df$month %in% month, , drop = FALSE]
  if (!is.null(start)) df <- df[df$month >= start, , drop = FALSE]
  if (!is.null(end))   df <- df[df$month <= end, , drop = FALSE]
  
  message(sprintf("%s: %d rasters selected (~%s MB)", mapset, nrow(df),
                  format(round(sum(df$bytes, na.rm = TRUE) / 1e6), big.mark = ",")))
  
  if (list_only) return(df)
  
  dir <- file.path(outdir, mapset)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  df$path <- NA_character_
  
  for (k in seq_len(nrow(df))) {
    
    res        <- rvf_download_one(
      url    = df$url[k]
    , bytes  = df$bytes[k]
    , outdir = dir)
    
    df$path[k] <- res$path
    
    message(sprintf("[%d/%d] %s %s: %s", k, nrow(df), df$month[k], res$status, res$path))
    if (summarize) rvf_summarize(res$path)
  }
  
  invisible(df)
  
}

## Read a bounding box (lon/lat, WGS84) from the remote file
rvf_read_bbox <- function(month, xmin, xmax, ymin, ymax, mapset = "VAXCRM-MEA") {
  
  url <- sprintf("https://storage.googleapis.com/fao-gismgr-rvf-data/DATA/RVF/MAPSET/%s/RVF.%s.%s.tif",
                 mapset, mapset, month)
  
  r   <- terra::rast(paste0("/vsicurl/", url))
  
  terra::crop(r, terra::ext(xmin, xmax, ymin, ymax))
  
}


####
## Functions for further summarizing FAO and comparing to our predictions -----------
####

## Rasterize hexes onto the FAO grid, cached in cache_dir.
get_fao_hex_zones <- function(template_file, hex_sf, cache_dir) {
  
  zone_file   <- file.path(cache_dir, "hex_zones.tif")
  lookup_file <- file.path(cache_dir, "hex_zones_lookup.Rds")
  template    <- terra::rast(template_file)
  
  ## Reuse the cached zones when the hex order and the grid both match
  if (file.exists(zone_file) && file.exists(lookup_file)) {
    zones  <- terra::rast(zone_file)
    lookup <- readRDS(lookup_file)
    if (identical(lookup$shapeName, hex_sf$shapeName) && terra::compareGeom(zones, template, stopOnError = FALSE)) {
      return(list(raster = zones, lookup = lookup))
    }
  }
  
  ## Hex index as the zone value; cells are assigned to the hex holding their center
  message("Rasterizing hexes onto the FAO grid (once)")
  hex_v      <- terra::vect(sf::st_transform(hex_sf, terra::crs(template)))
  hex_v$zone <- seq_len(nrow(hex_v))
  
  zones  <- terra::rasterize(
    hex_v
  , template
  , field    = "zone"
  , filename = zone_file, overwrite = TRUE
  , wopt     = list(datatype = "INT2U", gdal = c("COMPRESS=DEFLATE")))
  
  lookup <- tibble::tibble(zone = hex_v$zone, shapeName = hex_sf$shapeName)
  
  saveRDS(lookup, lookup_file)
  
  list(raster = zones, lookup = lookup)
  
}

## Pixel counts of each FAO class within each hex for one month
count_fao_classes_by_hex <- function(raster_file, zones, lookup) {
  
  fao <- terra::rast(raster_file)
  ct  <- terra::crosstab(c(zones, fao), long = TRUE)
  
  names(ct) <- c("zone", "class", "n")
  
  ## Wide counts per hex with every class present (missing classes get 0 cells)
  ct |>
    dplyr::mutate(class = paste0("n_", class)) |>
    tidyr::pivot_wider(names_from = class, values_from = n, values_fill = 0) |>
    (\(d) { for (cl in paste0("n_", 0:3)) if (!cl %in% names(d)) d[[cl]] <- 0; d })() |>
    dplyr::mutate(n_cells = n_0 + n_1 + n_2 + n_3) |>
    dplyr::inner_join(lookup, by = "zone") |>
    dplyr::select(shapeName, n_cells, n_0, n_1, n_2, n_3) |>
    tibble::as_tibble()
  
}

## Assign each forecast window to a target calendar month, then average the forecasts 
 ## per hex, target month and window. 
 ## "issue_month": window k from a base date in month M - k targets month M. 
 ## "midpoint": the month holding the window's midpoint. 
 ## lead_days is the mean number of days from the base dates to the first day of the target month
forecasts_to_target_months <- function(hex_preds, forecast_windows, month_alignment = "issue_month") {
  
  ## Window order: 1 for the first window, 2 for the second, ...
  forecast_windows <- forecast_windows |>
    dplyr::arrange(forecast_interval) |>
    dplyr::mutate(months_ahead = dplyr::row_number())
  
  hex_preds |>
    dplyr::left_join(forecast_windows, by = "forecast_interval") |>
    dplyr::mutate(
      target_month = if (month_alignment == "issue_month") {
        ## Month arithmetic on a running month count (a vector of lubridate periods would be summed)
        month_index <- lubridate::year(date) * 12 + lubridate::month(date) - 1 + months_ahead
        lubridate::make_date(month_index %/% 12, month_index %% 12 + 1)
      } else {
        lubridate::floor_date(date + (window_lower + forecast_interval) / 2, "month")
      }
      , lead_days    = as.numeric(target_month - date)
    ) |>
    dplyr::summarize(
      prob_pred   = mean(prob_pred)
    , lead_days   = mean(lead_days)
    , n_forecasts = dplyr::n()
    , .by = c(shapeName, target_month, forecast_interval))
  
}

## ROC-AUC and PR-AUC of every score against hex-month outbreaks, pooled over 
 ## months and by month. FAO predictions are scored within each lead's hex-months
 ## so our model and fao are comparable
score_fao_comparison <- function(joined, min_events_for_auc) {
  
  ## Long table: one row per hex-month, lead and score
  long <- joined |>
    dplyr::select(shapeName, target_month, forecast_interval, outbreak
                  , model_prob = prob_pred, fao_frac_current = frac_current
                  , fao_frac_any = frac_any, fao_frac_persistent = frac_persistent) |>
    tidyr::pivot_longer(c(model_prob, dplyr::starts_with("fao_")), names_to = "score", values_to = "value")
  
  ## Scores for one group of rows
  auc_rows <- function(d) {
    
    n_pos <- sum(d$outbreak)
    ok    <- n_pos >= min_events_for_auc && n_pos < nrow(d)
    truth <- factor(d$outbreak, levels = c(1, 0))
    tibble::tibble(
      n = nrow(d), n_pos = n_pos
    , roc_auc = if (ok) yardstick::roc_auc_vec(truth, d$value, event_level = "first") else NA_real_
    , pr_auc  = if (ok) yardstick::pr_auc_vec(truth, d$value, event_level = "first") else NA_real_
    , prevalence = n_pos / nrow(d))
    
  }
  
  pooled <- long |>
    dplyr::reframe(auc_rows(dplyr::pick(dplyr::everything())), .by = c(forecast_interval, score)) |>
    dplyr::mutate(target_month = as.Date(NA), .before = 1)
  
  by_month <- long |>
    dplyr::reframe(auc_rows(dplyr::pick(dplyr::everything())), .by = c(target_month, forecast_interval, score))
  
  dplyr::bind_rows(pooled, by_month) |>
    dplyr::mutate(scope = ifelse(is.na(target_month), "pooled", "month"), .before = 1) |>
    dplyr::arrange(scope != "pooled", target_month, forecast_interval, score)
  
}

## Per-month metrics for FAO and for our model at each forecast window, on the same
 ## hexes and months.
 ## Scores: FAO share of hex area at current-month risk (frac_current), model probability.
 ## rank_pct is each hex's percentile among all hexes that month (1 = highest risk; ties
 ## share their average rank, so FAO hexes with no risk all sit at the same middling rank)
summarize_monthly_metrics <- function(joined, top_share, model_high_threshold) {
  
  ## FAO maps do not change with the model window, so one row per hex and target month
  fao_rows <- joined |>
    dplyr::distinct(shapeName, target_month, outbreak, score = frac_current, high = fao_flag) |>
    dplyr::mutate(product = "FAO DST", forecast_interval = NA_real_, high_threshold = NA_real_)
  
  ## FAO's overall share of hex-months flagged high: the model's alert budget by default
  fao_high_share <- mean(fao_rows$high)
  
  ## Model high threshold per window: matches FAO's budget unless a fixed threshold is given
  model_rows <- joined |>
    dplyr::mutate(
      high_threshold = if (is.null(model_high_threshold)) stats::quantile(prob_pred, 1 - fao_high_share, names = FALSE) else model_high_threshold
    , .by = forecast_interval) |>
    dplyr::transmute(shapeName, target_month, outbreak, score = prob_pred
                     , high = as.integer(prob_pred >= high_threshold)
                     , product = "Model", forecast_interval, high_threshold)
  
  dplyr::bind_rows(fao_rows, model_rows) |>
    dplyr::mutate(
      rank_pct = (rank(score, ties.method = "average") - 1) / (dplyr::n() - 1)
    , .by      = c(product, forecast_interval, target_month)) |>
    dplyr::summarize(
      n_hexes                = dplyr::n()
      , n_outbreak_hexes       = sum(outbreak)
      , high_threshold         = dplyr::first(high_threshold)
      , share_hexes_high       = mean(high)
      , top_cutoff_score       = stats::quantile(score, 1 - top_share, names = FALSE)
      , share_outbreaks_high   = if (any(outbreak == 1)) mean(high[outbreak == 1]) else NA_real_
      , share_outbreaks_top    = if (any(outbreak == 1)) mean(rank_pct[outbreak == 1] >= 1 - top_share) else NA_real_
      , mean_outbreak_rank_pct = if (any(outbreak == 1)) mean(rank_pct[outbreak == 1]) else NA_real_
      , share_other_hexes_high = mean(high[outbreak == 0])
      , .by = c(product, forecast_interval, target_month)) |>
    ## Hit rate minus false-alarm rate: high in outbreak hexes and low everywhere else
    dplyr::mutate(peirce_skill = share_outbreaks_high - share_other_hexes_high) |>
    dplyr::arrange(target_month, product, forecast_interval)
  
}

## Our model at each forecast window against FAO for each target month. 
 ## model_advantage > 0 means our model did better: in months with outbreaks, a 
 ## higher mean outbreak rank; in month without, fewer hexes flagged high
compare_monthly_metrics <- function(monthly_metrics) {
  
  ## Metrics compared side by side
  metric_cols <- c("share_hexes_high", "share_outbreaks_high", "share_outbreaks_top", "mean_outbreak_rank_pct", "peirce_skill")
  
  fao <- monthly_metrics |>
    dplyr::filter(product == "FAO DST") |>
    dplyr::select(target_month, n_outbreak_hexes, dplyr::all_of(metric_cols)) |>
    dplyr::rename_with(~ paste0("fao_", .x), dplyr::all_of(metric_cols))
  
  monthly_metrics |>
    dplyr::filter(product == "Model") |>
    dplyr::select(target_month, forecast_interval, dplyr::all_of(metric_cols)) |>
    dplyr::rename_with(~ paste0("model_", .x), dplyr::all_of(metric_cols)) |>
    dplyr::inner_join(fao, by = "target_month") |>
    dplyr::mutate(
      outbreak_month  = n_outbreak_hexes > 0
      , deciding_metric = ifelse(outbreak_month, "mean outbreak rank (higher is better)", "share of hexes high (lower is better)")
      , model_advantage = ifelse(outbreak_month
                                 , model_mean_outbreak_rank_pct - fao_mean_outbreak_rank_pct
                                 , fao_share_hexes_high - model_share_hexes_high)
      , better          = dplyr::case_when(
        abs(model_advantage) < 1e-9 ~ "tie"
        , model_advantage > 0         ~ "Model"
        , TRUE                        ~ "FAO DST")
    ) |>
    dplyr::relocate(target_month, forecast_interval, n_outbreak_hexes, outbreak_month, better, model_advantage, deciding_metric) |>
    dplyr::arrange(target_month, forecast_interval)
  
}

## How much the two models agree: rank correlation (agreement_correlation), and our
 ## forecasts by FAO hex category (agreement_by_class)
summarize_fao_agreement <- function(joined) {
  
  correlation <- joined |>
    dplyr::summarize(
      spearman_frac_current = suppressWarnings(stats::cor(prob_pred, frac_current, method = "spearman"))
      , spearman_frac_any     = suppressWarnings(stats::cor(prob_pred, frac_any, method = "spearman"))
      , .by = c(target_month, forecast_interval)) |>
    dplyr::arrange(target_month, forecast_interval)
  
  by_class <- joined |>
    dplyr::mutate(rank_pct = dplyr::percent_rank(prob_pred), .by = c(target_month, forecast_interval)) |>
    dplyr::summarize(
      n_hexes          = dplyr::n()
      , n_outbreak_hexes = sum(outbreak)
      , median_prob      = stats::median(prob_pred)
      , mean_prob        = mean(prob_pred)
      , median_rank_pct  = stats::median(rank_pct)
      , .by = c(target_month, forecast_interval, fao_label)) |>
    dplyr::arrange(target_month, forecast_interval, fao_label)
  
  list(agreement_correlation = correlation, agreement_by_class = by_class)
  
}


####
## Functions for plotting and saving output -----------------------------------------
####

## Figures: maps for each month, our forecasts by FAO category, and the monthly time series
plot_fao_comparison <- function(joined, tables, region_hexes, map_lead) {
  
  plots <- list()
  
  ## Our forecast percentile within hex-month, by FAO category (one panel per lead)
  plots$model_by_fao_class <- joined |>
    dplyr::mutate(rank_pct = dplyr::percent_rank(prob_pred), .by = c(target_month, forecast_interval)) |>
    ggplot2::ggplot(ggplot2::aes(fao_label, rank_pct)) +
    ggplot2::geom_boxplot(outlier.size = 0.3) +
    ggplot2::facet_wrap(~ forecast_interval, nrow = 1, labeller = ggplot2::labeller(forecast_interval = \(x) paste0("window ending day ", x))) +
    ggplot2::labs(x = "FAO DST hex category", y = "Model forecast percentile among hexes (same month and lead)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  
  ## Monthly metrics over time and which product did better each month
  plots$monthly_comparison <- plot_monthly_comparison(tables$monthly_metrics, tables$monthly_model_vs_fao)
  
  ## Side-by-side maps per target month (needs hex polygons)
  if (!is.null(region_hexes)) {
    
    hex_sf <- if (inherits(region_hexes, "sf")) region_hexes else region_hexes[[1]]
    ## Calling sf here loads its dplyr methods, so the joins below keep the geometry
    hex_sf <- sf::st_as_sf(hex_sf)
    plots$maps <- joined |>
      dplyr::filter(forecast_interval == map_lead) |>
      split(~ target_month) |>
      purrr::map(plot_one_fao_month, hex_sf = hex_sf, map_lead = map_lead) |>
      rlang::set_names(\(nm) format(as.Date(nm), "%Y-%m"))
    
  }
  
  plots
  
}

## Two-panel map for one month: FAO current-risk share and our forecast, outbreak hexes outlined
plot_one_fao_month <- function(month_rows, hex_sf, map_lead) {
  
  map_sf      <- hex_sf |> dplyr::inner_join(month_rows, by = "shapeName")
  outbreak_sf <- map_sf |> dplyr::filter(outbreak == 1)
  
  base_map <- function(fill_col, fill_lab, trans) {
    
    ggplot2::ggplot(map_sf) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_col]]), colour = NA) +
      ggplot2::geom_sf(data = outbreak_sf, fill = NA, colour = "red", linewidth = 0.5) +
      ggplot2::scale_fill_viridis_c(option = "magma", trans = trans, name = fill_lab, na.value = "grey80") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom", legend.key.width = grid::unit(1.5, "cm"))
    
  }
  
  month_lab <- format(month_rows$target_month[1], "%Y-%m")
  
  patchwork::wrap_plots(
    base_map("frac_current", "FAO: share of hex at\ncurrent-month risk (1 or 3)", "identity") +
      ggplot2::ggtitle(paste0("FAO DST, ", format(month_rows$fao_month[1], "%Y-%m")))
    , base_map("prob_pred", "Model probability", "log10") +
      ggplot2::ggtitle(paste0("Model, window ending day ", map_lead, ", target ", month_lab))
    , nrow = 1) +
    patchwork::plot_annotation(caption = "Red outline: hex with an outbreak (cases > 0) starting in the target month")
  
}

## Time series of the monthly metrics for FAO and each model window (shaded months have
## outbreaks), and a tile strip of which product did better in each month and window
plot_monthly_comparison <- function(monthly_metrics, model_vs_fao) {
  
  ## One series per product: FAO, and the model at each window (darker = shorter lead)
  windows       <- sort(unique(stats::na.omit(monthly_metrics$forecast_interval)))
  series_levels <- c("FAO DST", paste0("Model, window ending day ", windows))
  series_cols   <- rlang::set_names(c("black", grDevices::hcl.colors(length(windows), "viridis")), series_levels)
  
  ## FAO drawn as triangles, model windows as circles, so the products are easy to tell apart
  series_shapes <- rlang::set_names(c(17, rep(16, length(windows))), series_levels)
  
  ## Missing months become NA rows so the lines break across gaps
  all_months <- seq(min(monthly_metrics$target_month), max(monthly_metrics$target_month), by = "month")
  x_scale    <- ggplot2::scale_x_date(limits = range(all_months) + c(-16, 16), date_labels = "%Y-%m")
  
  series <- monthly_metrics |>
    dplyr::mutate(series = factor(ifelse(product == "FAO DST", "FAO DST", paste0("Model, window ending day ", forecast_interval)), levels = series_levels)) |>
    tidyr::complete(series, target_month = all_months)
  
  ## Months with outbreaks, shaded in every panel and labelled with their outbreak hex count
  outbreak_months <- monthly_metrics |>
    dplyr::filter(product == "FAO DST", n_outbreak_hexes > 0) |>
    dplyr::distinct(target_month, n_outbreak_hexes)
  shade <- ggplot2::geom_rect(data = outbreak_months, ggplot2::aes(xmin = target_month - 14, xmax = target_month + 14, ymin = -Inf, ymax = Inf)
                              , inherit.aes = FALSE, fill = "grey90")
  
  ## One metric over time, with an optional dashed no-skill reference line
  panel <- function(col, ylab, title, ref = NULL) {
    
    ggplot2::ggplot(series, ggplot2::aes(target_month, .data[[col]], colour = series, shape = series)) +
      shade +
      (if (!is.null(ref)) ggplot2::geom_hline(yintercept = ref, linetype = "dashed", colour = "grey50")) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(size = 1.5, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = series_cols, name = NULL, drop = FALSE) +
      ggplot2::scale_shape_manual(values = series_shapes, name = NULL, drop = FALSE) +
      x_scale +
      ggplot2::labs(x = NULL, y = ylab, title = title) +
      ggplot2::theme_bw()
    
  }
  
  ## Outbreak hexes per month (square-root axis so single-outbreak months stay visible)
  counts <- ggplot2::ggplot(outbreak_months, ggplot2::aes(target_month, n_outbreak_hexes)) +
    ggplot2::geom_col(width = 25, fill = "grey40") +
    ggplot2::scale_y_sqrt() +
    x_scale +
    ggplot2::labs(x = NULL, y = "Outbreak hexes", title = "Hexes with an outbreak starting in the target month (these months are shaded below)") +
    ggplot2::theme_bw()
  
  extent <- panel("share_hexes_high", "Share of hexes high", "A. Extent of elevated risk (lower is better in months without outbreaks)") + scale_y_sqrt()
  hits   <- panel("share_outbreaks_high", "Share of outbreak\nhexes high", "B. Outbreak hexes flagged high (higher is better)")
  ranks  <- panel("mean_outbreak_rank_pct", "Mean percentile rank\nof outbreak hexes", "C. Rank of outbreak hexes among all hexes (1 = top; dashed = random)", ref = 0.5)
  peirce <- panel("peirce_skill", "Hit rate minus\nfalse-alarm rate", "D. High in outbreak hexes and low elsewhere (dashed = no skill)", ref = 0)
  
  ## Which product did better: by outbreak rank in outbreak months (R), by extent otherwise (E)
  winner <- model_vs_fao |>
    ggplot2::ggplot(ggplot2::aes(target_month, factor(forecast_interval, levels = windows), fill = model_advantage)) +
    ggplot2::geom_tile(width = 27, colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(outbreak_month, "R", "E")), size = 2.8) +
    ggplot2::scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0, name = "Model advantage\n(blue = model better,\nred = FAO better)") +
    x_scale +
    ggplot2::labs(x = "Target month", y = "Model window\n(upper bound, days)"
                  , title = "E. Which product did better (R: mean outbreak rank, in outbreak months; E: share of hexes high, in months without)") +
    ggplot2::theme_bw()
  
  patchwork::wrap_plots(counts, extent, hits, ranks, peirce, winner, ncol = 1, heights = c(0.5, 1, 1, 1, 1, 0.9), guides = "collect") &
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10))
  
}

## Save tables (csv), figures (png) and everything (qs) in out_dir
save_fao_comparison <- function(out, out_dir) {
  
  for (d in c("tables", "figures")) dir.create(file.path(out_dir, d), showWarnings = FALSE, recursive = TRUE)
  
  purrr::iwalk(out$tables, ~ utils::write.csv(.x, file.path(out_dir, "tables", paste0(.y, ".csv")), row.names = FALSE))
  
  ## Maps get a wide canvas; patchwork objects are ggplots, so ggsave handles both
  save_one <- function(p, stub, width = 11, height = 6) {
    ggplot2::ggsave(file.path(out_dir, "figures", paste0(stub, ".png")), p, width = width, height = height, dpi = 110)
  }
  
  save_one(out$plots$model_by_fao_class, "model_by_fao_class")
  save_one(out$plots$monthly_comparison, "monthly_comparison", width = 13, height = 15)
  purrr::iwalk(out$plots$maps, ~ save_one(.x, paste0("map_", .y), width = 14, height = 8))
  
  qs::qsave(out, file.path(out_dir, "results.qs"))
  invisible(out_dir)
  
}

## Plain-language description of every column in the monthly tables
fao_comparison_column_guide <- function(top_share) {
  
  top_lab <- paste0("top ", round(100 * top_share), "%")
  
  tibble::tribble(
    ~column,                  ~meaning
    , "target_month",           "Calendar month being compared: the FAO map for this month, and model windows that target it (window k issued from base dates in the month k months earlier)"
    , "product",                "FAO DST, or Model (one row per model window)"
    , "forecast_interval",      "Model window by its upper bound in days (30 = days 1-30 = issued 1 month before the target month, 60 = 2 months before, ...); NA for FAO"
    , "n_hexes",                "Hexes compared that month"
    , "n_outbreak_hexes",       "Hexes with an outbreak (cases > 0) starting in the target month"
    , "high_threshold",         "Model probability at or above which a hex is 'high' (set so the model flags the same total share of hex-months as FAO over all months compared)"
    , "share_hexes_high",       "Share of all hexes flagged high that month: how much of Africa is called elevated risk. In months without outbreaks, lower is better"
    , "top_cutoff_score",       paste0("Score of the hex at the ", top_lab, " boundary: model probability, or FAO share of hex area at current-month risk. Units differ between products, so compare only over time within a product")
    , "share_outbreaks_high",   "Share of outbreak hexes that were flagged high (hit rate)"
    , "share_outbreaks_top",    paste0("Share of outbreak hexes ranked in the ", top_lab, " of hexes that month")
    , "mean_outbreak_rank_pct", "Mean percentile rank of outbreak hexes among all hexes that month (1 = highest risk hex, 0.5 = what random ranking gives). FAO ties (e.g., all hexes with no risk) share their average rank"
    , "share_other_hexes_high", "Share of hexes without an outbreak that were flagged high (false-alarm rate)"
    , "peirce_skill",           "share_outbreaks_high minus share_other_hexes_high: high in outbreak hexes and low elsewhere (0 = no skill, 1 = perfect)"
    , "fao_* / model_*",        "In monthly_model_vs_fao: the metric above for FAO and for the model at that window"
    , "better",                 "Which product did better that month: by mean_outbreak_rank_pct in months with outbreaks, by share_hexes_high (lower wins) in months without"
    , "model_advantage",        "The model's margin on the deciding metric (> 0 = model better)"
  )
  
}
