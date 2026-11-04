#' Per-region quality and context for masking regional forecast maps
#'
#' Builds one row per region (e.g., aggregated ADM2 regions) with the two pieces of context
#' used by plot_regional_masked_forecasts():
#'  - forecast skill: mean log-loss gain of the out-of-fold copula probabilities over a flat
#'    forecast (the eval set's regional outbreak rate for that window), pooled across windows.
#'    Skill is taken from the outer folds where the region has outer-fold rows (information
#'    available before the test period) and from the test period otherwise. Regions are ranked
#'    on it (1 = best; skill_rank) and classed into quality_class: "Demonstrated skill" (outbreaks
#'    in the scored rows and a gain > 0), "No outbreaks in evaluation period" (no outbreaks in the scored rows, so
#'    discrimination cannot be judged) or "Low Forecast Skill" (outbreaks and a gain <= 0). The class sets
#'    the map hatching
#'  - outbreak history: labelled outbreaks (cases > 0) that started up to history_end, placed in
#'    regions by their coordinates, binned into history_class
#' Test-period skill is also kept as a diagnostic column but does not feed the mask.
#'
#' @title build_region_context
#' @param regional Output of evaluate_regional_copula() (uses oof, region_outbreaks and region_overlap)
#' @param history_end Last date counted as "prior" outbreak history (the day before the test period)
#' @param skill_sets Evaluation sets used for the mask skill, in order of preference
#' @param history_breaks Breaks for binning prior outbreak counts into history_class
#' @param history_labels Labels for the history_class bins
#' @return Tibble with one row per region: area_index (and country / region when present),
#'   skill_source, n_rows, n_events, mean_logloss, mean_logloss_flat, logloss_gain, skill_rank,
#'   quality_class, test_logloss_gain (diagnostic only), n_prior_outbreaks, n_prior_years,
#'   last_prior_outbreak, n_prior_cases and history_class
#' @author Morgan Kain
#' @export
build_region_context <- function(
    regional
  , history_end
  , skill_sets     = c("outer_folds", "test")
  , history_breaks = c(-Inf, 0, 2, 10, Inf)
  , history_labels = c("No prior outbreaks", "1-2 prior outbreaks", "3-10 prior outbreaks", "> 10 prior outbreaks")
) {

  ## Log-loss gain over a flat forecast per region and evaluation set, then the mask skill
   ## (outer folds first) with its rank and quality class, and test-period skill alongside
  skill_by_set <- logloss_gain_by_unit(regional$oof, id_col = "area_index", prob_col = "p_copula")
  mask_skill   <- unit_mask_skill(skill_by_set, "area_index", skill_sets)

  ## One row per region with its skill and prior outbreak history
  regional$region_overlap$regions_ea |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::any_of(c("area_index", "country", "region"))) |>
    dplyr::left_join(mask_skill, by = "area_index") |>
    join_outbreak_history(regional$region_outbreaks, "area_index", history_end, history_breaks, history_labels)

}


#' Regional forecast maps masked by region context
#'
#' Maps the out-of-fold copula probability of at least one outbreak in each region for chosen
#' base dates (one map per forecast window). Marks drawn over the (unchanged) fill show the
#' region's quality_class: dense dark diagonal hatching for "Low Forecast Skill", light stippling (a dot
#' grid) for "No outbreaks in evaluation period" and none for "Demonstrated skill". Outline width shows prior outbreak
#' history (history_class). Outbreaks that started in each window are counted per region (distinct
#' labelled outbreaks, placed by their coordinates) and drawn as one red dot at a point inside the
#' region, with dot area proportional to the count on a scale shared by every map.
#'
#' @title plot_regional_masked_forecasts
#' @param regional Output of evaluate_regional_copula()
#' @param region_context Output of build_region_context()
#' @param forecast_windows Tibble with forecast_interval and window_lower
#' @param eval_set_to_plot Evaluation set mapped
#' @param map_dates Base dates to map
#' @param history_linewidths Outline widths for the history_class levels, lowest to highest
#' @param hatch_spacing_km Spacing (km) of the marks for each marked quality_class (perpendicular
#'   distance between lines, or distance between dots)
#' @param hatch_styles Mark style for each marked quality_class: "lines" (diagonal hatching) or
#'   "dots" (stippling)
#' @param hatch_colours Colours of the marks for each marked quality_class
#' @param n_top Number of regions with the highest probability on each map given a dashed red outline
#' @param fill_limits Colour-scale limits for the probabilities; NULL uses the range of the mapped
#'   evaluation set (forecast maps pass the limits saved from the training run)
#' @return Nested named list of ggplots: one element per map date (named by date), each a list
#'   with one map per forecast window (named by the window's upper bound, e.g. "30")
#' @author Morgan Kain
#' @export
plot_regional_masked_forecasts <- function(
    regional
  , region_context
  , forecast_windows
  , eval_set_to_plot   = "test"
  , map_dates
  , history_linewidths = c(0.05, 0.3, 0.6, 1.1)
  , hatch_spacing_km   = c("Low Forecast Skill" = 40, "No outbreaks in evaluation period" = 150)
  , hatch_styles       = c("Low Forecast Skill" = "lines", "No outbreaks in evaluation period" = "dots")
  , hatch_colours      = c("Low Forecast Skill" = "grey5", "No outbreaks in evaluation period" = "grey92")
  , n_top              = 10
  , fill_limits        = NULL
) {

  ## Regions in lon/lat with their outline column
  regions_ll <- regional$region_overlap$regions_ll |>
    dplyr::select(area_index) |>
    dplyr::left_join(region_context |> dplyr::select(area_index, history_class), by = "area_index")

  ## Hatch lines and stipple dots for the quality classes (static across dates and windows, so built once)
  hatching <- make_quality_hatching(regions_ll, region_context, sf::st_crs(regional$region_overlap$regions_ea), hatch_spacing_km, hatch_styles)

  crs_ea <- sf::st_crs(regional$region_overlap$regions_ea)

  ## One anchor point per region for the outbreak-count dots (point on surface is always inside
   ## the region, unlike the centroid of a concave or coastal region)
  region_anchors <- regions_ll |>
    dplyr::select(area_index) |>
    sf::st_transform(crs_ea) |>
    sf::st_set_agr("constant") |>
    sf::st_point_on_surface() |>
    sf::st_transform(4326)

  ## Distinct outbreaks starting in each region's window, for every mapped date (outbreaks placed
   ## in regions by their coordinates, as the regional labels are)
  outbreak_counts <- tidyr::expand_grid(date = as.Date(map_dates), forecast_windows) |>
    dplyr::cross_join(regional$region_outbreaks |> dplyr::select(area_index, outbreak_id, start_date)) |>
    dplyr::filter(start_date >= date + window_lower, start_date < date + forecast_interval) |>
    dplyr::summarize(n_outbreaks = dplyr::n_distinct(outbreak_id), .by = c(date, forecast_interval, area_index))

  ## Shared dot-size range so a given count is the same size on every map
  size_max <- max(c(1, outbreak_counts$n_outbreaks))

  ## Colour range across every mapped probability in the evaluation set
  if (is.null(fill_limits)) {
    fill_limits <- range(regional$oof$p_copula[regional$oof$eval_set == eval_set_to_plot & regional$oof$p_copula > 0], na.rm = TRUE)
  }

  purrr::map(rlang::set_names(as.character(map_dates)), function(d) {

    base_date <- as.Date(d)

    ## One copy of every region per window, with that date's probabilities (NA = not scored)
    map_rows <- regional$oof |>
      dplyr::filter(eval_set == eval_set_to_plot, date == base_date) |>
      dplyr::select(area_index, forecast_interval, p_copula)

    map_sf <- tidyr::expand_grid(area_index = regions_ll$area_index, forecast_interval = forecast_windows$forecast_interval) |>
      dplyr::left_join(map_rows, by = c("area_index", "forecast_interval")) |>
      dplyr::left_join(regions_ll, by = "area_index") |>
      sf::st_as_sf()

    ## Outbreak counts from this base date at their regions' anchor points
    window_counts <- region_anchors |>
      dplyr::inner_join(outbreak_counts |> dplyr::filter(date == base_date), by = "area_index")

    ## One map per forecast window, named by the window's upper bound
    purrr::pmap(forecast_windows, function(forecast_interval, window_lower) {
      plot_one_masked_window(
        map_sf             = map_sf[map_sf$forecast_interval == forecast_interval, ]
      , window_counts      = window_counts[window_counts$forecast_interval == forecast_interval, ]
      , size_max           = size_max
      , n_top              = n_top
      , hatching           = hatching
      , hatch_colours      = hatch_colours
      , title             = paste0("Regional outbreak probability from base date ", d
                                    , ", days ", window_lower, "-", forecast_interval - 1)
      , fill_limits        = fill_limits
      , history_levels     = levels(region_context$history_class)
      , history_linewidths = history_linewidths)
    }) |>
      rlang::set_names(as.character(forecast_windows$forecast_interval))

  })

}


## One masked regional map for a single base date and forecast window
plot_one_masked_window <- function(map_sf, window_counts, size_max, n_top, hatching, hatch_colours, title, fill_limits, history_levels, history_linewidths) {

  ## Regions with the highest probability on this map (unscored regions excluded)
  top_regions <- map_sf |>
    dplyr::filter(!is.na(p_copula)) |>
    dplyr::slice_max(p_copula, n = n_top, with_ties = FALSE)
  top_label   <- paste("Top", n_top, "regions by P(outbreak)")

  ## Mark colours of the classes present in each layer, in legend (factor level) order
  mark_colours <- function(x) unname(hatch_colours[levels(droplevels(x$quality_class))])
  has_lines    <- nrow(hatching$lines) > 0
  has_dots     <- nrow(hatching$dots) > 0
  ## Without outbreaks in the window the dot layer is empty and its legend would fall back to
   ## other layers' key glyphs, so the size legend is dropped and the subtitle says so instead
  has_counts   <- nrow(window_counts) > 0
  dot_note     <- if (has_counts) "red dot = outbreaks in the region in the window (area proportional to count)" else "no outbreaks in this window"

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = map_sf, ggplot2::aes(fill = p_copula, linewidth = history_class), colour = "grey15") +
    ## Quality marks over the fill, drawn thin / small so the fill colour stays readable. Lines and
     ## dots map different aesthetics (linetype, shape) so each gets its own legend key glyph
    (if (has_lines) ggplot2::geom_sf(data = hatching$lines, ggplot2::aes(linetype = quality_class)
                                     , colour = hatch_colours[as.character(hatching$lines$quality_class)], linewidth = 0.25, key_glyph = "path")) +
    (if (has_dots) ggplot2::geom_sf(data = hatching$dots, ggplot2::aes(shape = quality_class)
                                    , colour = hatch_colours[as.character(hatching$dots$quality_class)], size = 0.45
                                    , alpha = 0.5)) +
    ## Thin dashed red outline on the highest-probability regions (colour is mapped to a single
     ## label only to give the outline a legend key)
    ggplot2::geom_sf(data = top_regions, ggplot2::aes(colour = top_label), fill = NA, linetype = "dashed"
                     , linewidth = 0.35, key_glyph = "path") +
    ## One dot per region with outbreaks in the window; dot area proportional to the count
    ggplot2::geom_sf(data = window_counts, ggplot2::aes(size = n_outbreaks), shape = 21, fill = "red", colour = "white", stroke = 0.3) +
    ## Shared colour limits so maps of different dates and windows are comparable; zero probabilities
     ## (log10 = -Inf, e.g. rounded forecasts) are squished to the lowest colour rather than drawn as missing
    ggplot2::scale_fill_viridis_c(trans = "log10", name = "P(outbreak)", na.value = "grey90", limits = fill_limits
                                  , oob = function(x, range) scales::squish(x, range, only.finite = FALSE)) +
    ggplot2::scale_colour_manual(values = rlang::set_names("red", top_label), name = NULL) +
    ggplot2::scale_size_area(name = "Outbreaks in\nthe window", max_size = 6, limits = c(0, size_max)
                             , breaks = function(l) { b <- unique(round(pretty(c(1, l[2]), 4))); b[b >= 1] }) +
    ## Marked classes named after hatch_colours, so the legends follow the class labels used
    ggplot2::scale_linetype_manual(values = rlang::set_names(rep("solid", length(hatch_colours)), names(hatch_colours)), name = "Forecast quality") +
    ggplot2::scale_shape_manual(values = rlang::set_names(rep(16, length(hatch_colours)), names(hatch_colours)), name = NULL) +
    ggplot2::scale_linewidth_manual(values = rlang::set_names(history_linewidths, history_levels)
                                    , name = "Outbreak history", drop = FALSE, na.value = history_linewidths[1]) +
    ## Legend order: probability, the two quality keys together, then outbreak history. Quality keys
     ## take the marks' own colours, on grey so the light stipple dots are visible
    ggplot2::guides(
      fill      = ggplot2::guide_colourbar(order = 1)
    , size      = if (has_counts) ggplot2::guide_legend(order = 2) else "none"
    , linetype  = if (has_lines) ggplot2::guide_legend(order = 3, override.aes = list(colour = mark_colours(hatching$lines), linewidth = 0.6)) else "none"
    , shape     = if (has_dots) ggplot2::guide_legend(order = 4, override.aes = list(colour = mark_colours(hatching$dots), size = 1.5)
                                                       , theme = ggplot2::theme(legend.key = ggplot2::element_rect(fill = "grey45", colour = NA))) else "none"
    , linewidth = ggplot2::guide_legend(order = 5)) +
    ggplot2::coord_sf(datum = NA) +
#    ggplot2::labs(title = title
#                  , subtitle = paste0("Forecast quality: diagonal hatching = low forecast skill, stippling = no outbreaks in evaluation period, no marks = demonstrated skill\n"
#                                      , "Outline width = prior outbreaks; ", dot_note)) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ## Explicit white background (theme_void leaves it transparent)
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))

}


## Diagonal hatch lines or stipple dots clipped to the regions of each marked quality class;
## returns a list of two lon/lat sf objects (lines, dots) with a quality_class column. Marks are
## laid out in the equal-area CRS so spacing is in km. id_col joins the shapes to the context
make_quality_hatching <- function(regions_ll, region_context, crs_ea, hatch_spacing_km, hatch_styles, id_col = "area_index") {

  hatched <- regions_ll |>
    dplyr::select(dplyr::all_of(id_col)) |>
    dplyr::inner_join(region_context |> dplyr::select(dplyr::all_of(id_col), quality_class), by = id_col) |>
    dplyr::filter(quality_class %in% names(hatch_spacing_km)) |>
    sf::st_transform(crs_ea)

  marks <- purrr::imap(hatch_spacing_km, function(spacing_km, cls) {
    area <- hatched |> dplyr::filter(quality_class == cls)
    if (!nrow(area)) return(NULL)
    ## One dissolved shape per class, then the marks inside it
    area <- sf::st_union(sf::st_make_valid(area))
    geom <- if (hatch_styles[[cls]] == "dots") {
      ## Square grid of dot centres, kept where they fall inside the class's regions
      pts <- sf::st_make_grid(area, cellsize = spacing_km * 1000, what = "centers")
      pts[lengths(sf::st_intersects(pts, area)) > 0]
    } else {
      sf::st_collection_extract(sf::st_intersection(diagonal_lines(sf::st_bbox(area), spacing_km * 1000, crs_ea), area), "LINESTRING")
    }
    sf::st_sf(quality_class = factor(cls, levels = levels(region_context$quality_class)), geometry = geom) |>
      sf::st_transform(4326)
  }) |>
    purrr::compact()

  ## Split by style so lines and dots can be drawn as separate layers
  list(
    lines = dplyr::bind_rows(marks[names(marks) %in% names(hatch_styles)[hatch_styles == "lines"]])
  , dots  = dplyr::bind_rows(marks[names(marks) %in% names(hatch_styles)[hatch_styles == "dots"]])
  )

}

## 45-degree lines covering a bounding box, spacing_m apart (measured perpendicular to the lines)
diagonal_lines <- function(bb, spacing_m, crs) {

  ## Lines y = x + offset; offsets step by spacing * sqrt(2) to give the perpendicular spacing
  offsets <- seq(bb[["ymin"]] - bb[["xmax"]], bb[["ymax"]] - bb[["xmin"]], by = spacing_m * sqrt(2))
  sf::st_sfc(
    lapply(offsets, function(o) sf::st_linestring(rbind(c(bb[["xmin"]], bb[["xmin"]] + o), c(bb[["xmax"]], bb[["xmax"]] + o))))
  , crs = crs)

}


## Mean log-loss gain of the mapped probabilities over a flat forecast, per unit (region or hex)
## and eval set (id_col / prob_col: area_index / p_copula for regions, shapeName / prob_pred for hexes)
logloss_gain_by_unit <- function(preds, id_col, prob_col) {

  preds |>
    dplyr::select(eval_set, dplyr::all_of(id_col), forecast_interval, p = dplyr::all_of(prob_col), true_out) |>
    ## Flat forecast: the eval set's outbreak rate for the window across all units
    dplyr::mutate(p_flat = mean(true_out), .by = c(eval_set, forecast_interval)) |>
    dplyr::mutate(
      logloss      = rare_log_score_vec(p, true_out)
    , logloss_flat = rare_log_score_vec(p_flat, true_out)
    ) |>
    ## Pooled across windows; positive gain = the model beat the flat forecast in the unit
    dplyr::summarize(
      n_rows            = dplyr::n()
    , n_events          = sum(true_out)
    , mean_logloss      = mean(logloss)
    , mean_logloss_flat = mean(logloss_flat)
    , logloss_gain      = mean(logloss_flat - logloss)
    , .by = c(eval_set, dplyr::all_of(id_col)))

}

## Mask skill per unit: the first set in skill_sets with scored rows for the unit, ranked (1 = largest
## gain over flat) and classed into quality_class (sets the map marks; without outbreaks, skill at
## catching them cannot be judged), with test-period skill kept as a diagnostic column
unit_mask_skill <- function(skill_by_set, id_col, skill_sets) {

  test_skill <- skill_by_set |>
    dplyr::filter(eval_set == "test") |>
    dplyr::select(dplyr::all_of(id_col), test_logloss_gain = logloss_gain)

  skill_by_set |>
    dplyr::filter(eval_set %in% skill_sets) |>
    dplyr::mutate(set_order = match(eval_set, skill_sets)) |>
    dplyr::slice_min(set_order, n = 1, by = dplyr::all_of(id_col)) |>
    dplyr::select(-set_order) |>
    dplyr::rename(skill_source = eval_set) |>
    dplyr::mutate(skill_rank = rank(-logloss_gain, ties.method = "min")) |>
    dplyr::mutate(quality_class = factor(dplyr::case_when(
      n_events == 0     ~ "No outbreaks in evaluation period"
    , logloss_gain > 0  ~ "Demonstrated skill"
    , TRUE              ~ "Low Forecast Skill")
    , levels = c("Demonstrated skill", "No outbreaks in evaluation period", "Low Forecast Skill"))) |>
    dplyr::left_join(test_skill, by = id_col)

}

## Add prior outbreak history (labelled outbreaks that started up to history_end) to a table of
## units, with the counts binned into history_class; outbreaks need id_col, start_date and cases
join_outbreak_history <- function(units, outbreaks, id_col, history_end, history_breaks, history_labels) {

  history <- outbreaks |>
    dplyr::filter(start_date <= history_end) |>
    dplyr::summarize(
      n_prior_outbreaks   = dplyr::n()
    , n_prior_years       = dplyr::n_distinct(lubridate::year(start_date))
    , last_prior_outbreak = max(start_date)
    , n_prior_cases       = sum(cases, na.rm = TRUE)
    , .by = dplyr::all_of(id_col))

  units |>
    dplyr::left_join(history, by = id_col) |>
    dplyr::mutate(
      dplyr::across(c(n_prior_outbreaks, n_prior_years, n_prior_cases), ~ tidyr::replace_na(.x, 0))
    , history_class = cut(n_prior_outbreaks, history_breaks, labels = history_labels)
    )

}
