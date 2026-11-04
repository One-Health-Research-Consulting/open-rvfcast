#' Hex forecast maps masked by hex context
#'
#' Hex-level counterpart of plot_regional_masked_forecasts(): maps the hex probability of an
#' outbreak (prob_pred, no regional up-scaling) for chosen base dates, one map per forecast
#' window. Because most hexes have no outbreaks in their evaluation period, the marks differ from
#' the regional maps: those hexes are drawn with a faded (transparent) fill rather than
#' stippling, "Low Forecast Skill" hexes are hatched, outline width shows prior outbreak history,
#' and hexes with an outbreak in the window (true_out = 1) get a dashed red outline.
#'
#' @title plot_hex_masked_forecasts
#' @param hex_preds Hex-level predictions (eval_set, date, shapeName, forecast_interval,
#'   prob_pred, true_out)
#' @param hex_context hex_context element of build_hex_context()
#' @param region_hexes Hex grid (sf, or a list whose first element is one)
#' @param forecast_windows Tibble with forecast_interval and window_lower
#' @param eval_set_to_plot Evaluation set mapped
#' @param map_dates Base dates to map
#' @param history_linewidths Outline widths for the history_class levels, lowest to highest
#' @param faded_class quality_class drawn with a faded fill
#' @param faded_alpha Fill opacity of the faded_class hexes (1 = opaque)
#' @param hatch_class quality_class drawn with diagonal hatching
#' @param hatch_spacing_km Perpendicular spacing (km) of the hatch lines
#' @param hatch_colour Colour of the hatch lines
#' @param hatch_linewidth Width of the hatch lines
#' @param crs_ea Equal-area CRS the hatching is laid out in (so spacing is in km)
#' @param fill_limits Colour-scale limits for the probabilities; NULL uses the range of the mapped
#'   evaluation set (forecast maps pass the limits saved from the training run)
#' @return Nested named list of ggplots: one element per map date (named by date), each a list
#'   with one map per forecast window (named by the window's upper bound, e.g. "30")
#' @author Morgan Kain
#' @export
plot_hex_masked_forecasts <- function(
    hex_preds
  , hex_context
  , region_hexes
  , forecast_windows
  , eval_set_to_plot   = "test"
  , map_dates
  , history_linewidths = c(0.05, 0.3, 0.6, 1.1)
  , faded_class        = "No outbreaks in evaluation period"
  , faded_alpha        = 0.25
  , hatch_class        = "Low Forecast Skill"
  , hatch_spacing_km   = 50
  , hatch_colour       = "black"
  , hatch_linewidth    = 0.45
  , crs_ea             = sf::st_crs("+proj=cea +datum=WGS84 +units=m")
  , fill_limits        = NULL
) {

  ## region_hexes target wraps the sf object in a list
  if (!inherits(region_hexes, "sf")) {
    region_hexes <- region_hexes[[1]]
  }

  ## Hexes in lon/lat with their outline and fade columns
  hexes_ll <- region_hexes |>
    dplyr::select(shapeName) |>
    sf::st_transform(4326) |>
    dplyr::left_join(hex_context |> dplyr::select(shapeName, history_class, quality_class), by = "shapeName") |>
    ## Faded hexes vs all others (hexes without a class are left opaque)
    dplyr::mutate(faded = factor(ifelse(quality_class %in% faded_class, faded_class, "Other"), levels = c(faded_class, "Other")))

  ## Hatch lines for the hatched class only (static across dates and windows, so built once)
  hatch_lines <- make_quality_hatching(hexes_ll |> dplyr::select(shapeName), hex_context, crs_ea
                                       , rlang::set_names(hatch_spacing_km, hatch_class)
                                       , rlang::set_names("lines", hatch_class), id_col = "shapeName")$lines

  ## Colour range across every mapped probability in the evaluation set
  if (is.null(fill_limits)) {
    set_probs   <- hex_preds$prob_pred[hex_preds$eval_set == eval_set_to_plot]
    fill_limits <- range(set_probs[set_probs > 0], na.rm = TRUE)
  }

  purrr::map(rlang::set_names(as.character(map_dates)), function(d) {

    base_date <- as.Date(d)

    ## One copy of every hex per window, with that date's probabilities and labels (NA = not predicted)
    map_rows <- hex_preds |>
      dplyr::filter(eval_set == eval_set_to_plot, date == base_date) |>
      dplyr::select(shapeName, forecast_interval, prob_pred, true_out)

    map_sf <- tidyr::expand_grid(shapeName = hexes_ll$shapeName, forecast_interval = forecast_windows$forecast_interval) |>
      dplyr::left_join(map_rows, by = c("shapeName", "forecast_interval")) |>
      dplyr::left_join(hexes_ll, by = "shapeName") |>
      sf::st_as_sf()

    ## One map per forecast window, named by the window's upper bound
    purrr::pmap(forecast_windows, function(forecast_interval, window_lower) {
      plot_one_hex_masked_window(
        map_sf             = map_sf[map_sf$forecast_interval == forecast_interval, ]
      , hatch_lines        = hatch_lines
      , title              = paste0("Hex outbreak probability from base date ", d
                                    , ", days ", window_lower, "-", forecast_interval - 1)
      , fill_limits        = fill_limits
      , history_levels     = levels(hex_context$history_class)
      , history_linewidths = history_linewidths
      , faded_class        = faded_class
      , faded_alpha        = faded_alpha
      , hatch_class        = hatch_class
      , hatch_colour       = hatch_colour
      , hatch_linewidth    = hatch_linewidth)
    }) |>
      rlang::set_names(as.character(forecast_windows$forecast_interval))

  })

}


## One masked hex map for a single base date and forecast window
plot_one_hex_masked_window <- function(map_sf, hatch_lines, title, fill_limits, history_levels, history_linewidths
                                       , faded_class, faded_alpha, hatch_class, hatch_colour, hatch_linewidth) {

  ## Hexes with an outbreak in this window
  outbreak_hexes <- map_sf |> dplyr::filter(true_out == 1)
  outbreak_label <- "Outbreak in the window"
  has_outbreaks  <- nrow(outbreak_hexes) > 0
  has_hatching   <- nrow(hatch_lines) > 0

  ggplot2::ggplot() +
    ## Fill faded for the faded class; alpha affects the fill only, so history outlines stay solid
    ggplot2::geom_sf(data = map_sf, ggplot2::aes(fill = prob_pred, alpha = faded, linewidth = history_class), colour = "grey15") +
    ## Hatching over the fill (linetype mapped to the class only to give it a legend key)
    (if (has_hatching) ggplot2::geom_sf(data = hatch_lines, ggplot2::aes(linetype = quality_class)
                                        , colour = hatch_colour, linewidth = hatch_linewidth, key_glyph = "path")) +
    ## Dashed red outline on hexes with an outbreak in the window (colour mapped to a single
     ## label only to give the outline a legend key)
    (if (has_outbreaks) ggplot2::geom_sf(data = outbreak_hexes, ggplot2::aes(colour = outbreak_label), fill = NA
                                         , linetype = "dashed", linewidth = 0.5, key_glyph = "path")) +
    ## Shared colour limits so maps of different dates and windows are comparable; zero probabilities
     ## (log10 = -Inf, e.g. rounded forecasts) are squished to the lowest colour rather than drawn as missing
    ggplot2::scale_fill_viridis_c(trans = "log10", name = "P(outbreak)", na.value = "grey90", limits = fill_limits
                                  , oob = function(x, range) scales::squish(x, range, only.finite = FALSE)) +
    ggplot2::scale_alpha_manual(values = rlang::set_names(c(faded_alpha, 1), c(faded_class, "Other")), breaks = faded_class, name = NULL) +
    ggplot2::scale_colour_manual(values = rlang::set_names("red", outbreak_label), name = NULL) +
    ggplot2::scale_linetype_manual(values = rlang::set_names("solid", hatch_class), name = "Forecast quality") +
    ggplot2::scale_linewidth_manual(values = rlang::set_names(history_linewidths, history_levels)
                                    , name = "Outbreak history", drop = FALSE, na.value = history_linewidths[1]) +
    ## Legend order: probability, outbreak outline, the two quality keys together, then outbreak history.
     ## The faded key uses a mid-scale fill so its transparency is visible
    ggplot2::guides(
      fill      = ggplot2::guide_colourbar(order = 1)
    , colour    = if (has_outbreaks) ggplot2::guide_legend(order = 2) else "none"
    , linetype  = if (has_hatching) ggplot2::guide_legend(order = 3, override.aes = list(colour = hatch_colour, linewidth = 0.6)) else "none"
    , alpha     = ggplot2::guide_legend(order = 4, override.aes = list(fill = viridisLite::viridis(1, begin = 0.6), colour = NA))
    , linewidth = ggplot2::guide_legend(order = 5, override.aes = list(alpha = 1, fill = "grey90"))) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ## Explicit white background (theme_void leaves it transparent)
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))

}
