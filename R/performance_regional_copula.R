#' Up-scale hex predictions to regions using a fitted Gaussian copula
#'
#' Turns hex-level outbreak probabilities into the probability of at least one outbreak in
#' each larger region (e.g., aggregated ADM2 regions), under five aggregation rules, and scores
#' them out of sample against outbreaks observed in each region:
#'  - p_mean:   area-weighted mean of hex probabilities (NOT a P(any); reference only)
#'  - p_max:    largest piece probability (perfect dependence; lower bound on P(any))
#'  - p_indep:  1 - prod(1 - q) (independence; upper bound under positive dependence)
#'  - p_copula: one-factor Gaussian copula with latent correlation rho fitted on regional
#'              outcomes (rho = 0 gives p_indep, rho -> 1 gives p_max)
#'  - p_recal:  complementary log-log recalibration of the summed hazard (P = 1 - exp(-e^a S^b))
#'
#' Hexes split across regions contribute pieces: the part of hex i covering a share w_i of the
#' hex keeps q_i = 1 - (1 - p_i)^w_i (risk spread evenly within the hex). 
#'
#' rho and the recalibration are fitted per forecast window by maximum likelihood with
#' leave-one-year-out cross-validation across all evaluation sets pooled. For speed the copula
#' likelihood is estimated on all multi-piece rows with an event or with a summed hazard above
#' `keep_hazard`, plus a weighted random sample (`sample_frac`) of the remaining multi-piece
#' rows; single-piece rows do not depend on rho and are left out of the rho fit.
#'
#' @title evaluate_regional_copula
#' @param hex_preds Tibble with eval_set, date, shapeName, forecast_interval and prob_pred
#' @param hexed_outbreaks Tibble saved by export_hexed_outbreaks() (needs longitude/latitude)
#' @param region_overlap Output of build_hex_region_overlap()
#' @param forecast_windows Tibble with forecast_interval and window_lower
#' @param rho_grid Latent correlations considered for the copula
#' @param latent_step Step of the integration grid over the shared latent factor (keep it
#'   well below sqrt(1 - max(rho_grid)) so the integrand is resolved at high rho)
#' @param min_coverage Minimum share of a region's area covered by predicted hexes
#' @param keep_hazard Multi-piece rows with summed hazard at or above this are always kept
#'   in the rho likelihood
#' @param sample_frac Share of the remaining multi-piece rows sampled for the rho likelihood
#' @param seed Random seed for the sampling
#' @return List with region_rows (every region-date-window row with all five probabilities
#'   from the full-data fit), oof (out-of-fold rows used for scoring), params_full,
#'   params_by_fold, scores (by eval_set, method and window), scores_by_size,
#'   rare_event_scores (information gain, N-test and Molchan skill by method),
#'   region_outbreaks (labelled outbreaks placed in regions) and region_overlap
#' @author Morgan Kain
#' @export
evaluate_regional_copula <- function(
    hex_preds
  , hexed_outbreaks
  , region_overlap
  , forecast_windows
  , rho_grid     = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
  , latent_step  = 0.05
  , min_coverage = 0.5
  , keep_hazard  = 1e-3
  , sample_frac  = 0.05
  , seed         = 10001
) {

  set.seed(seed)
  latent_grid <- seq(-7, 7, by = latent_step)

  
  #################################################################################
  #### 1) Hex-region pieces and one row per region-date-window --------------------
  #################################################################################
  
  pieces <- build_region_pieces(hex_preds, region_overlap$overlap)

  ## Labelled outbreaks placed in regions once; reused for the labels, the region context and the maps
  region_outbreaks <- place_outbreaks_in_regions(hexed_outbreaks, region_overlap$regions_ea)

  ## Overall: one row per eval_set -by- region -by- base date -by- forecast window, summarizing the hex pieces
   ## that make up the region (labels, the region size and the copula / recalibration columns are joined on below)
  ## Column Details:
   ## coverage          -- share of the region's area covered by predicted hexes (sum of adm_frac); rows below min_coverage are not scored
   ## n_pieces          -- number of hex pieces in the region; single-piece rows do not depend on rho
   ## hex_equiv         -- size of the covered part of the region in hex equivalents (sum of hex_frac)
   ## hazard            -- summed piece hazard, sum of -hex_frac * log(1 - p); the input to the cloglog recalibration
   ## p_mean            -- area-weighted mean of the FULL hex probabilities (weights = adm_frac): the risk of a typical spot in
   ##                      the region, NOT the probability of any outbreak in it. Usually below p_max (an average cannot exceed
   ##                      the largest value averaged), but because it uses full hex probabilities rather than the shrunken piece
   ##                      probabilities it can land between, or even above, p_max and p_indep when a high-risk hex only clips
   ##                      the region; its position relative to the bounds reflects geometry, not dependence. Reference only
   ## p_max             -- largest piece probability, piece_q = 1 - (1 - p)^hex_frac; P(any outbreak) if all pieces always have
   ##                      outbreaks together (perfect dependence), and the lower bound on P(any)
   ## p_indep           -- 1 - prod(1 - piece_q) = 1 - exp(-hazard); P(any outbreak) if pieces are independent, and the upper bound
   ##                      on P(any) when outbreaks are positively associated. The fitted copula (p_copula) lies between p_max and p_indep
  region_rows <- summarize_region_rows(pieces) |>
    dplyr::left_join(
      label_regions_from_outbreaks(region_outbreaks, hex_preds, forecast_windows)
    , by = c("eval_set", "area_index", "date", "forecast_interval")) |>
    dplyr::mutate(
      true_out = tidyr::replace_na(obs_out, 0)
    , year     = lubridate::year(date)
    ) |>
    dplyr::select(-obs_out) |>
    dplyr::left_join(
      region_overlap$regions_ea |> sf::st_drop_geometry() |> dplyr::select(area_index, region_hex_equiv = hex_equiv)
    , by = "area_index")

  ## Size classes by quartile of region size (hex equivalents) across regions
  size_breaks <- unique(stats::quantile(region_overlap$regions_ea$hex_equiv, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
  
  region_rows <- region_rows |> dplyr::mutate(size_class = cut(region_hex_equiv, size_breaks, include.lowest = TRUE, dig.lab = 3))

  ## Copula probabilities need piece latent thresholds aligned with rows
  pieces$z <- stats::qnorm(pieces$piece_q, lower.tail = FALSE)

  eval_rows <- region_rows |> dplyr::filter(coverage >= min_coverage)

  
  #################################################################################
  #### 2) Rows used to estimate rho (weighted sample of multi-piece rows) ---------
  #################################################################################
  
  est_rows <- eval_rows |>
    dplyr::filter(n_pieces > 1) |>
    dplyr::mutate(
      always = true_out == 1 | hazard >= keep_hazard
    , keep   = always | stats::runif(dplyr::n()) < sample_frac
    , weight = ifelse(always, 1, 1 / sample_frac)
    ) |>
    dplyr::filter(keep)

  ## Copula P(any) for the estimation rows at every rho on the grid
  est_p <- copula_p_for_rows(pieces, est_rows$row_id, rho_grid, latent_grid)

  ## Weighted log-likelihood contributions per row and rho
  est_ll <- purrr::map(est_p, ~ -est_rows$weight * rare_log_score_vec(.x, est_rows$true_out))

  
  #################################################################################
  #### 3) Fit rho and the recalibration: full data and leave-one-year-out ---------
  #################################################################################
  
  fit_one <- function(train_rows_est, train_rows_all) {
    rho_loglik <- vapply(est_ll, function(ll) sum(ll[train_rows_est]), numeric(1))
    recal      <- fit_regional_cloglog(train_rows_all)
    tibble::tibble(
      rho_hat     = rho_grid[which.max(rho_loglik)]
    , recal_a     = stats::coef(recal)[1]
    , recal_b     = stats::coef(recal)[2]
    , recal_b_se  = sqrt(diag(stats::vcov(recal)))[2]
    )
  }

  params_full <- purrr::map_dfr(forecast_windows$forecast_interval, function(fi) {
    fit_one(est_rows$forecast_interval == fi, eval_rows |> dplyr::filter(forecast_interval == fi)) |>
      dplyr::mutate(forecast_interval = fi, n_events = sum(eval_rows$true_out[eval_rows$forecast_interval == fi]), .before = 1)
  })

  params_by_fold <- tidyr::expand_grid(held_out_year = sort(unique(eval_rows$year)), forecast_interval = forecast_windows$forecast_interval) |>
    purrr::pmap_dfr(function(held_out_year, forecast_interval) {
      fit_one(
        est_rows$forecast_interval == forecast_interval & est_rows$year != held_out_year
      , eval_rows |> dplyr::filter(.data$forecast_interval == .env$forecast_interval, year != held_out_year)
      ) |>
        dplyr::mutate(held_out_year = held_out_year, forecast_interval = forecast_interval, .before = 1)
    })

  
  #################################################################################
  #### 4) Out-of-fold predictions for every evaluation row ------------------------
  #################################################################################
  
  oof <- eval_rows |>
    dplyr::left_join(params_by_fold, by = c("year" = "held_out_year", "forecast_interval")) |>
    dplyr::mutate(p_recal = -expm1(-exp(recal_a) * pmax(hazard, 1e-12)^recal_b))
  oof$p_copula <- copula_p_at_row_rho(pieces, oof, latent_grid)

  ## Full-data fit applied to every region row (the regional product)
  region_rows <- region_rows |>
    dplyr::left_join(params_full |> dplyr::select(-n_events), by = "forecast_interval") |>
    dplyr::mutate(p_recal = -expm1(-exp(recal_a) * pmax(hazard, 1e-12)^recal_b))
  region_rows$p_copula <- copula_p_at_row_rho(pieces, region_rows, latent_grid)

  
  #################################################################################
  #### 5) Out-of-sample scores ----------------------------------------------------
  #################################################################################
  
  oof_long <- oof |>
    tidyr::pivot_longer(dplyr::all_of(names(regional_method_labels())), names_to = "method", values_to = "prob_pred") |>
    dplyr::mutate(method = factor(regional_method_labels()[method], levels = regional_method_labels()))

  scores <- oof_long |>
    dplyr::group_by(eval_set, method, forecast_interval) |>
    dplyr::group_modify(~ score_regional_method(.x)) |>
    dplyr::ungroup()

  scores_by_size <- oof_long |>
    dplyr::group_by(eval_set, method, size_class) |>
    dplyr::group_modify(~ score_regional_method(.x)) |>
    dplyr::ungroup()

  rare_event_scores <- oof_long |>
    dplyr::group_by(eval_set, method, forecast_interval) |>
    dplyr::group_modify(function(d, k) {
      cu <- rare_molchan_curve(d$prob_pred, d$true_out)
      dplyr::bind_cols(
        rare_basic_scores(d$prob_pred, d$true_out)
      , rare_n_test(d$prob_pred, d$true_out) |> dplyr::select(ratio_obs_exp, z_poisson_binomial)
      , tibble::tibble(ASS_tau_0.05 = rare_area_skill_score(cu, 0.05), ASS_full = rare_area_skill_score(cu, 1))
      )
    }) |>
    dplyr::ungroup()

  list(
    region_rows       = region_rows |> dplyr::select(-row_id)
  , oof               = oof |> dplyr::select(-row_id)
  , params_full       = params_full
  , params_by_fold    = params_by_fold
  , scores            = scores
  , scores_by_size    = scores_by_size
  , rare_event_scores = rare_event_scores
  , region_outbreaks  = region_outbreaks
  , region_overlap    = region_overlap)

}

#' Figures for evaluate_regional_copula() output
#'
#' @title plot_regional_copula
#' @param regional Output of evaluate_regional_copula()
#' @param eval_set_to_plot Evaluation set drawn in the reliability and size figures
#' @return Named list of ggplots: reliability, obs_exp_by_size, rho_by_year (maps are made by
#'   plot_regional_masked_forecasts())
#' @author Morgan Kain
#' @export
plot_regional_copula <- function(regional, eval_set_to_plot) {

  plots <- list()

  ## Reliability on log axes, pooled over windows
  reliability_bins <- c(0, 1e-4, 3e-4, 1e-3, 3e-3, 1e-2, 3e-2, 0.1, 0.3, 1)
  plots$reliability <- regional$oof |>
    dplyr::filter(eval_set == eval_set_to_plot) |>
    tidyr::pivot_longer(dplyr::all_of(names(regional_method_labels())), names_to = "method", values_to = "prob_pred") |>
    dplyr::mutate(
      method = factor(regional_method_labels()[method], levels = regional_method_labels())
    , bin    = cut(prob_pred, reliability_bins, include.lowest = TRUE)) |>
    dplyr::summarize(n = dplyr::n(), pred = mean(prob_pred), obs = mean(true_out), n_events = sum(true_out), .by = c(method, bin)) |>
    dplyr::filter(n_events > 0) |>
    ggplot2::ggplot(ggplot2::aes(pred, obs)) +
    ggplot2::geom_abline(linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(ggplot2::aes(size = n)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~method, nrow = 1) +
    ggplot2::labs(x = "Predicted P(outbreak in region)", y = "Observed rate", size = "Region-date rows"
                  , title = "Regional reliability, leave-one-year-out predictions") +
    ggplot2::theme_bw()

  ## Observed / expected events by region size: 1 is perfect, < 1 overprediction
  plots$obs_exp_by_size <- regional$scores_by_size |>
    dplyr::filter(eval_set == eval_set_to_plot) |>
    ggplot2::ggplot(ggplot2::aes(size_class, obs_over_exp, colour = method, group = method)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Region size (hex equivalents, quartiles)", y = "Observed / expected events", colour = NULL) +
    ggplot2::theme_bw()

  ## Stability of the fitted correlation across held-out years
  plots$rho_by_year <- regional$params_by_fold |>
    ggplot2::ggplot(ggplot2::aes(held_out_year, rho_hat, colour = factor(forecast_interval))) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x = "Held-out year", y = "Fitted latent correlation (rho)", colour = "Forecast\ninterval") +
    ggplot2::theme_bw()

  plots

}

## Display names of the five aggregation rules
regional_method_labels <- function() {
  c(
    p_mean   = "Area-weighted mean"
  , p_max    = "Max (perfect dependence)"
  , p_indep  = "Independence"
  , p_copula = "Gaussian copula (fitted rho)"
  , p_recal  = "cloglog recalibration"
  )
}

## One row per eval_set -by- hex -by- region -by- date -by- window piece
 ## with its hazard and probability
build_region_pieces <- function(hex_preds, overlap) {

  hex_preds |>
    dplyr::select(eval_set, date, shapeName, forecast_interval, prob_pred) |>
    dplyr::inner_join(
      overlap |> dplyr::select(shapeName, area_index, hex_frac, adm_frac)
    , by = "shapeName", relationship = "many-to-many") |>
    dplyr::mutate(
      piece_hazard = -hex_frac * log1p(-pmin(prob_pred, 1 - 1e-12))
    , piece_q      = -expm1(-piece_hazard)
    ) |>
    dplyr::arrange(eval_set, area_index, date, forecast_interval) |>
    ## Integer row id shared by all pieces of a region-date-window row
    dplyr::mutate(row_id = as.integer(vctrs::vec_group_id(dplyr::pick(eval_set, area_index, date, forecast_interval))))

}

## Parameter-free aggregations, one row per region-date-window
summarize_region_rows <- function(pieces) {

  out <- pieces |>
    dplyr::summarize(
      coverage  = sum(adm_frac)
    , n_pieces  = dplyr::n()
    , hex_equiv = sum(hex_frac)
    , hazard    = sum(piece_hazard)
    , p_mean    = sum(adm_frac * prob_pred) / sum(adm_frac)
    , p_max     = max(piece_q)
    , p_indep   = -expm1(-sum(piece_hazard))
    , .by = c(row_id, eval_set, area_index, date, forecast_interval)) |>
    dplyr::arrange(row_id)

  ## Pieces and rows must line up on row_id for the copula sums
  stopifnot(identical(out$row_id, seq_len(nrow(out))))

  out

}

## Region-level outcomes from outbreak points, restricted to each set's predicted hexes
label_regions_from_outbreaks <- function(region_outbreaks, hex_preds, forecast_windows) {

  ## Per evaluation set: its base dates and the hexes it predicts
  sets <- hex_preds |>
    dplyr::distinct(eval_set, date, shapeName)

  purrr::map_dfr(unique(sets$eval_set), function(es) {
    
    set_dates <- unique(sets$date[sets$eval_set == es])
    set_hexes <- unique(sets$shapeName[sets$eval_set == es])
    
    tidyr::expand_grid(date = set_dates, forecast_windows) |>
      dplyr::cross_join(region_outbreaks |> dplyr::filter(shapeName %in% set_hexes)) |>
      dplyr::filter(start_date >= date + window_lower, start_date < date + forecast_interval) |>
      dplyr::distinct(area_index, date, forecast_interval) |>
      dplyr::mutate(eval_set = es, obs_out = 1)
    
  })

}

## Labelled outbreaks (cases > 0) placed in regions by their raw coordinates: one row per
## outbreak and region with area_index, shapeName (the outbreak's labelled hex), outbreak_id,
## start_date and cases
place_outbreaks_in_regions <- function(hexed_outbreaks, regions_ea) {

  hexed_outbreaks |>
    dplyr::filter(counts_toward_label == 1) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(regions_ea)) |>
    sf::st_join(regions_ea |> dplyr::select(area_index), left = FALSE) |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::select(area_index, shapeName, outbreak_id, start_date, cases)

}

## Copula P(any) for selected rows at each rho in `rhos`
copula_p_for_rows <- function(pieces, row_ids, rhos, latent_grid) {

  ## Pieces of the selected rows, re-indexed to positions in row_ids
  sel   <- pieces$row_id %in% row_ids
  local <- match(pieces$row_id[sel], row_ids)
  z     <- pieces$z[sel]

  purrr::map(rlang::set_names(rhos, paste0("rho_", rhos)), function(rho) {
    copula_p_any(z, local, length(row_ids), rho, latent_grid)
  })

}

## Copula P(any) for rows that each carry their own rho_hat column
copula_p_at_row_rho <- function(pieces, rows, latent_grid) {

  out <- rows$p_indep
  
  ## Single-piece rows do not depend on rho; compute the rest once per distinct rho
  multi <- rows$n_pieces > 1
  
  for (rho in unique(rows$rho_hat[multi])) {
    which_rows <- which(multi & rows$rho_hat == rho)
    out[which_rows] <- copula_p_for_rows(pieces, rows$row_id[which_rows], rho, latent_grid)[[1]]
  }
  
  out

}

## P(any) under an exchangeable Gaussian copula: 
## conditional on the shared factor W = w the pieces are independent
## so P(none | w) = prod_i pnorm((z_i - sqrt(rho) w) / sqrt(1 - rho))
## and P(any) = E_W[1 - P(none | W)], integrated over latent_grid
copula_p_any <- function(z, row_index, n_rows, rho, latent_grid) {

  if (rho == 0) {
    return(-expm1(as.vector(rowsum(stats::pnorm(z, log.p = TRUE), row_index, reorder = TRUE))[seq_len(n_rows)]))
  }

  grid_wts <- stats::dnorm(latent_grid) / sum(stats::dnorm(latent_grid))
  p_any    <- numeric(n_rows)

  for (k in seq_along(latent_grid)) {
    ## log P(none | w_k) per row, summed over its pieces
    log_none <- as.vector(rowsum(
      stats::pnorm((z - sqrt(rho) * latent_grid[k]) / sqrt(1 - rho), log.p = TRUE)
    , row_index, reorder = TRUE))
    p_any <- p_any + grid_wts[k] * -expm1(log_none)
  }

  p_any

}

## Complementary log-log recalibration of the summed hazard
fit_regional_cloglog <- function(rows) {
  
  stats::glm(
    true_out ~ log_hazard
  , family = stats::binomial(link = "cloglog")
  , data   = rows |> dplyr::mutate(log_hazard = log(pmax(hazard, 1e-12))))
  
}

## Forecast-centric scores for one method plus observed / expected events
score_regional_method <- function(df, min_events_for_auc = 5) {
  
  score_group(df, min_events_for_auc = min_events_for_auc) |>
    dplyr::mutate(
      expected_events = sum(df$prob_pred)
    , obs_over_exp    = sum(df$true_out) / sum(df$prob_pred))
  
}
