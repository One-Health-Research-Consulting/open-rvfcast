#' Probabilistic scores for rare binary events
#'
#' Scores one or more competing forecast models/"models" (e.g., the xgboost predictions, a
#' flat, and a historical reference rate).
#'  - Brier score, Brier skill score, log loss, ignorance, information gain per event
#'  - CORP decomposition (miscalibration / discrimination / uncertainty) and reliability curve
#'  - CSEP-style N-test
#'  - information gain vs flat and vs the reference + Rhoades et al. (2011) T/W tests
#'  - Molchan diagram, area skill score, recall at 1% / 5% of space-time alarmed
#'  - SEDI / EDI at 1% alarmed
#'  - space-time block-bootstrap confidence intervals
#'
#' @title score_rare_event_forecasts
#' @param scoring_data Long tibble with eval_set, forecast_interval, model, date, location,
#'   x (longitude), y (latitude), predicted_probability and truth (0/1).
#' @param reference_model Name of the baseline model compared against (if present)
#' @param n_boot Block-bootstrap replicates (0 to skip)
#' @param bootstrap_sets eval_set values to bootstrap (bootstrapping is a bit slow)
#' @param block_tile_deg Bootstrap block size: tiles of X degrees -by- calendar year
#' @param n_boot_corp Resamples for the CORP reliability consistency band
#' @param seed Random seed for the bootstrap and CORP bands
#' @return List of tibbles (each with eval_set and forecast_interval columns): basic,
#'   corp_decomposition, n_test, information_gain, molchan, sedi, zero_smoothing, 
#'   bootstrap_ci, and curve tibbles for
#'   plotting (corp_curve, corp_band, molchan_curve)
#' @author Morgan Kain
#' @export
score_rare_event_forecasts <- function(
    scoring_data
  , reference_model = "reference"
  , n_boot          = 200
  , bootstrap_sets  = unique(scoring_data$eval_set)
  , block_tile_deg  = 5
  , n_boot_corp     = 50
  , seed            = 10001
) {

  set.seed(seed)

  ## One scoring pass per evaluation set and forecast window
  groups <- scoring_data |>
    dplyr::distinct(eval_set, forecast_interval) |>
    dplyr::arrange(eval_set, forecast_interval)

  per_group <- purrr::pmap(groups, function(eval_set, forecast_interval) {

    group_data <- scoring_data |>
      dplyr::filter(.data$eval_set == .env$eval_set, .data$forecast_interval == .env$forecast_interval)

    out <- score_rare_event_group(
      group_data      = group_data
    , reference_model = reference_model
    , n_boot          = if (eval_set %in% bootstrap_sets) n_boot else 0
    , block_tile_deg  = block_tile_deg
    , n_boot_corp     = n_boot_corp)
    
    ## Label every table with its group
    purrr::map(out, ~ dplyr::mutate(.x, eval_set = eval_set, forecast_interval = forecast_interval, .before = 1))

  })

  ## Stack each table across groups
  table_names <- names(per_group[[1]])
  purrr::map(rlang::set_names(table_names), function(nm) dplyr::bind_rows(purrr::map(per_group, nm)))

}


#' Figures for score_rare_event_forecasts() output
#'
#' @title plot_rare_event_scores
#' @param scores Output of score_rare_event_forecasts()
#' @param eval_set_to_plot Which evaluation set to draw
#' @return Named list of ggplots: corp, molchan, molchan_zoom
#' @author Morgan Kain
#' @export
plot_rare_event_scores <- function(scores, eval_set_to_plot) {

  ## Keep only the chosen evaluation set
  pick <- function(tb) dplyr::filter(tb, eval_set == eval_set_to_plot)

  plots <- list()

  ## CORP reliability diagram with its consistency band under calibration
  plots$corp <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = pick(scores$corp_band)
    , ggplot2::aes(p, ymin = pmax(lo, 1e-7), ymax = pmax(hi, 1e-7)), alpha = 0.25) +
    ggplot2::geom_step(
      data = pick(scores$corp_curve) |> dplyr::filter(p_recal > 0)
    , ggplot2::aes(p, p_recal), colour = "firebrick") +
    ggplot2::geom_abline(linetype = 2) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::facet_grid(model ~ forecast_interval) +
    ggplot2::labs(
      title = "CORP reliability diagrams (90% consistency band under perfect calibration)"
    , x = "Forecast probability", y = "Conditional event probability (PAV)") +
    ggplot2::theme_bw()

  ## Molchan diagram: miss rate against the fraction of space-time alarmed
  molchan_plot <- function(xmax) {
    pick(scores$molchan_curve) |>
      ggplot2::ggplot(ggplot2::aes(tau, nu, colour = model)) +
      ggplot2::geom_abline(intercept = 1, slope = -1, linetype = 2) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(weighting ~ forecast_interval) +
      ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(0, 1)) +
      ggplot2::labs(
        x = "Fraction of space-time alarmed", y = "Miss rate"
      , title = sprintf("Molchan diagram (fraction alarmed <= %.2f); dashed = random", xmax)) +
      ggplot2::theme_bw()
  }
  plots$molchan      <- molchan_plot(1)
  plots$molchan_zoom <- molchan_plot(0.1)

  plots

}


## Score every model in one eval_set -by- forecast_interval group
score_rare_event_group <- function(group_data, reference_model, n_boot, block_tile_deg, n_boot_corp) {

  ## Align models on identical (date, location) rows
  aligned     <- group_data |> dplyr::arrange(model, date, location)
  model_names <- unique(aligned$model)
  key         <- aligned |> dplyr::filter(model == model_names[1])
  y           <- key$truth

  P <- rlang::set_names(model_names) |>
    purrr::map(~ aligned |> dplyr::filter(model == .x) |> dplyr::pull(predicted_probability))

  ## check to see if anything became unaligned
  stopifnot(all(purrr::map_lgl(model_names, ~ identical(
    aligned |> dplyr::filter(model == .x) |> dplyr::pull(truth), y))))

  ## Constant forecast (event rate of the scored data)
  P$flat <- rep(mean(y), length(y))

  ## Mix models that put exact zeros on rows with a little climatology,
   ## choosing the weight that maximizes the model's own log-likelihood
  smoothed <- smooth_zero_probabilities(P, y)
  P        <- smoothed$P

  scored  <- setdiff(names(P), "flat")
  has_ref <- reference_model %in% names(P)
  p_ref   <- if (has_ref) P[[reference_model]] else NULL
  p_clim  <- P$flat

  ## Every table below also gets eval_set and forecast_interval columns in score_rare_event_forecasts()
  tables <- list()

  ## Overall: Produces the "basic" rare event scores; one row per model, including the flat forecast
  ## Column Details: 
   ## model                    -- forecast being scored (e.g., xgboost, reference, flat)
   ## n_rows                   -- number of location-date rows scored
   ## n_events                 -- number of rows with an outbreak (truth == 1)
   ## brier                    -- mean squared difference between predicted probability and outcome (0 = perfect; lower is better)
   ## brier_skill_score        -- 1 - brier / (brier of the flat forecast); 0 = no better than flat, > 0 = better, 1 = perfect
   ## log_loss_nats            -- mean negative log-likelihood per row, natural log (lower is better)
   ## ignorance_bits           -- log_loss_nats expressed in bits (base-2 log)
   ## info_gain_nats_per_event -- total log-likelihood gain over the flat forecast divided by the number
   ##                             of events, in nats (> 0 = better than flat)
   ## prob_gain_per_event      -- exp(info_gain_nats_per_event): how many times more probability the model put on the
   ##                             outbreaks that happened than the flat forecast did, after paying for its false alarms
  tables$basic <- purrr::imap_dfr(P, ~ rare_basic_scores(.x, y) |> dplyr::mutate(model = .y, .before = 1))

  ## CORP decomposition: score = miscalibration - discrimination + uncertainty
  ## Overall: CORP decomposition (Dimitriadis et al. 2021) of the Brier and log scores; one row per model -by- score (flat excluded).
   ## The isotonic (PAV) recalibration of a forecast is the best-calibrated forecast with the same ranking
  ## Column Details: 
   ## model              -- forecast being scored
   ## score              -- which score is decomposed ("brier" or "log")
   ## mean_score         -- mean score of the forecast as issued (lower is better)
   ## MCB_miscalibration -- score lost to miscalibration: mean_score minus the score after PAV recalibration (0 = perfectly calibrated; lower is better)
   ## DSC_discrimination -- score gained from discrimination: flat forecast's score minus the recalibrated 
   ##                       score (higher is better; 0 = cannot separate events from non-events)
   ## UNC_uncertainty    -- score of the flat forecast; the inherent difficulty of the data (identical for every model)
  tables$corp_decomposition <- purrr::imap_dfr(P[scored], ~ dplyr::bind_rows(
    rare_corp_decomposition(.x, y, "brier"), rare_corp_decomposition(.x, y, "log")) |>
      dplyr::mutate(model = .y, .before = 1))

  ## Expected number to occur based on predicted probabilities vs true number that occurred
  ## Overall: CSEP-style N-test (calibration in the large); one row per model, including flat
  ## Column Details: 
   ## model              -- forecast being scored
   ## observed_events    -- number of outbreaks that happened
   ## expected_events    -- sum of predicted probabilities (the number the forecast implied)
   ## ratio_obs_exp      -- observed_events / expected_events (1 = right overall level; < 1 = over-prediction; > 1 = under-prediction)
   ## delta1_P_N_ge_obs  -- Poisson probability of at least the observed count given the expected count; small = the model under-predicts
   ## delta2_P_N_le_obs  -- Poisson probability of at most the observed count; small = the model over-predicts
   ## z_poisson_binomial -- (observed - expected) / Poisson-binomial standard deviation; |z| above about 2 flags large miscalibration 
  tables$n_test <- purrr::imap_dfr(P, ~ rare_n_test(.x, y) |> dplyr::mutate(model = .y, .before = 1))

  ## Gain in predictive power vs chosen baseline "models"
  ## Overall: information gain of each model over a comparison forecast (flat, and the reference when present),
   ## with Rhoades et al. (2011) significance tests; one row per model -by- comparison
  ## Column Details: 
   ## model               -- forecast being scored
   ## versus              -- comparison forecast ("flat" or the reference model)
   ## IG_nats_per_event   -- (log-likelihood of model - log-likelihood of comparison) / number of events, in nats (> 0 favors the model)
   ## prob_gain_per_event -- exp(IG_nats_per_event): times more probability the model put on the outbreaks 
   ##                        than the comparison did, net of false alarms
   ## IG_rhoades          -- Rhoades et al. Poisson-approximation version of the information gain per event 
   ##                        (close to IG_nats_per_event when probabilities are small)
   ## T_stat              -- Rhoades T statistic: IG_rhoades divided by its standard error across events
   ## p_value_T           -- two-sided p-value of the T-test (small = the two forecasts differ in information)
   ## p_value_W           -- Wilcoxon signed-rank (W-test) p-value; a non-parametric check of the same comparison
  tables$information_gain <- dplyr::bind_rows(
    purrr::map_dfr(scored, ~ rare_info_gain_test(P[[.x]], p_clim, y) |>
                     dplyr::mutate(model = .x, versus = "flat", .before = 1))
  , if (has_ref) purrr::map_dfr(setdiff(scored, reference_model), ~ rare_info_gain_test(P[[.x]], p_ref, y) |>
                                  dplyr::mutate(model = .x, versus = reference_model, .before = 1))
  )

  ## Molchan diagram: miss rate against the fraction of space-time alarmed 
  ## Overall: Molchan-diagram summaries of ranking skill when the highest-risk share of space-time is alarmed; one row per model (flat excluded)
  ## Column Details: 
   ## model                 -- forecast being scored
   ## ASS_full              -- area skill score over the whole diagram: average hit rate across all alarm fractions (0.5 = random, 1 = perfect)
   ## ASS_tau_0.05          -- area skill score when at most 5% of space-time is alarmed (average hit rate over alarm fractions 0-5%)
   ## random_ASS_0.05       -- ASS_tau_0.05 expected from random alarms (0.025)
   ## recall_at_1pct        -- share of outbreaks inside the top 1% of rows by predicted probability
   ## recall_at_5pct        -- share of outbreaks inside the top 5% of rows
   ## ASS_full_ref_weighted -- area skill score with alarmed space-time weighted by the reference forecast's probabilities;
   ##                          0.5 = ranks no better than the reference, > 0.5 = adds ranking skill beyond it (NA for the reference itself)
  tables$molchan <- purrr::imap_dfr(P[scored], function(p, m) {
    cu <- rare_molchan_curve(p, y)
    tibble::tibble(
      model                 = m
      ## Area skill score up to tau_max (random expectation tau_max / 2)
    , ASS_full              = rare_area_skill_score(cu, 1)
    , ASS_tau_0.05          = rare_area_skill_score(cu, 0.05)
    , random_ASS_0.05       = 0.05 / 2
    , recall_at_1pct        = 1 - stats::approx(cu$tau, cu$nu, xout = 0.01, ties = "ordered", rule = 2)$y
    , recall_at_5pct        = 1 - stats::approx(cu$tau, cu$nu, xout = 0.05, ties = "ordered", rule = 2)$y
    , ASS_full_ref_weighted = if (has_ref && m != reference_model)
        rare_area_skill_score(rare_molchan_curve(p, y, weight = p_ref), 1) else NA_real_
    )
  })

  ## alarming at least `frac` of space-time
  ## Overall: extreme dependence indices (Ferro & Stephenson 2011), which stay informative for rare events, at the first
   ## threshold that alarms at least 1% of rows; one row per model (flat excluded)
  ## Column Details: 
   ## model      -- forecast being scored
   ## alarm_frac -- share of rows alarmed at that threshold (just over 0.01)
   ## H          -- hit rate: share of outbreak rows alarmed
   ## F          -- false-alarm rate: share of non-outbreak rows alarmed
   ## EDI        -- extreme dependence index (0 = random, 1 = perfect, < 0 = worse than random)
   ## SEDI       -- symmetric extreme dependence index; same scale as EDI and also robust to the base rate
  tables$sedi <- purrr::imap_dfr(P[scored], ~ rare_sedi_at_alarm_fraction(.x, y, 0.01) |>
                                   dplyr::mutate(model = .y, .before = 1))

  ## Overall: models that predicted exactly 0 on some rows, and how they were mixed with the flat rate before scoring
   ## (the log score of an outbreak at p = 0 is infinite); one row per such model, empty when no model has zeros
  ## Column Details: 
   ## model                -- forecast that had exact zeros
   ## share_rows_zero      -- share of rows with p = 0
   ## events_at_zero       -- outbreaks that happened on those rows
   ## share_events_at_zero -- events_at_zero / all outbreaks
   ## mixing_weight        -- weight w in p* = (1 - w) * p + w * flat rate, chosen to maximize the model's own log-likelihood
   ## floor_probability    -- smallest probability after mixing (w * flat rate)
  tables$zero_smoothing <- smoothed$table

  ## Overall: space-time block-bootstrap 95% intervals (blocks = block_tile_deg-degree tiles x calendar years, resampled
   ## with replacement); one row per model -by- statistic (flat excluded), empty when n_boot = 0
  ## Column Details: 
   ## model     -- forecast being scored
   ## statistic -- BSS (Brier skill score vs flat), IG_vs_clim (information gain per event vs flat, nats),
   ##              IG_vs_ref (information gain per event vs the reference, nats), ASS_tau_0.05 (area skill score at <= 5% alarmed),
   ##              obs_over_exp (observed / expected outbreaks)
   ## lo_2.5    -- 2.5th percentile across bootstrap replicates
   ## median    -- median across replicates
   ## hi_97.5   -- 97.5th percentile across replicates
  tables$bootstrap_ci <- if (n_boot > 0) {
    rare_block_bootstrap(P[scored], y, key, reference_model, n_boot, block_tile_deg)
  } else {
    tibble::tibble()
  }

  ## Curves for figures (CORP only for non-flat models)
  corp <- purrr::map(P[scored], ~ rare_corp_curve(.x, y, n_boot = n_boot_corp))
  
  ## Overall: CORP reliability-curve points for figures; one row per forecast value where the recalibrated value changes (flat excluded)
  ## Column Details: 
   ## p       -- forecast probability
   ## p_recal -- PAV-recalibrated probability: the event rate that forecast value actually corresponds to (on the 1:1 line = calibrated)
   ## model   -- forecast being scored
  tables$corp_curve <- purrr::imap_dfr(corp, ~ .x$curve |> dplyr::mutate(model = .y))
  
  ## Overall: consistency band around the CORP curve for figures; one row per grid probability (flat excluded)
  ## Column Details: 
   ## p     -- grid of forecast probabilities
   ## lo    -- lower edge of the 90% band of recalibrated curves expected if the forecast were perfectly calibrated
   ##          (outcomes resampled as Bernoulli draws from the forecast itself)
   ## hi    -- upper edge of that band
   ## model -- forecast being scored
  tables$corp_band  <- purrr::imap_dfr(corp, ~ .x$band |> dplyr::mutate(model = .y))

  ## Overall: Molchan-diagram points for figures (corner points only); one row per threshold x model x weighting
  ## Column Details: 
   ## threshold -- probability threshold (alarm when p >= threshold)
   ## tau       -- share of space-time alarmed (or, when weighted by the reference, share of the reference's probability mass alarmed)
   ## nu        -- miss rate: share of outbreaks not alarmed
   ## model     -- forecast being scored
   ## weighting -- "unweighted" or "weighted by reference"
  tables$molchan_curve <- dplyr::bind_rows(
    purrr::imap_dfr(P, ~ rare_thin_molchan(rare_molchan_curve(.x, y)) |>
                      dplyr::mutate(model = .y, weighting = "unweighted"))
  , if (has_ref) purrr::imap_dfr(P, ~ rare_thin_molchan(rare_molchan_curve(.x, y, weight = p_ref)) |>
                                   dplyr::mutate(model = .y, weighting = "weighted by reference"))
  )
  
  tables

}


## Mix models with exact-zero probabilities with a little climatology
smooth_zero_probabilities <- function(P, y) {

  zero_models <- names(P)[vapply(P, function(p) any(p == 0) && any(p > 0), logical(1))]
  tab_zero    <- tibble::tibble()

  for (m in zero_models) {
    p_raw <- P[[m]]
    ## Negative log-likelihood as a function of log10(mixing weight)
    nll <- function(lw) sum(rare_log_score_vec((1 - 10^lw) * p_raw + 10^lw * mean(y), y))
    w   <- 10^stats::optimize(nll, c(-8, 0))$minimum
    P[[m]] <- (1 - w) * p_raw + w * mean(y)
    tab_zero <- dplyr::bind_rows(tab_zero, tibble::tibble(
      model = m, share_rows_zero = mean(p_raw == 0), events_at_zero = sum(y[p_raw == 0])
    , share_events_at_zero = sum(y[p_raw == 0]) / sum(y), mixing_weight = w
    , floor_probability = w * mean(y)))
  }

  list(P = P, table = tab_zero)

}

## Space-time block bootstrap for some of the rare-event statistics
rare_block_bootstrap <- function(P, y, key, reference_model, n_boot, block_tile_deg) {

  ## Blocks = spatial tiles -by- calendar years, so resampling respects clustering of events
  block_id   <- interaction(floor(key$x / block_tile_deg), floor(key$y / block_tile_deg)
                            , format(key$date, "%Y"), drop = TRUE)
  block_rows <- split(seq_along(y), block_id)
  has_ref    <- reference_model %in% names(P)
  p_ref      <- if (has_ref) P[[reference_model]] else NULL

  boot <- purrr::map_dfr(seq_len(n_boot), function(b) {
    idx <- unlist(block_rows[sample(length(block_rows), replace = TRUE)], use.names = FALSE)
    yy  <- y[idx]
    if (sum(yy) < 2) return(NULL)
    cl  <- rep(mean(yy), length(yy))
    purrr::imap_dfr(P, function(p, m) {
      pp <- p[idx]
      tibble::tibble(
        model        = m
        ## Brier skill score
      , BSS          = 1 - mean((pp - yy)^2) / mean((cl - yy)^2)
      , IG_vs_clim   = (sum(rare_log_score_vec(cl, yy)) - sum(rare_log_score_vec(pp, yy))) / sum(yy)
      , IG_vs_ref    = if (!has_ref || m == reference_model) NA_real_ else
          (sum(rare_log_score_vec(p_ref[idx], yy)) - sum(rare_log_score_vec(pp, yy))) / sum(yy)
      , ASS_tau_0.05 = rare_area_skill_score(rare_molchan_curve(pp, yy), 0.05)
      , obs_over_exp = sum(yy) / sum(pp)
      )
    })
  })

  boot |>
    tidyr::pivot_longer(-model, names_to = "statistic") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::summarise(
      lo_2.5  = stats::quantile(value, 0.025)
    , median  = stats::median(value)
    , hi_97.5 = stats::quantile(value, 0.975)
    , .by = c(model, statistic))

}

## Probabilities kept away from 0 and 1
rare_clip <- function(p, eps = 1e-12) pmin(pmax(p, eps), 1 - eps)

## Negative log-likelihood per row
rare_log_score_vec <- function(p, y) {
  p <- rare_clip(p)
  -(y * log(p) + (1 - y) * log1p(-p))
}

## Brier, log loss, ignorance and skill (to be compared between predictions and references)
rare_basic_scores <- function(p, y, clim = mean(y)) {
  n_ev     <- sum(y)
  bs       <- mean((p - y)^2)
  bs_c     <- mean((clim - y)^2)
  ll       <- mean(rare_log_score_vec(p, y))
  ll_c     <- mean(rare_log_score_vec(rep(clim, length(y)), y))
  ig_event <- length(y) * (ll_c - ll) / n_ev
  tibble::tibble(
    n_rows                   = length(y)
  , n_events                 = n_ev
  , brier                    = bs
  , brier_skill_score        = 1 - bs / bs_c
  , log_loss_nats            = ll
  , ignorance_bits           = ll / log(2)
  , info_gain_nats_per_event = ig_event
  , prob_gain_per_event      = exp(ig_event)
  )
}

## Isotonic (PAV) recalibration, pooling tied forecasts first
rare_pav_recalibrate <- function(p, y) {
  o   <- order(p)
  xs  <- p[o]
  grp <- cumsum(c(TRUE, diff(xs) != 0))
  n_g <- tabulate(grp)
  fit <- rare_weighted_pav(rowsum(y[o], grp)[, 1] / n_g, n_g)[grp]
  out <- numeric(length(p))
  out[o] <- fit
  out
}

## Weighted pool-adjacent-violators on values already ordered by the forecast
rare_weighted_pav <- function(v, w) {
  n   <- length(v)
  val <- numeric(n); wt <- numeric(n); len <- integer(n)
  k   <- 0L
  for (i in seq_len(n)) {
    k <- k + 1L
    val[k] <- v[i]; wt[k] <- w[i]; len[k] <- 1L
    ## Merge backwards while monotonicity is violated
    while (k > 1L && val[k - 1L] > val[k]) {
      tot         <- wt[k - 1L] + wt[k]
      val[k - 1L] <- (wt[k - 1L] * val[k - 1L] + wt[k] * val[k]) / tot
      wt[k - 1L]  <- tot
      len[k - 1L] <- len[k - 1L] + len[k]
      k <- k - 1L
    }
  }
  rep(val[seq_len(k)], len[seq_len(k)])
}

## CORP decomposition: score = miscalibration - discrimination + uncertainty
rare_corp_decomposition <- function(p, y, score = c("brier", "log")) {
  score <- match.arg(score)
  S <- switch(score
  , brier = function(q) mean((q - y)^2)
  , log   = function(q) mean(rare_log_score_vec(q, y)))
  pc    <- rare_pav_recalibrate(p, y)
  s     <- S(p); s_cal <- S(pc); s_ref <- S(rep(mean(y), length(y)))
  tibble::tibble(
    score = score, mean_score = s
  , MCB_miscalibration = s - s_cal
  , DSC_discrimination = s_ref - s_cal
  , UNC_uncertainty    = s_ref)
}

## CORP reliability curve and its consistency band under calibration
rare_corp_curve <- function(p, y, n_boot = 50, n_grid = 80, level = 0.90) {
  pc     <- rare_pav_recalibrate(p, y)
  o      <- order(p); xs <- p[o]
  keep   <- !duplicated(xs)
  grid_p <- sort(unique(stats::quantile(p, 1 - 10^seq(-4, 0, length.out = n_grid), names = FALSE)))
  band   <- if (n_boot > 0) {
    boot <- vapply(seq_len(n_boot), function(b) {
      yb  <- stats::rbinom(length(p), 1, p)
      pcb <- rare_pav_recalibrate(p, yb)[o]
      stats::approx(xs[keep], pcb[keep], xout = grid_p, rule = 2)$y
    }, numeric(length(grid_p)))
    a <- (1 - level) / 2
    tibble::tibble(p = grid_p, lo = apply(boot, 1, stats::quantile, a), hi = apply(boot, 1, stats::quantile, 1 - a))
  } else {
    tibble::tibble(p = numeric(0), lo = numeric(0), hi = numeric(0))
  }
  
  ## Thin the step curve to its change points so saved curves stay small
  curve <- tibble::tibble(p = xs[keep], p_recal = pc[o][keep])
  curve <- curve[c(TRUE, diff(curve$p_recal) != 0) | c(diff(curve$p_recal) != 0, TRUE), ]
  list(curve = curve, band = band)
}

## CSEP-style N-test: is the total expected count consistent with the observed count?
rare_n_test <- function(p, y) {
  n_obs  <- sum(y)
  lambda <- sum(p)
  sd_pb  <- sqrt(sum(p * (1 - p)))
  tibble::tibble(
    observed_events    = n_obs
  , expected_events    = lambda
  , ratio_obs_exp      = n_obs / lambda
  , delta1_P_N_ge_obs  = 1 - stats::ppois(n_obs - 1, lambda)
  , delta2_P_N_le_obs  = stats::ppois(n_obs, lambda)
  , z_poisson_binomial = (n_obs - lambda) / sd_pb
  )
}

## Information gain of model A over model B, with Rhoades et al. (2011) T- and W-tests
rare_info_gain_test <- function(pA, pB, y) {
  pA    <- rare_clip(pA); pB <- rare_clip(pB)
  n_ev  <- sum(y)
  llA   <- sum(y * log(pA) + (1 - y) * log1p(-pA))
  llB   <- sum(y * log(pB) + (1 - y) * log1p(-pB))
  ig    <- (llA - llB) / n_ev
  X     <- log(pA[y == 1]) - log(pB[y == 1])
  shift <- (sum(pA) - sum(pB)) / n_ev
  I_rh  <- mean(X) - shift
  t_st  <- I_rh / (stats::sd(X) / sqrt(n_ev))
  w_p   <- tryCatch(suppressWarnings(stats::wilcox.test(X - shift, mu = 0)$p.value)
                    , error = function(e) NA_real_)
  tibble::tibble(
    IG_nats_per_event = ig, prob_gain_per_event = exp(ig)
  , IG_rhoades = I_rh, T_stat = t_st
  , p_value_T = 2 * stats::pt(-abs(t_st), df = n_ev - 1), p_value_W = w_p)
}

## Hit rate and false-alarm rate at every threshold (alarm when p >= threshold)
rare_hf_table <- function(p, y) {
  o    <- order(p, decreasing = TRUE); p <- p[o]; y <- y[o]
  last <- c(p[-1] != p[-length(p)], TRUE)
  n1   <- sum(y); n0 <- length(y) - n1
  tibble::tibble(
    threshold  = c(Inf, p[last])
  , alarm_frac = c(0, which(last)) / length(y)
  , H          = c(0, cumsum(y)[last]) / n1
  , F          = c(0, cumsum(1 - y)[last]) / n0)
}

## Molchan trajectory: (weighted) fraction alarmed tau against miss rate nu
rare_molchan_curve <- function(p, y, weight = NULL) {
  if (is.null(weight)) weight <- rep(1, length(p))
  o    <- order(p, decreasing = TRUE)
  p    <- p[o]; y <- y[o]; w <- weight[o] / sum(weight)
  last <- c(p[-1] != p[-length(p)], TRUE)
  tibble::tibble(
    threshold = c(Inf, p[last])
  , tau       = c(0, cumsum(w)[last])
  , nu        = 1 - c(0, cumsum(y)[last]) / sum(y))
}

## Area skill score up to tau_max (random expectation tau_max / 2)
rare_area_skill_score <- function(curve, tau_max = 1) {
  nu_at <- stats::approx(curve$tau, curve$nu, xout = tau_max, ties = "ordered", rule = 2)$y
  keep  <- curve$tau < tau_max
  tt    <- c(curve$tau[keep], tau_max)
  hr    <- 1 - c(curve$nu[keep], nu_at)
  sum(diff(tt) * (utils::head(hr, -1) + utils::tail(hr, -1)) / 2) / tau_max
}

## Corner points of a Molchan trajectory (for plotting)
rare_thin_molchan <- function(curve) {
  ch <- which(diff(curve$nu) != 0)
  curve[sort(unique(c(1, ch, ch + 1, nrow(curve)))), ]
}

## Extremal dependence indices (Ferro & Stephenson 2011)
rare_edi  <- function(H, F) (log(F) - log(H)) / (log(F) + log(H))
rare_sedi <- function(H, F) (log(F) - log(H) - log(1 - F) + log(1 - H)) /
  (log(F) + log(H) + log(1 - F) + log(1 - H))

## EDI / SEDI at the first threshold alarming at least `frac` of space-time
rare_sedi_at_alarm_fraction <- function(p, y, frac = 0.01) {
  sc <- rare_hf_table(p, y) |>
    dplyr::filter(H > 0, H < 1, F > 0, F < 1) |>
    dplyr::mutate(EDI = rare_edi(H, F), SEDI = rare_sedi(H, F))
  if (!nrow(sc)) return(tibble::tibble(alarm_frac = NA_real_, H = NA_real_, F = NA_real_, EDI = NA_real_, SEDI = NA_real_))
  sc |> dplyr::filter(alarm_frac >= frac) |> dplyr::slice(1) |> dplyr::select(alarm_frac, H, F, EDI, SEDI)
}
