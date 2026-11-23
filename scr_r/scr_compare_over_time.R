####
## Script to compare model performance across different fits over time
####

## All raw probabilities saved to
 ## outputs/fit_evaluation/ex_fits.all_probs_raw_Sys.Date().qs

## Each timestamped file is a snapshot from one run of the pipeline (see
 ## ex_fits.all_probs_raw in model_framework_targets.R)

## comparing across files gives a history/paper trail of how performance has 
 ## changed as the model itself has changed. Run this by hand whenever a 
 ## check-in is wanted; it is not part of the targets pipeline.

## PLACEHOLDER FOR NOW UNTIL WE HAVE MULTIPLE FITS -- with only one dated file
 ## saved so far, this script runs but the comparison is trivial (one point
 ## per line). It has been left runnable end-to-end so nothing further needs
 ## to be written once more dated fits exist.
  ## NOTE: Just a few options currently here, will want to add some other
  ## performance metrics when we have some more fits to compare 

library(tidyverse)
library(qs)
library(yardstick)

eval_path <- "outputs/fit_evaluation/"

#### Load and score every saved raw-probability file -----------------------------

## One row per run date x aggregation type x forecast interval, pooled across
 ## outer folds (folds are already pooled together at this stage for the
 ## other fit_evaluation exports, e.g. save_fig_pieces, so keep that convention)
compare_fits_over_time <- function(eval_path) {

  raw_files <- list.files(eval_path, pattern = "^ex_fits\\.all_probs_raw_.*\\.qs$", full.names = TRUE)

  if (length(raw_files) == 0) {
    stop("No ex_fits.all_probs_raw_*.qs files found in ", eval_path)
  }

  ## The run date is the date the pipeline was executed (baked into the
   ## filename), not a date being predicted
  run_dates <- raw_files |> basename() |> str_extract("\\d{4}-\\d{2}-\\d{2}") |> as.Date()

  if (length(raw_files) < 2) {
    message("Only ", length(raw_files), " dated fit file(s) found in ", eval_path
          , "; comparison will be trivial until more fits accumulate.")
  }

  purrr::map2(raw_files, run_dates, score_one_run) |> bind_rows()

}

#### Comparison plot over time ------------------------------------------------------

## Line plot of each metric over run date, faceted by scoring metric and
 ## aggregation type, colored by forecast interval (NA interval = the
 ## temporally-aggregated views, which collapse across forecast windows)
plot_fits_over_time <- function(all_metrics) {

  plot_dat <- all_metrics |>
    dplyr::select(run_date, aggregation, forecast_interval, roc_auc, pr_auc, logloss, brier, brier_skill, reliability, resolution) |>
    pivot_longer(cols = c(roc_auc, pr_auc, logloss, brier, brier_skill, reliability, resolution), names_to = "metric", values_to = "value")

  ggplot(plot_dat, aes(x = run_date, y = value, colour = factor(forecast_interval))) +
    geom_line() +
    geom_point() +
    facet_grid(metric ~ aggregation, scales = "free_y") +
    scale_x_date(date_labels = "%Y-%m-%d") +
    labs(
        x      = "Model run date"
      , y      = NULL
      , colour = "Forecast\ninterval (days)"
      , title  = "openRVFcast performance over time"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}

#### Helper: score a single dated fit file -----------------------------------------

## Computes ranking (ROC-AUC, PR-AUC), probabilistic (log loss, Brier score,
 ## Brier Skill Score relative to base-rate climatology), and its Murphy (1973)
 ## reliability/resolution decomposition, per aggregation type / forecast interval,
 ## matching the metric choices used elsewhere in the pipeline (see
 ## R/make_recipe.R, event_level = "first" for the positive class).
 ## brier_skill alone can look damning under extreme rarity even when the model
 ## has real discriminative value -- reliability (miscalibration, want LOW) and
 ## resolution (discrimination, want HIGH) pull those two failure modes apart;
 ## see reliability_decomposition() below.
score_one_run <- function(path, run_date) {

  dat <- qread(path) |> mutate(true_out_fct = factor(true_out, levels = c("1", "0")))

  dat |>
    group_by(aggregation, forecast_interval) |>
    group_modify(~ {
      clim_p   <- mean(.x$true_out)
      bs_model <- mean((.x$prob_pred - .x$true_out)^2)
      bs_clim  <- mean((clim_p - .x$true_out)^2)
      decomp   <- reliability_decomposition(.x$prob_pred, .x$true_out)
      tibble(
          n            = nrow(.x)
        , n_pos        = sum(.x$true_out == 1)
        , prevalence   = clim_p
        , roc_auc      = tryCatch(
            yardstick::roc_auc_vec(factor(.x$true_out, levels = c("1", "0")), .x$prob_pred, event_level = "first")
          , error = function(e) NA_real_)
        , pr_auc       = tryCatch(
            yardstick::pr_auc_vec(factor(.x$true_out, levels = c("1", "0")), .x$prob_pred, event_level = "first")
          , error = function(e) NA_real_)
        , logloss      = tryCatch(
            yardstick::mn_log_loss_vec(factor(.x$true_out, levels = c("1", "0")), .x$prob_pred, event_level = "first")
          , error = function(e) NA_real_)
        , brier        = bs_model
        , brier_skill  = if (bs_clim > 0) 1 - (bs_model / bs_clim) else NA_real_
        , reliability  = decomp$reliability
        , resolution   = decomp$resolution
      )
    }) |>
    ungroup() |>
    mutate(run_date = run_date, .before = 1)

}

## Murphy (1973) three-term Brier decomposition (brier = reliability - resolution +
 ## uncertainty) using fixed, hand-chosen probability bins sized for this problem's
 ## extreme rarity (finer near 0, where almost all the mass sits, coarser above 0.1).
 ## Bins this coarse necessarily lose some resolution/reliability signal relative to
 ## an unbinned estimator, but exact per-value binning is not identifiable from a
 ## finite sample anyway -- this is the standard practical tradeoff for this
 ## decomposition, not a shortcut specific to this script
reliability_decomposition <- function(prob_pred, true_out, breaks = c(0, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 1)) {
  clim_p <- mean(true_out)
  bin    <- cut(prob_pred, breaks, include.lowest = TRUE)
  tab    <- tibble(prob_pred, true_out, bin) |>
    group_by(bin) |>
    summarize(n = n(), pbar = mean(prob_pred), ybar = mean(true_out), .groups = "drop")
  list(
    reliability = sum(tab$n * (tab$pbar - tab$ybar)^2) / length(true_out)
  , resolution  = sum(tab$n * (tab$ybar - clim_p)^2) / length(true_out)
  )
}

#### Run ---------------------------------------------------------------------------

all_metrics  <- compare_fits_over_time(eval_path)
gg_over_time <- plot_fits_over_time(all_metrics)

print(gg_over_time)

## Save both the combined metric table and the comparison figure, timestamped
 ## like the rest of fit_evaluation so this comparison itself becomes part of
 ## the paper trail
qsave(all_metrics, paste0(eval_path, "ex_fits.performance_over_time_", Sys.Date(), ".qs"))
ggsave(paste0(eval_path, "gg_performance_over_time_", Sys.Date(), ".svg"), gg_over_time, width = 10, height = 8)
