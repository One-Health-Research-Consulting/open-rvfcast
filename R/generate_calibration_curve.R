#' Function for generating the calibration tibble for model evaluation
#'
#'
#' @title generate_calibration_curve

#' @param preds tibble that contains the model predictions
#' @param test_data splitted_data$test_data
#' @param predname name of the predicted values
#' @param truename name of the true outcome
#' @param splitgrp group to split on 
#' @return Tibble of model fit output
#' @author Morgan Kain
#' @export

generate_calibration_curve <- function(preds, test_data, predname, truename, splitgrp) {
  
  if (is.na(splitgrp)) {splitgrp <- NULL}
  
  binoms.all <- preds %>% dplyr::group_by_at(splitgrp) %>% 
    dplyr::group_split() %>% purrr::map_dfr(function(dat) {
      
      ## Optimized breaks using reliabilitydiag
      opt_breaks <- optimise_bins(dat, predname, truename) %>%
        get_optimised_bin_values() %>%
        (\(x) x$breaks)()
      
      ## And even breaks to get the full range
      even_breaks <- seq(0.00, 1, by = 0.025)
      
      expr.opt  <- paste("cut(", predname, ", breaks = opt_breaks, include.lowest = TRUE, right = FALSE)", sep = "")
      expr.even <- paste("cut(", predname, ", breaks = even_breaks, include.lowest = TRUE, right = FALSE)", sep = "")
      
      grps.opt <- dat %>%
        dplyr::select(all_of(splitgrp), !!predname, !!truename) %>%
        dplyr::mutate(predicted_grp = eval(parse(text = expr.opt))) %>% 
        group_by(predicted_grp) %>% 
        mutate(
          predicted_grp_med    = quantile(get(predname), 0.500, na.rm = TRUE)
          , predicted_grp_upr    = quantile(get(predname), 0.975, na.rm = TRUE)
          , predicted_grp_uprn   = quantile(get(predname), 0.800, na.rm = TRUE)
          , predicted_grp_lwr    = quantile(get(predname), 0.025, na.rm = TRUE)
          , predicted_grp_lwrn   = quantile(get(predname), 0.200, na.rm = TRUE)
        ) %>% ungroup()
      
      grps.even <- dat %>%
        dplyr::select(all_of(splitgrp), !!predname, !!truename) %>%
        dplyr::mutate(predicted_grp = eval(parse(text = expr.even))) %>%
        group_by(predicted_grp) %>% 
        mutate(
          predicted_grp_med    = quantile(get(predname), 0.500, na.rm = TRUE)
          , predicted_grp_upr    = quantile(get(predname), 0.975, na.rm = TRUE)
          , predicted_grp_uprn   = quantile(get(predname), 0.800, na.rm = TRUE)
          , predicted_grp_lwr    = quantile(get(predname), 0.025, na.rm = TRUE)
          , predicted_grp_lwrn   = quantile(get(predname), 0.200, na.rm = TRUE)
        ) %>% ungroup()
      
      grp_sizes.opt <- grps.opt %>%
        dplyr::group_by(predicted_grp) %>% 
        summarize(n = n(), truth = sum(get(truename))) %>%
        dplyr::ungroup() %>% 
        dplyr::arrange(predicted_grp) 
      
      grp_sizes.even <- grps.even %>%
        dplyr::group_by(predicted_grp) %>% 
        summarize(n = n(), truth = sum(get(truename))) %>%
        dplyr::ungroup() %>% 
        dplyr::arrange(predicted_grp) 
      
      binoms.opt <- grps.opt %>% 
        dplyr::group_by(predicted_grp) %>% 
        dplyr::group_split() %>% 
        purrr::map_dfr(function(tw) {
          binom <- binom::binom.confint(x = sum(tw %>% pull(get(truename))), n = nrow(tw), methods = "wilson")
          tw.t <- tw %>%
            dplyr::distinct(
              predicted_grp, predicted_grp_med, predicted_grp_lwr, predicted_grp_lwrn
              , predicted_grp_upr, predicted_grp_uprn
            ) %>%
            dplyr::mutate(grp_mean = binom$mean, grp_lwr = binom$lower, grp_upr = binom$upper)
          tw.t 
        })
      
      binoms.even <- grps.even %>% 
        dplyr::group_by(predicted_grp) %>% 
        dplyr::group_split() %>% 
        purrr::map_dfr(function(tw) {
          binom <- binom::binom.confint(x = sum(tw %>% pull(get(truename))), n = nrow(tw), methods = "wilson")
          tw.t <- tw %>%
            dplyr::distinct(
              predicted_grp, predicted_grp_med, predicted_grp_lwr, predicted_grp_lwrn
              , predicted_grp_upr, predicted_grp_uprn
            ) %>%
            dplyr::mutate(grp_mean = binom$mean, grp_lwr = binom$lower, grp_upr = binom$upper)
          tw.t 
        })
      
      binoms.opt  <- binoms.opt %>% left_join(., grp_sizes.opt)
      binoms.even <- binoms.even %>% left_join(., grp_sizes.even)
      
      if (!is.null(splitgrp)) {
        binoms.opt  <- binoms.opt  %>% mutate(forecast_interval = dat$forecast_interval[1])
        binoms.even <- binoms.even %>% mutate(forecast_interval = dat$forecast_interval[1])
      }
      
      binoms.opt %>% 
        dplyr::select(all_of(splitgrp)) %>% distinct() %>% 
        mutate(
          calibration_curves.opt  = list(binoms.opt)
          , calibration_curves.even = list(binoms.even)
        )
      
    })
  
  binoms.all
  
}

#### Helpers ---------------------------------------------------------------------------
optimise_bins              <- function(dat, predname, truename) {
  rd <- reliabilitydiag::reliabilitydiag(
    x = dat %>% pull(get(predname)),
    y = as.integer(dat %>% pull(get(truename)))
  )
  
  ## Return optimised bins ----
  rd$x$bins
}
get_optimised_bin_values   <- function(opt_bins) {
  with(
    opt_bins,
    list(
      n = n,
      median = Map(median, x_min, x_max) |> unlist(),
      breaks = c(x_min, tail(x_max, 1))
    )
  )
}

#### And the plotting code -------------------------------------------------------------
plot_calibration <- function(caltib, xg, yg, forcastvals) {
  
  ## Unpack 
  caltib.opt  <- do.call("rbind", caltib$calibration_curves.opt)
  caltib.even <- do.call("rbind", caltib$calibration_curves.even)
  
  ## NOTE: Really hacky rough approach to get this working, needs cleanup for sure
  if (!is.na(yg)) {
    caltib.opt  <- caltib.opt %>% filter(forecast_interval %in% forcastvals) %>% filter(truth > 0)
    caltib.even <- caltib.even %>% filter(forecast_interval %in% forcastvals) %>% filter(truth > 0)
  } else {
    caltib.opt  <- caltib.opt %>% filter(truth > 0)
    caltib.even <- caltib.even %>% filter(truth > 0)
  }
  
  gg1.opt <- caltib.opt %>% {
    ggplot(., aes(grp_mean, predicted_grp_med)) +
      geom_abline(color = "gray50") +
      geom_errorbarh(aes(xmin = grp_lwr, xmax = grp_upr), linewidth = 0.5, width = 0) +
      geom_errorbar(aes(ymin = predicted_grp_lwr, ymax = predicted_grp_upr), linewidth = 0.5, width = 0) +
      geom_errorbar(aes(ymin = predicted_grp_lwrn, ymax = predicted_grp_uprn), linewidth = 1, width = 0) +
      geom_point(aes(size = log(n)), pch = 21, fill = "white") +
      scale_size_continuous(name = "Number
Predicted
(log)") +
      scale_x_log10() +
      scale_y_log10() +
      labs(y = "Forecasted outbreak probability", x = "Observed outbreak rate", color = "") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 14)
        , axis.title = element_text(size = 16)
        , plot.title.position = "plot"
        , plot.caption = element_text(hjust = 0)
      ) 
  }
  
  gg1.even <- caltib.even %>% {
    ggplot(., aes(grp_mean, predicted_grp_med)) +
      geom_abline(color = "gray50") +
      geom_errorbarh(aes(xmin = grp_lwr, xmax = grp_upr), linewidth = 0.5, width = 0) +
      geom_errorbar(aes(ymin = predicted_grp_lwr, ymax = predicted_grp_upr), linewidth = 0.5, width = 0) +
      geom_errorbar(aes(ymin = predicted_grp_lwrn, ymax = predicted_grp_uprn), linewidth = 1, width = 0) +
      geom_point(aes(size = log(n)), pch = 21, fill = "white") +
      scale_size_continuous(name = "Number
Predicted
(log)") +
      scale_x_sqrt() +
      scale_y_sqrt() +
      labs(y = "Forecasted outbreak probability", x = "Observed outbreak rate", color = "") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 14)
        , axis.title = element_text(size = 16)
        , plot.title.position = "plot"
        , plot.caption = element_text(hjust = 0)
      ) 
  }
  
  ## NOTE: Really hacky rough approach to get this working, needs cleanup for sure
  if (!is.na(yg)) {
    if (!is.null(yg) & !is.null(xg)) {
      gg1.opt  <- gg1.opt + facet_grid(get(yg) ~ get(xg))
      gg1.even <- gg1.even + facet_grid(get(yg) ~ get(xg))
    } else if (!is.null(yg) & is.null(xg)) {
      gg1.opt  <- gg1.opt + facet_wrap(~get(yg), ncol = 1)
      gg1.even <- gg1.even + facet_wrap(~get(yg), ncol = 1)
    } else if (is.null(yg) & !is.null(xg)) {
      gg1.opt  <- gg1.opt + facet_wrap(~get(xg), ncol = 1)
      gg1.even <- gg1.even + facet_wrap(~get(xg), ncol = 1)
    } else {
      gg1.opt  <- gg1.opt
      gg1.even <- gg1.even
    }
  }
  
  return(
    tibble(
      calplot.opt  = gg1.opt %>% list()
      , calplot.even = gg1.even %>% list()
    )
  )
  
}
