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
  
  all_dat_preds <- preds %>% group_by(outer_fold_id) %>% group_split() %>% purrr::map_dfr(function(preddat) {
    
    preddat$preds[[1]] %>% 
      mutate(
        index    = preddat$assess_data[[1]]
        , outbreak = as.numeric(as.character(.pred_class))
        , .before  = 1
      ) %>% 
      dplyr::select(-.pred_class) %>%
      rename(outbreak_pred = outbreak) %>%
      left_join(
        .
        , test_data %>% filter(index %in% preddat$assess_data[[1]]) 
      ) %>%
      dplyr::select(!!predname, !!truename, any_of(splitgrp)) %>%
      mutate(outer_fold_id = preddat$outer_fold_id)
    
  }) 
  
  binoms.all <- all_dat_preds %>% dplyr::group_by_at(splitgrp) %>% 
    dplyr::group_split() %>% purrr::map_dfr(function(dat) {
      
      grp_vals <- dat %>% dplyr::select(all_of(splitgrp)) 
      grp_vals <- apply(grp_vals %>% as.matrix(), 2, unique) 
      
      opt_breaks <- optimise_bins(dat, predname, truename) %>%
        get_optimised_bin_values() %>%
        (\(x) x$breaks)()
      
      expr <- paste(
        "cut("
        , predname
        , ", breaks = opt_breaks, include.lowest = TRUE, right = FALSE)"
        , sep = ""
      )
      
      grps <- dat %>%
        dplyr::select(all_of(splitgrp), !!predname, !!truename) %>%
        dplyr::mutate(
          predicted_grp = eval(parse(text = expr))
          , predicted_grp_median = median(get(predname), na.rm = TRUE)
          , predicted_grp_mean = stringr::str_remove_all(predicted_grp, "\\[|\\)|\\]") %>%
            stringr::str_split(pattern = ",") %>%
            lapply(function(x) as.numeric(x) %>% median()) %>% unlist()
          , predicted_grp_min = min(get(predname), na.rm = TRUE)
          , predicted_grp_max = max(get(predname), na.rm = TRUE)
        ) %>% ungroup()
      
      grp_sizes <- grps %>%
        dplyr::group_by(predicted_grp) %>% 
        summarize(n = n(), truth = sum(get(truename))) %>%
        dplyr::ungroup() %>% 
        dplyr::arrange(predicted_grp) 
      
      binoms <- grps %>% 
        dplyr::group_by(predicted_grp) %>% 
        dplyr::group_split() %>% 
        purrr::map_dfr(function(tw) {
          binom <- binom::binom.confint(x = sum(tw %>% pull(get(truename))), n = nrow(tw), methods = "wilson")
          tw.t <- tw %>%
            dplyr::distinct(predicted_grp, predicted_grp_median, predicted_grp_mean, predicted_grp_max, predicted_grp_min) %>%
            dplyr::mutate(grp_mean = binom$mean, grp_lwr = binom$lower, grp_upr = binom$upper)
          tw.t 
        })
      
      for (i in seq_along(grp_vals)) {
        binoms <- binoms %>% mutate(!!names(grp_vals)[i] := grp_vals[i], .before = 1)
      }
      
      binoms %>% dplyr::select(all_of(splitgrp)) %>% distinct() %>% 
        mutate(calibration_curves = list(binoms))
      
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
  caltib <- do.call("rbind", caltib$calibration_curves)
  
  max_val <- round(max(caltib$grp_upr) + 0.005, digits = 3)
  
  gg1 <- caltib %>% filter(forecast_interval %in% forcastvals) %>% {
    ggplot(., aes(grp_mean, predicted_grp_mean)) +
      geom_abline(color = "gray50") +
      geom_errorbar(aes(xmin = grp_lwr, xmax = grp_upr)) +
      geom_point(pch = 21,fill = "white") +
      scale_x_sqrt(limits = c(0, max_val)) +
      scale_y_sqrt(limits = c(0, max_val)) +
      labs(y = "Forecasted outbreak probability", x = "Observed outbreak rate", color = "") +
      theme_minimal() +
      theme(
        text = element_text(size = 16)
      , plot.title.position = "plot"
      , plot.caption = element_text(hjust = 0)
      ) 
    
  }
  
  if (!is.null(yg) & !is.null(xg)) {
    return(gg1 + facet_grid(get(yg) ~ get(xg)))
  } else if (!is.null(yg) & is.null(xg)) {
    gg1 + facet_wrap(~get(yg), ncol = 1)
  } else if (is.null(yg) & !is.null(xg)) {
    gg1 + facet_wrap(~get(xg), ncol = 1)
  } else {
    gg1
  }

}
