#' Series of plotting functions for output of examine_fit
#'
#'
#' @title plot.X

#' @param dat saved path to RDS of piece of examine_fit
#' @return tibble of figures for report
#' @author Morgan Kain
#' @export


plot.summary_probs <- function(dat) {
  td <- readRDS(dat)
  td %>% 
    mutate(
      forecast_interval = ifelse(is.na(forecast_interval), "Collapsed", forecast_interval)
    , forecast_interval = as.factor(forecast_interval)
    , aggregation = factor(
       aggregation, levels = c(
        "No aggregation", "Spatial aggregation"
      , "Temporal aggregation", "Double aggregation"
        )
       )
      ) %>% {
    ggplot(., aes(date, mid)) + 
      geom_errorbar(aes(date, ymin = lwr_n, ymax = ur_n, colour = forecast_interval)
                    , linewidth = 0.7) +
      geom_errorbar(aes(date, ymin = lwr, ymax = upr, colour = forecast_interval)
                    , linewidth = 0.2) +
      geom_point(aes(colour = forecast_interval)) +
      scale_colour_brewer(
        palette = "Dark2", name = "Forecast
Interval"
      ) +
      scale_y_log10() +
      facet_grid(aggregation~true_out) +
      geom_hline(yintercept = 0.01, linetype = "dashed", alpha = 0.6) +
      geom_hline(yintercept = 0.1, linetype = "dotted", alpha = 0.6) +
      geom_hline(yintercept = 0.5, linetype = "longdash", alpha = 0.6) +
      
      geom_vline(xintercept = as.Date(c(
        "2020-12-20", "2021-08-17", "2022-04-14", "2022-12-10", "2023-08-07", "2024-04-03"
      )), linetype = "dotted", alpha = 0.4) +
      
      xlab("Date") + ylab("Probability of Outbreak") +
      theme(
        axis.text.x = element_text(size = 12)
      , axis.text.y = element_text(size = 12)
      )
  }
  
}
## Incomplete functions that probably are not needed since the previous function exports a list of plots alread
plot.plotted_calibration <- function(dat) {
  td <- readRDS(dat)
}
plot.prob_dens_plot <- function(dat) {
  td <- readRDS(dat)
}
plot.map_split <- function(dat) {
  td <- readRDS(dat)
}

