#' Examine the quality of predictions on test data from model fit
#'
#'
#' @title examine_fit

#' @param model_out target model_out_for_eval
#' @param test_data splitted_data$test_data
#' @param region_districts region_districts
#' @param africa_sf path to saved previously cleaned combined sf of all sub-regions (created with combine_africa_sf)
#' @param p_thresh probability over which to assign a one
#' @return Tibble of summary metrics of model fit
#' @author Morgan Kain
#' @export

examine_fits_within <- function(model_out, test_data, region_districts, africa_sf, p_thresh) {

  ## Load the previously created / saved Africa map of sub-regions per Country
  afmap <- readRDS(africa_sf)
  
  ## Combine predictions with the data
  dat_with_pred <- model_out$preds[[1]] %>% 
    mutate(
      index    = model_out$assess_data[[1]]
    , outbreak = as.numeric(as.character(.pred_class))
    , .before  = 1
    ) %>% 
    dplyr::select(-.pred_class) %>%
    rename(outbreak_pred = outbreak) %>%
    left_join(
      .
    , test_data %>% filter(index %in% model_out$assess_data[[1]]) 
    )
  
  ## Confusion matrix -- just doing it by hand to avoid issues
  conf_mat <- purrr::map(seq_along(p_thresh), .f = function(i) {
     
     conf_mat.d <- dat_with_pred %>% 
       mutate(outbreak_assigned = ifelse(.pred_1 > p_thresh[i], 1, 0), .after = outbreak_pred) %>%
       group_by(forecast_interval) %>%
       group_by(forecast_interval, outbreak, outbreak_assigned) %>%
       summarize(nout = n()) %>% 
       mutate(positive_threshold = p_thresh[i], .before = 1)
     
   }) %>% do.call("rbind", .)
  
conf_mat_plot <- conf_mat %>% 
    filter(positive_threshold %in% c(0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    group_by(forecast_interval, positive_threshold) %>%
    mutate(prop_t = log(nout / sum(nout))) %>% {
    ggplot(., aes(outbreak, outbreak_assigned)) + 
      geom_tile(aes(fill = prop_t)) +
      scale_fill_continuous() +
      facet_grid(positive_threshold ~ forecast_interval) +
      geom_text(aes(label = nout), color = "white", size = 3) +
      theme(
        axis.text.x = element_text(size = 10)
      , axis.text.y = element_text(size = 10)
      ) +
        scale_x_continuous(breaks = c(0, 1)) +
        scale_y_continuous(breaks = c(0, 1)) +
        xlab("True Outbreaks") + ylab("Predicted Outbreaks")
  }
    
  ## Distribution of probabilities for true 1s and true 0s
prob_dens_plot <- dat_with_pred %>% 
    mutate(
      outbreak = as.factor(outbreak)
    , .pred_1  = qlogis(.pred_1)
    ) %>% {
      ggplot(., aes(x = .pred_1)) + 
      geom_density(aes(fill = outbreak), alpha = 0.5, colour = NA) +
      facet_wrap(~forecast_interval) +
      scale_fill_brewer(palette = "Dark2") +
      xlab("Predicted Probability of Outbreak (logit scale)") +
      ylab("Density")
    }
  
  ## Estimating spatial variability of divergence between truth and predictions
  africa_sf <- afmap %>%
    mutate(country_norm = norm_key(country),
           region_norm  = norm_key(region))
  
  preds_summarized <- prep_preds_for_map(dat_with_pred)
  
  map_sf <- africa_sf %>% 
    left_join(
      .
    , preds_summarized, by = c("country_norm", "region_norm")
    )
  
  pred_map <- map_sf %>% 
    mutate(true_out = as.factor(true_out)) %>% 
    filter(!is.na(prob_pred)) %>% {
      ggplot(.) +
        geom_sf(aes(fill = prob_pred), colour = "black", linewidth = 0.01, alpha = 0.2) +
        geom_sf(
          data = map_sf %>% 
            mutate(true_out = as.factor(true_out)) %>% 
            filter(!is.na(prob_pred)) %>%
            filter(true_out == 1)
          , aes(fill = prob_pred), colour = "black", linewidth = 0.4
          , alpha = 1
        ) +
        scale_fill_viridis_c(name = "Pr(outbreak)", limits = c(0, 1), oob = scales::squish) +
        coord_sf() +
        theme_void() +
        labs(title = "Predicted Outbreak Probability by Region")
    }
  
  return(
    tibble(
        outer_fold_id  = model_out$outer_fold_id
      , index          = model_out$assess_data
      , metrics        = model_out$metrics
      , conf_mat       = conf_mat %>% list()
      , conf_mat_plot  = conf_mat_plot %>% list()
      , prob_dens_plot = prob_dens_plot %>% list()
      , predictions    = preds_summarized %>% list()
      , map            = pred_map %>% list()
    )
  )
    
}


examine_fits_across <- function(ex_within, test_data) {
  
}


## Series of helper functions for debugging
combine_africa_sf  <- function(sf_list) {
  
  africa_sf <- do.call(rbind, sf_list)  
  
  africa_sf %<>%
    transmute(
      country = shapeGroup
    , region  = shapeName
    , geometry = geometry
    ) %>%
    sf::st_make_valid()
  
  orig_crs <- sf::st_crs(africa_sf)
  
  africa_sf %<>%
    vfix() %>%
    ## Web Mercator; fine for area ballpark
    sf::st_transform(3857) %>%
    sf::st_cast("POLYGON", warn = FALSE) %>%
    mutate(area_km2 = as.numeric(sf::st_area(geometry)) / 1e6) %>%
    filter(area_km2 >= 5) %>%                 
    group_by(country, region) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    vfix()

  rmapshaper::ms_simplify(
    africa_sf
  , keep = 0.05
  , keep_shapes = TRUE
  ) %>% sf::st_transform(orig_crs)
  
}
vfix               <- function(x) {
  x <- sf::st_make_valid(x)
  x <- sf::st_collection_extract(x, "POLYGON")
  x
}
norm_key           <- function(x) {
  x %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_squish()
}
prep_preds_for_map <- function(preds_all
                             , prob_col = ".pred_1"
                             , country_col = "Country"
                             , region_col = "shapeName"
                             ) {
  preds_all %>%
    mutate(
      country_norm = norm_key(.data[[country_col]])
      , region_norm  = norm_key(.data[[region_col]])
    ) %>%
    group_by(country_norm, region_norm) %>%
    summarize(
      prob_pred = 1 - prod(1 - get(prob_col))
    , true_out  = max(outbreak)
    ) %>%
    ungroup() 
}

