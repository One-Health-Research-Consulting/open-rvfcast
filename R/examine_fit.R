#' Examine the quality of predictions on test data from model fit
#'
#'
#' @title examine_fit

#' @param model_out target model_out_for_eval
#' @param test_data splitted_data$test_data
#' @param train_data splitted_data$train_data
#' @param region_districts region_districts
#' @param africa_sf path to saved previously cleaned combined sf of all sub-regions (created with combine_africa_sf)
#' @return Tibble of summary metrics of model fit
#' @author Morgan Kain
#' @export

examine_fit <- function(model_out, test_data, train_data, region_districts, africa_sf) {

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
    conf_mat.t <- matrix(
      data = rep(0, 4)
    , ncol = 2, nrow = 2
    , dimnames = list(Prediction = c("0", "1"), Truth = c("0", "1"))
    )
    
    conf_mat.d <- dat_with_pred %>% 
      group_by(outbreak, outbreak_pred) %>%
      summarize(nout = n()) 
    
    for (q in 1:nrow(conf_mat.d)) {
      conf_mat.t[
        as.character(conf_mat.d$outbreak_pred[q])
      , as.character(conf_mat.d$outbreak[q])
      ] <- conf_mat.d$nout[q]
    }
    
    conf_mat <- conf_mat.t
    
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
          , aes(fill = prob_pred), colour = "white", linewidth = 0.01
          , alpha = 1
        ) +
        scale_fill_viridis_c(name = "Pr(outbreak)", limits = c(0, 1), oob = scales::squish) +
        coord_sf() +
        theme_void() +
        labs(title = "Predicted Outbreak Probability by Region")
    }
  
  return(
    tibble(
        outer_fold_id = model_out$outer_fold_id
      , index         = model_out$assess_data
      , metrics       = model_out$metrics
      , conf_mat      = conf_mat %>% list()
      , predictions   = preds_summarized %>% list()
      , map           = pred_map %>% list()
    )
  )
    
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

