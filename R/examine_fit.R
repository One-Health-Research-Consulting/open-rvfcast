#' Examine the quality of predictions on test data from model fit
#'
#'
#' @title examine_fit

#' @param model_out target model_out_for_eval
#' @param test_data splitted_data$test_data
#' @param regions broken up area map
#' @param larger_districts performance_hexes or just set to NULL if not using hexes or if you dont want to evaluate at a larger scale
#' @param africa_sf path to saved previously cleaned combined sf of all sub-regions (created with combine_africa_sf)
#' @param region_to_sum country or other region type into which to summarize predictions
#' @param p_thresh probability over which to assign a one
#' @return Tibble of summary metrics of model fit
#' @author Morgan Kain
#' @export

examine_fits_within <- function(model_out, test_data, regions, larger_districts
                                , africa_sf, region_to_sum, p_thresh, using_hexes
                                ) {

  ## Load the previously created / saved Africa map of sub-regions per Country
  if (!using_hexes) {
    africa_sf <- readRDS(africa_sf) %>%
      mutate(country_norm = norm_key(country),
             region_norm  = norm_key(region))
  } else {
    africa_sf <- regions[[1]] %>% 
      rename(region_norm = shapeName) %>% mutate(country_norm = "null")
    if (!is.null(larger_districts)) {
      eval_regions <- larger_districts[[1]] 
      africa_sf    <- africa_sf %>% mutate(shapeName = h3jsr::get_parent(region_norm, 2)) 
    }
  }
  
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
  
  ## Add in the larger H3 hexes
  if (!is.null(larger_districts)) {
  dat_with_pred <- dat_with_pred %>%
    rename(region_norm = shapeName) %>% 
    left_join(africa_sf %>% as.data.frame() %>% dplyr::select(region_norm, shapeName)) %>%
    relocate(shapeName, .after = "region_norm")
  }
  
  ## And finally other country-level or regions if desired
  if (!is.null(region_to_sum)) {
    
    intersecting_hexes <- st_intersects(region_to_sum[[1]], africa_sf) %>% as.data.frame() %>%
      rename(area_index = row.id, index = col.id)
    
    dat_with_pred <- dat_with_pred %>% 
      rename(region_norm = shapeName) %>% 
      left_join(
        .
      , africa_sf %>% as.data.frame() %>% dplyr::select(region_norm) %>%
         mutate(index = seq(n())) %>% left_join(intersecting_hexes) %>% 
         filter(!is.na(area_index)) %>% dplyr::select(-index)
      ) %>% filter(!is.na(area_index)) %>%
      left_join(
        .
      , region_to_sum[[1]] %>% as.data.frame() %>% dplyr::select(shapeName) %>% mutate(area_index = seq(n()))
      ) 
  }
  
  ## Do all of the performance summaries for each date, for:
   ## A) No aggregation: each small hex for each forecast window
   ## B) Spatial aggregation: larger hex for each forecast window
   ## C) Temporal aggregation: small hexes across forecast windows
   ## D) Double aggregation: larger hexes across forecast windows
  
  if (!is.null(larger_districts) & !is.null(region_to_sum)) {
    stop("Exactly one of larger_districts or region_to_sum should be NULL")
  } else if (!is.null(larger_districts)) {
    aggregation_list <- list(
      "No aggregation"       = c("date", "region_norm", "forecast_interval")
    , "Spatial aggregation"  = c("date", "shapeName", "forecast_interval")
    , "Temporal aggregation" = c("date", "region_norm")
    , "Double aggregation"   = c("date", "shapeName")
    )
  } else {
    aggregation_list <- list(
      "Spatial aggregation"  = c("date", "shapeName", "forecast_interval")
    , "Double aggregation"   = c("date", "shapeName")
    )
  }
  
 out_list <- purrr::pmap(list(aggregation_list, names(aggregation_list)), .f = function(x, z) {
    
    ## Do the appropriate level of summarization
    dat.s <- dat_with_pred %>% 
      dplyr::group_by(across(all_of(x))) %>%
      summarize(
        prob_pred = 1 - prod(1 - .pred_1)
      , true_out  = max(outbreak)
      ) %>% ungroup()
    
    ## Build calibration curves
    calcurves <- generate_calibration_curve(
      preds      = dat.s
    , test_data  = splitted_data$test_data[[1]]
    , predname   = "prob_pred"
    , truename   = "true_out"
    , splitgrp   = ifelse("forecast_interval" %in% x, "forecast_interval", NA)
    )
    
    plotted_calibration <- plot_calibration(
      caltib      = calcurves
    , xg          = NULL 
    , yg          = ifelse("forecast_interval" %in% x, "forecast_interval", NA)
    , forcastvals = if("forecast_interval" %in% x){c(30, 90, 150)}else{NA}
    )
    
    ## Summary table
    summary_probs <- dat.s %>% dplyr::group_by(across(all_of(
      c(x, "true_out")[c(x, "true_out") %notin% c("region_norm", "shapeName")]
    ))) %>%
      summarize(
        lwr   = quantile(prob_pred, 0.025)
      , lwr_n = quantile(prob_pred, 0.200)
      , mid   = quantile(prob_pred, 0.500)
      , ur_n  = quantile(prob_pred, 0.800)
      , upr   = quantile(prob_pred, 0.975)
        )
    
    ## Confusion matrix -- just doing it by hand to avoid issues
    conf_mat <- purrr::map(seq_along(p_thresh), .f = function(i) {
      
      if ("forecast_interval" %in% x) {
        conf_mat.d <- dat.s %>% 
          mutate(outbreak_assigned = ifelse(prob_pred > p_thresh[i], 1, 0)) %>%
          group_by(forecast_interval, true_out, outbreak_assigned) %>%
          summarize(nout = n()) %>% 
          mutate(positive_threshold = p_thresh[i], .before = 1)
      } else {
        conf_mat.d <- dat.s %>% 
          mutate(outbreak_assigned = ifelse(prob_pred > p_thresh[i], 1, 0)) %>%
          group_by(true_out, outbreak_assigned) %>%
          summarize(nout = n()) %>% 
          mutate(positive_threshold = p_thresh[i], .before = 1)
      }
      
    }) %>% do.call("rbind", .)
    
    if ("forecast_interval" %in% x) {
    conf_mat.t <- conf_mat %>% 
      filter(positive_threshold %in% c(0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
      group_by(forecast_interval, positive_threshold) %>%
      mutate(prop_t = log(nout / sum(nout)))
    } else {
      conf_mat.t <- conf_mat %>% 
        filter(positive_threshold %in% c(0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
        group_by(positive_threshold) %>%
        mutate(prop_t = log(nout / sum(nout)))
    } 
    
    conf_mat_plot <- conf_mat.t %>% {
        ggplot(., aes(true_out, outbreak_assigned)) + 
          geom_tile(aes(fill = prop_t)) +
          scale_fill_continuous() +
          facet_grid(positive_threshold ~ forecast_interval) +
          geom_text(aes(label = nout), color = "white", size = 3) +
          theme(
            axis.text.x  = element_text(size = 10)
            , axis.text.y  = element_text(size = 10)
            , axis.title.x = element_text(size = 12)
            , axis.title.y = element_text(size = 12)
          ) +
          scale_x_continuous(breaks = c(0, 1)) +
          scale_y_continuous(breaks = c(0, 1)) +
          xlab("True Outbreaks") + ylab("Predicted Outbreaks") +
          ggtitle(paste(
            "Prediction Period = "
            , strsplit(model_out$assess_range, " ")[[1]] %>% paste(., collapse = " - ")
          ))
      }
    
    ## Distribution of probabilities for true 1s and true 0s
    prob_dens_plot <- dat.s %>% 
      mutate(
        true_out  = as.factor(true_out)
      , prob_pred = qlogis(prob_pred)
      ) %>% {
        ggplot(., aes(x = prob_pred)) + 
          geom_density(aes(fill = true_out), alpha = 0.5, colour = NA) +
          {
            if("forecast_interval" %in% x) {
              facet_wrap(~forecast_interval, scales = "free")
            }
          } +
          scale_fill_brewer(palette = "Dark2") +
          theme(
            axis.text.x  = element_text(size = 10)
            , axis.text.y  = element_text(size = 10)
            , axis.title.x = element_text(size = 12)
            , axis.title.y = element_text(size = 12)
          ) +
          xlab("Predicted Probability of Outbreak (logit scale)") +
          ylab("Density") +
          ggtitle(paste(
            "Prediction Period = "
          , strsplit(model_out$assess_range, " ")[[1]] %>% paste(., collapse = " - ")
          ))
      }
    
    map_list <- lapply(unique(dat.s$date) %>% as.list(), FUN = function(d) {
      
      if ("shapeName" %in% x) {
        if (!is.null(larger_districts) & !is.null(region_to_sum)) {
          stop("Exactly one of larger_districts or region_to_sum should be NULL")
        } else if (!is.null(larger_districts)) {
          map.dat.s <- eval_regions %>% left_join(., dat.s)
        } else {
          map.dat.s <- region_to_sum[[1]] %>% left_join(., dat.s)
        }
      } else {
        map.dat.s <- africa_sf %>% left_join(., dat.s)
      }
      
      map.dat.s %>% 
        mutate(true_out = as.factor(true_out)) %>% 
        filter(!is.na(prob_pred)) %>%
        filter(date == d) %>% {
          ggplot(.) +
            geom_sf(aes(fill = prob_pred), colour = NA, linewidth = 0, alpha = 0.2) +
            geom_sf(
              data = map.dat.s %>% 
                mutate(true_out = as.factor(true_out)) %>% 
                filter(!is.na(prob_pred)) %>%
                filter(date == d) %>%
                filter(true_out == 1)
              , aes(fill = prob_pred), colour = "black", linewidth = 0.4
              , alpha = 1
            ) +
            scale_fill_viridis_c(name = "Pr(outbreak)", limits = c(0, 1), oob = scales::squish) +
            coord_sf() +
            theme_void() +
            labs(title = "Predicted Outbreak Probability by Region") +
            {
              if("forecast_interval" %in% x) {
                facet_wrap(~forecast_interval)
              }
            } +
            ggtitle(paste("Prediction Date = ", d))
        }
      
    })
    
    return(
      tibble(
        aggregation    = z
      , date_range     = unique(dat.s$date) %>% list()
      , summary_probs  = summary_probs %>% list()
      , conf_mat       = conf_mat %>% list()
      , conf_mat_plot  = conf_mat_plot %>% list()
      , calcurves      = calcurves %>% list()
      , plotted_calibration = plotted_calibration %>% list()
      , prob_dens_plot = prob_dens_plot %>% list()
      , map_split      = map_list %>% list()
      )
    ) 
    
  })
 
 out_list <- out_list %>% 
   bind_rows() %>% 
   mutate(
      outer_fold_id  = model_out$outer_fold_id
    , index          = model_out$assess_data
    , metrics        = model_out$metrics
   )
 
 return(out_list)
  
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
                             , grouping_vals
                             ) {
  preds_all %>%
    mutate(
      country_norm = norm_key(.data[[country_col]])
    , region_norm  = norm_key(.data[[region_col]])
    ) %>%
    group_by_at(all_of(c("country_norm", "region_norm", grouping_vals))) %>%
    summarize(
      prob_pred = 1 - prod(1 - get(prob_col))
    , true_out  = max(outbreak)
    ) %>%
    ungroup() 
}

