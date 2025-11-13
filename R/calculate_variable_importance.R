#' For each fitted model calculate variable importance 
#'
#'
#' @title calculate_variable_importance

#' @param model_dat path to saved model fit
#' @param fitdir path to saved extracted parsnip fit
#' @param recdir path to saved recipe
#' @param num_vars number of variables for partial dependence plots
#' @param
#' @param
#' @return Tibble of variable importance metrics
#' @author Morgan Kain
#' @export

calculate_variable_importance   <- function(model_dat, fitdir, recdir, num_vars) {
  
  ## generate path to the fit for this model
  fit_path    <- paste(fitdir, "/parsnip_fit_", model_dat$outer_fold_id, ".Rds", sep = "")
  rec_path    <- paste(fitdir, "/recipe_", model_dat$outer_fold_id, ".Rds", sep = "")
  
  ## Load fit from RDS
  parsnip_fit  <- readRDS(fit_path)
  saved_recipe <- readRDS(rec_path)
  
  ## Variable Importance
  VIP     <- vip::vip(parsnip_fit, num_features = 30)
  imp_tbl <- vip::vi(parsnip_fit) %>% dplyr::arrange(., desc(Importance))
  
  ## More detail
  xgb_booster <- parsnip_fit$fit
  VIP.xgb     <- xgboost::xgb.importance(model = xgb_booster, feature_names = xgb_booster$feature_names) 
  VIP.xgb.p   <- VIP.xgb %>% xgboost::xgb.plot.importance(top_n = 30)
  
  vartt <- imp_tbl[1:num_vars, 1]$Variable
  
for (i in seq_along(vartt)) {
  p1 <- pdp::partial(
    object          = parsnip::extract_fit_engine(parsnip_fit)
  , pred.var        = vartt[i]
  , pred.fun        = function(object, newdata) { predict(object, as.matrix(newdata)) }
  , grid.resolution = 30
  , ice             = FALSE
  , train           = recipes::bake(
      saved_recipe
    , new_data = model_dat$dat_for_pdp[[1]] %>% dplyr::select(-weights, -outbreak)
    )
  ## Class Probs
  , type            = "classification"
  ## Gor binary, can also specify the positive class name
  , which.class     = 1
  , parallel        = FALSE
) %>% 
    group_by_at(vartt[i]) %>%
    summarise(yhat = mean(yhat), .groups = "drop") %>%
    rename(value = 1) %>%
    mutate(variable = vartt[i], .before = 1)
  if (i == 1) {
    p1.f <- p1
  } else {
    p1.f <- rbind(p1.f, p1)
  }
  rm(p1); gc()
}
  
  return(
    tibble(
      top_variables_vip = imp_tbl   %>% mutate(outer_fold_id = model_dat$outer_fold_id) %>% list()
    , top_variables_xgb = VIP.xgb.p %>% mutate(outer_fold_id = model_dat$outer_fold_id) %>% list()
    , pdp_df            = p1.f      %>% mutate(outer_fold_id = model_dat$outer_fold_id) %>% list()
    )
  )
  
}
prep_for_variable_importance_a  <- function(model_dat, splitted_data) {
  
  ## Prep data for partial dependence plots
  dat_for_pdp <- rbind(
    splitted_data$train_data[[1]]
    , splitted_data$test_data[[1]] %>% filter(index %in% model_dat$train_data[[1]])
  ) %>% dplyr::select(-cases) %>%
    mutate(
      outbreak          = factor(outbreak, levels = c(1, 0))
    , forecast_interval = as.factor(forecast_interval)
    ) %>% 
    group_by(forecast_interval) %>% 
    mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
    , weights = ifelse(outbreak == "0", 1, weights)
    , weights = hardhat::importance_weights(weights)
    , .after = "index"
    ) %>% 
    mutate(month = month(date), .after = date) %>%
    ## Reduce size for getting partial dependence plot
    group_by(shapeName, month) %>%
    sample_n(size = 1, replace = n() < 1) %>%
    ungroup()
  
  return(
    bind_cols(
      model_dat
    , tibble(
      dat_for_pdp       = dat_for_pdp %>% list()
    )
   )
  )
  
}

#' Across all fitted models, compare variable importance

compare_vi <- function(variable_importance) {
  
  vi.gg <- variable_importance$top_variables_xgb %>% 
    do.call("rbind", .) %>% 
    group_by(Feature) %>%
    mutate(avg_gain = mean(Gain)) %>%
    arrange(avg_gain) %>% 
    ungroup() %>% 
    mutate(Feature = factor(Feature, levels = unique(Feature))) %>% {
    ggplot(., aes(Gain, Feature)) + 
      geom_point(aes(group = outer_fold_id)) + 
      theme(
        axis.text.x = element_text(size = 10)
      , axis.text.y = element_text(size = 10)
      , strip.text.x = element_text(size = 7)
      ) 
  }
  
  pdp.gg <- variable_importance$pdp_df %>% do.call("rbind", .) %>% {
    ggplot(., aes(value, yhat)) + 
      geom_line(aes(group = outer_fold_id)) + 
      theme(
        axis.text.x = element_text(size = 10)
      , axis.text.y = element_text(size = 10)
      , strip.text.x = element_text(size = 7)
      ) +
      facet_wrap(~variable)
  }
  
  return(
    tibble(
        vi_gg  = vi.gg %>% list()
      , pdp_gg = pdp.gg %>% list()
    )
  )
  
}

