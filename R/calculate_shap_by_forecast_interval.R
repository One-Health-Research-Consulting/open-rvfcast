#' For each fitted model compute SHAP values and summarize by forecast_interval
#'
#'
#' @title calculate_shap_by_forecast_interval

#' @param model_dat output of prep_for_shap_a (contains dat_for_shap list-column)
#' @param final_hyper_set The choice of hyperparameters for final model fitting
#' @param fitdir path to saved extracted parsnip fit and recipe
#' @param recdir path to saved recipe
#' @return Tibble of mean absolute and mean signed SHAP values per feature per forecast_interval
#' @author Morgan Kain
#' @export

calculate_shap_by_forecast_interval <- function(model_dat, final_hyper_set, fitdir, recdir) {

  fit_path <- paste(fitdir, "/parsnip_fit_", model_dat$outer_fold_id
                    , "_", final_hyper_set$index
                    , ".Rds", sep = "")
  rec_path <- paste(fitdir, "/recipe_", model_dat$outer_fold_id
                    , "_", final_hyper_set$index
                    , ".Rds", sep = "")

  parsnip_fit   <- readRDS(fit_path)
  saved_recipe <- readRDS(rec_path)

  shap_data          <- model_dat$dat_for_shap[[1]] |> dplyr::select(-weights, -outbreak)
  forecast_intervals <- as.character(shap_data$forecast_interval)

  ## Bake the "shap data" (made with prep_for_shap_a) for maing predictions
   ## with predcontrib = TRUE
  baked_x <- recipes::bake(saved_recipe, new_data = shap_data) |>
    dplyr::select(-dplyr::any_of("outbreak"))

  xgb_model <- parsnip::extract_fit_engine(parsnip_fit)

  ## Do the predictions to get shap (predcontrib = TRUE)
  shap_mat  <- predict(xgb_model, as.matrix(baked_x), predcontrib = TRUE)

  ## Bias estiamte -- not needed for our purposes
  shap_df                   <- as.data.frame(shap_mat[, -ncol(shap_mat)])

  ## Add back forecast interval for summarizing shap by interval
  shap_df$forecast_interval <- forecast_intervals
  shap_df$outer_fold_id     <- model_dat$outer_fold_id

  ## Do the summarization by forecast interval and feature
   ## the thing we care about to understand variable contribution by forecast interval
  shap_df |>
    pivot_longer(
      cols      = -c(forecast_interval, outer_fold_id)
    , names_to  = "feature"
    , values_to = "shap_value"
    ) |>
    dplyr::group_by(outer_fold_id, forecast_interval, feature) |>
    dplyr::summarise(
      mean_abs_shap = mean(abs(shap_value))
    , mean_shap     = mean(shap_value)
    , .groups       = "drop"
    )

}

#' Prepare data for SHAP calculation, stratified by forecast_interval
#'
#'
#' @title prep_for_shap_a

#' @param model_dat row of model_out_for_eval
#' @param splitted_data split_data output
#' @return model_dat with dat_for_shap list-column appended
#' @author Morgan Kain
#' @export

prep_for_shap_a <- function(model_dat, splitted_data) {

  ## Data prep
  dat_for_shap <- rbind(
    splitted_data$train_data[[1]]
  , splitted_data$test_data[[1]] |> dplyr::filter(index %in% model_dat$train_data[[1]])
  ) |>
    dplyr::select(-cases) |>
    dplyr::mutate(
      outbreak          = factor(outbreak, levels = c(1, 0))
    , forecast_interval = as.factor(forecast_interval)
    ) |>
    group_by(forecast_interval) |>
    dplyr::mutate(
      weights = length(which(outbreak == "0")) / length(which(outbreak == "1"))
    , weights = ifelse(outbreak == "0", 1, weights)
    , weights = hardhat::importance_weights(weights)
    , .after  = "index"
    ) |>
    dplyr::ungroup() |>
    ## This chunk makes the shap problem a bit more manageable by extracting a random
     ## value per hex per forecast interval for each month of the year
     ## In theory would be better to do for all values, but this makes the problem
     ## much more manageable as the shap calculation already takes like 15 minutes
     ## per out_fold_id with this.
     ## Shap should be approximately linear in time with rows. This is about 100k rows
     ## per which takes about 15 mintues. Leaving all is about 6 million rows which
     ## would be 15 HOURS.
     ## Possibly better to use say size = 5, though the prediction might already be
     ## pretty stable as is
    dplyr::mutate(month = lubridate::month(date), .after = date) |>
    group_by(shapeName, month, forecast_interval) |>
    dplyr::sample_n(size = 1, replace = n() < 1) |>
    ungroup()

  dplyr::bind_cols(
    model_dat
  , tibble(dat_for_shap = dat_for_shap |> list())
  )

}

#' Aggregate SHAP-by-forecast-interval results across outer folds and plot
#'
#'
#' @title compare_shap_by_forecast_interval

#' @param shap_results list of tibbles from calculate_shap_by_forecast_interval (one per outer fold)
#' @return Tibble with shap_summary, heatmap (absolute), heatmap (relative), and line plot
#' @author Morgan Kain
#' @export

compare_shap_by_forecast_interval <- function(shap_results) {

  d <- shap_results |>
    bind_rows() |>
    ## Summarize across fits across the various outer fold ids to focus on the
     ## overall response across the various fits
    group_by(forecast_interval, feature) |>
    dplyr::summarise(
      mean_abs_shap = mean(mean_abs_shap)
    , mean_shap     = mean(mean_shap)
    , .groups       = "drop"
    ) |>
    dplyr::mutate(forecast_interval = factor(
      forecast_interval
    , levels = as.character(sort(as.numeric(unique(forecast_interval))))
    ))

  ## Extract out the top n features
  top_features <- d |>
    dplyr::filter(!grepl("forecast_interval", feature)) |>
    group_by(feature) |>
    dplyr::summarise(overall_mean_abs_shap = mean(mean_abs_shap), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(overall_mean_abs_shap)) |>
    dplyr::slice_head(n = 20) |>
    pull(feature)

  ## Build the needed data for the heatmap visual
  heatmap_data <- d |>
    dplyr::filter(feature %in% top_features) |>
    group_by(forecast_interval) |>
    dplyr::mutate(rel_shap = mean_abs_shap / sum(mean_abs_shap)) |>
    ungroup() |>
    dplyr::mutate(feature = factor(feature, levels = rev(top_features)))

  ## Heatmap visual (absolute value) of variable importance by forecast_interval
  heatmap_abs_gg <- ggplot(heatmap_data, aes(x = forecast_interval, y = feature, fill = mean_abs_shap)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Mean |SHAP|") +
    labs(x = "Forecast interval (days)", y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10)
    , axis.text.y = element_text(size = 8)
    )

  ## Heatmap visual (relative value) of variable importance by forecast_interval
  heatmap_rel_gg <- ggplot(heatmap_data, aes(x = forecast_interval, y = feature, fill = rel_shap)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Relative SHAP") +
    labs(x = "Forecast interval (days)", y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10)
    , axis.text.y = element_text(size = 8)
    )

  line_data <- heatmap_data |>
    dplyr::mutate(forecast_interval_num = as.numeric(as.character(forecast_interval)))

  ## Feature importance across forecast interval for each feature (line graph here
   ## puts focus on change in each feature across the intervals)
  line_gg <- ggplot(line_data, aes(x = forecast_interval_num, y = mean_abs_shap, color = feature)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = sort(unique(line_data$forecast_interval_num))) +
    labs(x = "Forecast interval (days)", y = "Mean |SHAP|", color = "Feature") +
    theme_minimal()

  tibble(
    shap_raw_data  = d              |> list()
  , shap_summary   = heatmap_data   |> list()
  , heatmap_abs_gg = heatmap_abs_gg |> list()
  , heatmap_rel_gg = heatmap_rel_gg |> list()
  , line_gg        = line_gg        |> list()
  )

}
