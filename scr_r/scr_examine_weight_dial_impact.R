####
## Script to explore the impact of the different indices (hyperparameter sets) chosen as 
## a function of different weighting dials and the practical implication of using
## these different sets on predicted probabilities (such as false positive and
## false negative rates)
####

## since we are doing this outside of targets, need to load some targets
{
  tar_load(train_data)
  tar_load(test_data)
  tar_load(tuning_grid)
  tar_load(folded_data_for_fitting)
  tar_load(train_data_fitting)
  tar_load(test_data_fitting)
  tar_load(positive_threshold)
  tar_load(weightings_on_ones)
  tar_load(start_p)
  tar_load(id_cols)
  tar_load(region_map)
  tar_load(performance_hexes)
}

## Set up a diagnostics folder to save output
dir_name <- "outputs/weightval_diagnostics"
dir.create(dir_name)

## Choose a fitting window
which_dates <- c(1, 2, 3, 4, 5)
  
## Loop over hyperparameter sets
for (i in 1:nrow(dial_best_sets$summarized_indices)) {
  
  print("--------------------")
  print("--------------------")
  print("--------------------")
  print(paste("index", i))
  print("--------------------")
  print("--------------------")
  print("--------------------")
  
for (j in seq_along(which_dates)) {
  
  print("--------------------")
  print("--------------------")
  print("--------------------")
  print(paste("date", j))
  print("--------------------")
  print("--------------------")
  print("--------------------")
  
write.csv(
  tuning_grid$par_grid[[1]] |> filter(index == dial_best_sets$summarized_indices[i, ]$index)
, paste0(dir_name, "/", "temp_hyperset.csv"))

## Within each loop fit for the chosen fitting window and save diagnostics to the new folder
fitted_model <- fit_model(
  final_hyper_set = paste0(dir_name, "/", "temp_hyperset.csv")
, full_data       = folded_data_for_fitting[which_dates[j], ]
, train_data      = train_data_fitting
, test_data       = test_data_fitting
, threshold       = positive_threshold
, weightings      = weightings_on_ones
, start_p         = start_p
, id_cols         = id_cols
, out_dir         = dir_name
, overwrite       = FALSE
, DEBUG           = FALSE
, index_boost     = country_index_boost)

model_out_for_eval <- build_model_out_for_eval(
  model_fits = fitted_model
, full_data  = folded_data_for_fitting[which_dates[j], ])

check_fit <- examine_fits_within(
  model_out        = model_out_for_eval
, test_data        = test_data
, regions          = region_map
, larger_districts = performance_hexes
, africa_sf        = path_to_simplifed_regions
, region_to_sum    = NULL
, p_thresh         = positive_threshold
, using_hexes      = using_hexes
, outpath          = dir_name
, outpath_for_for  = dir_name
, outpath_for_app  = dir_name
, purpose          = purpose
, overwrite        = TRUE
  ## Make predictions for a given country
, country_code     = "ZAF")

test_fit <- qread(check_fit)

qsave(test_fit, paste0(dir_name, "/", "check_fit_", j, "_", i, ".qs"))

#variable_importance_prep_a <- prep_for_variable_importance_a(
#  model_dat  = model_out_for_eval[1, ]
#, train_data = train_data
#, test_data  = test_data)

#variable_importance <- calculate_variable_importance(
#  model_dat       = variable_importance_prep_a
#, final_hyper_set = "outputs/hyperparameters/temp_hyperset.csv"
#, fitted_model    = fitted_model
#, fitdir          = dir_name
#, recdir          = dir_name
#, num_vars        = 10)

  }
}

## After the loop is done:

## load them all 
all_runs <- expand.grid(
  indices = seq(nrow(dial_best_sets$summarized_indices))
, dates   = which_dates)

all_fits <- purrr::map(1:nrow(all_runs), .f = function (i) {
  
  this_index <- all_runs[i, ]$indices
  this_date  <- all_runs[i, ]$dates
  
  check_fit <- paste0(dir_name, "/check_fit_", this_date, "_", this_index, ".qs")
  
  print(check_fit)

  test_fit <- qread(check_fit)
  
  tpreds <- test_fit$all_preds[[1]] |>
    left_join(
      test_data |>
        group_by(shapeName) |>
        dplyr::slice(1) |>
        ungroup() |>
        dplyr::select(shapeName, Country)
    )
  
  tpreds |> mutate(index = dial_best_sets$summarized_indices[this_index, ]$index, .before = 1)
  
}) |> bind_rows()

## and compare the output across the chosen fitting window for the different 
 ## hyperparameter sets in a few meaningful ways

## 0) Some needed cleaning
all_fits.s <- all_fits |>
  group_by(index, true_out) |>
  summarize(
    lwr   = quantile(prob_pred, 0.025)
    , lwr_n = quantile(prob_pred, 0.200)
    , mid   = quantile(prob_pred, 0.500)
    , upr_n = quantile(prob_pred, 0.800)
    , upr   = quantile(prob_pred, 0.975)
  )

hex_avg_rank <- all_fits |>
  group_by(shapeName) |>
  summarize(prob_pred = mean(prob_pred)) |>
  arrange(desc(prob_pred)) |>
  mutate(hex_rank = seq(n()))

hex_samples <- seq(1, nrow(hex_avg_rank), by = 250)
hex_samples <- hex_avg_rank[hex_samples, ]$shapeName

## 1) overall density for 0s and 1s
ggplot(all_fits.s |> mutate(index = as.factor(index), true_out = as.factor(true_out)) |>
         mutate(true_out_index = interaction(true_out, index))
       , aes(x = true_out_index, y = mid, colour = true_out)) + 
  geom_errorbar(aes(ymin = lwr_n, ymax = upr_n), width = 0, linewidth = 2) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.4, linewidth = 0.3)


## 2) temporal trends 
all_fits |> 
  filter(forecast_interval == 30) |>
  filter(shapeName %in% hex_samples) |>
  mutate(index = as.factor(index)) %>% {
    ggplot(., aes(date, prob_pred)) + 
      geom_line(aes(colour = index)) +
      scale_y_log10() +
      facet_wrap(~shapeName, scales = "free")
  }

all_fits |> 
  filter(forecast_interval == 30) |>
  group_by(index, shapeName) |>
  summarize(sd_prob = sd(prob_pred)) |>
  mutate(index = as.factor(index)) |>
  group_by(index) |>
  summarize(
    lwr   = quantile(sd_prob, 0.025)
  , lwr_n = quantile(sd_prob, 0.200)
  , mid   = quantile(sd_prob, 0.500)
  , upr_n = quantile(sd_prob, 0.800)
  , upr   = quantile(sd_prob, 0.975)
  ) %>% {
    ggplot(., aes(x = index, y = mid)) +
      geom_errorbar(aes(ymin = lwr_n, ymax = upr_n), width = 0, linewidth = 2) +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.4, linewidth = 0.3)
  }


