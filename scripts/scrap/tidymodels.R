library(targets)
library(tidyverse)
library(tidymodels)

tar_load(model_data)

model_data$outbreak_30 <- factor(model_data$outbreak_30)

n <- model_data |> filter(date <= "2017-12-31") |> nrow() 

model_data_split <- initial_time_split(model_data, prop = n/nrow(model_data))

train <- training(model_data_split)
holdout <- testing(model_data_split)

# TODO set base score to the background rate in training data
# TODO add area as term in model and set interactions (see bottom of script)
# TODO use briar score for assessment metric

# where to add base_score? set as background rate of positives in training set
spec <-  parsnip::boost_tree(
  trees = 1000,
  tree_depth = hardhat::tune(),
  min_n = hardhat::tune(),
  loss_reduction = hardhat::tune(),                   
  sample_size = hardhat::tune(), 
  mtry = hardhat::tune(),
  learn_rate = hardhat::tune()
) |>
  parsnip::set_engine("xgboost", num_class = 2, objective = "binary:logistic", base_score = ) |> # add interactions here
  parsnip::set_mode("classification")

grid <- dials::grid_latin_hypercube(
  dials::tree_depth(),
  dials::min_n(),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  dials::finalize(dials::mtry(), model_data),
  dials::learn_rate(),
  size = 2
) 

rec <-  recipe(formula = as.formula(outbreak_30 ~
                                      anomaly_relative_humidity_30 +
                                      anomaly_temperature_30 +
                                      anomaly_precipitation_30 +
                                      anomaly_relative_humidity_60 +
                                      anomaly_temperature_60 +
                                      anomaly_precipitation_60 +
                                      anomaly_relative_humidity_90 +
                                      anomaly_temperature_90 +
                                      anomaly_precipitation_90 +
                                      anomaly_temperature_forecast_29 +
                                      anomaly_precipitation_forecast_29+
                                      anomaly_relative_humidity_forecast_29+
                                      anomaly_ndvi_30 +
                                      anomaly_ndvi_60 +
                                      anomaly_ndvi_90) ,
               data = model_data) 

wf <-  workflows::workflow() |>
  workflows::add_recipe(rec) |>
  workflows::add_model(spec)

# Take 2
rolling_n <- n_distinct(model_data$shapeName)
traintest_splits <- rolling_origin(model_data, 
                                   initial = rolling_n, 
                                   assess = rolling_n, 
                                   skip = rolling_n - 1, 
                                   cumulative = FALSE) |> 
  mutate(
    train_outbreaks = map_int(splits, \(x) sum(analysis(x)$outbreak_30==TRUE)),
    test_outbreaks = map_int(splits, \(x) sum(assessment(x)$outbreak_30==TRUE))
  )

# Rolling approach to prevent any future data leakage
# Make custom approach that can a) handle 0 assessment events, and b) weights ability to predict events
# Does this approach need a holdout dataset? What if we use the best parameters combination and report the assessment performance over time? 
rolling_n <- n_distinct(model_data$shapeName)
splits <- rolling_origin(model_data, 
                         initial = rolling_n, 
                         assess = rolling_n, 
                         skip = rolling_n - 1)

a1 <- analysis(splits$splits[[1]])
a1$date |> table()
b1 <- assessment(splits$splits[[1]])
b1$date |> table()

a2 <- analysis(splits$splits[[2]])
a2$date |> table()
b2 <- assessment(splits$splits[[2]])
b2$date |> table()

a3 <- analysis(splits$splits[[3]])
a3$date |> table()
b3 <- assessment(splits$splits[[3]])
b3$date |> table()


splits_sub <- splits[1,] # Subset the folds, causes to fail because class is lost
class(splits_sub) <- class(splits) # Give it back the class and now it works

tuned <- tune::tune_grid(
  wf,
  resamples = splits_sub,
  grid = grid,
  control = tune::control_grid(verbose = TRUE)
)


tuned$.notes[[1]]$note

tune::show_best(x = tuned, metric = "accuracy")

# note: spatial autocorrelation - not extrapolating in space. rolls forward in time. 

# train from 2005-2017
# second pass to do geographic regional blocking to be able to incorporate 2018-on data

# scoring - set base score to 0.03. That is the baseline probability. that means when training data is all 0, it will still predict some probability above 0
# but xgboost will still classify anything under 0.5 as a negative
# no, xgboost returns logistic likelihood - within fold fitting
# calculate metric as the brier score- this is robust

# have area as a term in the model. if we want to modify a probability, keep that term from interacting with others.
# xgboost allows us to put in interaction constrainst - separate term that is not part of the mix and match of trees
# marginal effect of that term is constant
# there are two terms in model, one is area, one is everythign else. sometime area will split, othertimes it will not. estimates it separatelt
# need to set xgboost interaction constraints
# https://xgboost.readthedocs.io/en/stable/tutorials/feature_interaction_constraint.html
# first group is area, second group is all other variables
# https://github.com/dmlc/xgboost/blob/59d7b8dc72df7ed942885676964ea0a681d09590/R-package/demo/interaction_constraints.R