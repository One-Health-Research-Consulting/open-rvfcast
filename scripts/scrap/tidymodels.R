library(targets)
library(tidyverse)
library(tidymodels)

tar_load(model_data)

model_data$outbreak_30 <- factor(model_data$outbreak_30)

n <- model_data |> filter(date <= "2017-12-31") |> nrow() 

model_data_split <- initial_time_split(model_data, prop = n/nrow(model_data))

train <- training(model_data_split)
holdout <- testing(model_data_split)

base_score <- sum(train$outbreak_30==TRUE)/nrow(train)

# TODO add area as term in model and set interactions (see bottom of script)

spec <-  parsnip::boost_tree(
  trees = 1000,
  tree_depth = hardhat::tune(),
  min_n = hardhat::tune(),
  loss_reduction = hardhat::tune(),                   
  sample_size = hardhat::tune(), 
  mtry = hardhat::tune(),
  learn_rate = hardhat::tune()
) |>
  parsnip::set_engine("xgboost", 
                      objective = "binary:logistic", 
                      base_score = base_score, # set the background/intercept rate - this allows the tree to split even when the training is all negatives
                      interaction_constraints = '[[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14], [15]]') |> # do not interact on area
  parsnip::set_mode("classification")

grid <- dials::grid_latin_hypercube(
  dials::tree_depth(),
  dials::min_n(),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  dials::finalize(dials::mtry(), train),
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
                                      anomaly_ndvi_90 + 
                                      area) ,
               data = train) 

#juice(prep(rec)) |> names()

wf <-  workflows::workflow() |>
  workflows::add_recipe(rec) |>
  workflows::add_model(spec)

# Rolling approach to prevent any future data leakage
# Make custom approach that can a) handle 0 assessment events, and b) weights ability to predict events
# Does this approach need a holdout dataset? What if we use the best parameters combination and report the assessment performance over time? 
rolling_n <- n_distinct(train$shapeName)
splits <- rolling_origin(train, 
                         initial = rolling_n, 
                         assess = rolling_n, 
                         skip = rolling_n - 1)


splits_sub <- splits[1,] # Subset the folds, causes to fail because class is lost
class(splits_sub) <- class(splits) # Give it back the class and now it works

tuned <- tune::tune_grid(
  wf,
  resamples = splits_sub,
  grid = grid,
  metrics = metric_set(brier_class),# scoring probabilities instead of class
  control = tune::control_grid(verbose = TRUE)
)

tuned$.metrics
tune::show_best(x = tuned, metric = "brier_class")



# notes: 

# spatial autocorrelation - not extrapolating in space. rolls forward in time. 

# train from 2005-2017
# second pass to do geographic regional blocking to be able to incorporate 2018-on data

# set base score to 0.01. That is the baseline probability. that means when training data is all 0, it will still predict some probability above 0

# have area as a term in the model. if we want to modify a probability, keep that term from interacting with others.
# xgboost allows us to put in interaction constrainst - separate term that is not part of the mix and match of trees
# marginal effect of that term is constant
# there are two terms in model, one is area, one is everythign else. sometime area will split, othertimes it will not. estimates it separatelt
# need to set xgboost interaction constraints
# https://xgboost.readthedocs.io/en/stable/tutorials/feature_interaction_constraint.html
# first group is area, second group is all other variables
# https://github.com/dmlc/xgboost/blob/59d7b8dc72df7ed942885676964ea0a681d09590/R-package/demo/interaction_constraints.R


# a1 <- analysis(splits$splits[[1]])
# a1$date |> table()
# b1 <- assessment(splits$splits[[1]])
# b1$date |> table()
# 
# a2 <- analysis(splits$splits[[2]])
# a2$date |> table()
# b2 <- assessment(splits$splits[[2]])
# b2$date |> table()
# 
# a3 <- analysis(splits$splits[[3]])
# a3$date |> table()
# b3 <- assessment(splits$splits[[3]])
# b3$date |> table()