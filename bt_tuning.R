# Boosted tree tuning ---

# load package(s) ---
library(tidyverse)
library(tidymodels)

# seed
set.seed(2023)

# load required objects ----
load("data_setup_median_pca.rda.nosync.rda")

# define model
bt_model <- boost_tree(
  mode = "regression",
  mtry = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost", importance = "impurity")

# set-up tuning grid ----
bt_params <- parameters(bt_model) %>%
  update(mtry = mtry(range = c(2, 10)),
         learn_rate = learn_rate(range = c(-5, -0.2)))

# define grid
bt_grid <- grid_regular(bt_params, levels = 5)

# Random forest workflow ----
bt_workflow <- workflow() %>%
  add_model(bt_model) %>%
  add_recipe(data_recipe)

# Tuning/Fitting ----
bt_tune <- bt_workflow %>%
  tune_grid(resamples = data_fold, grid = bt_grid)

# Write out results & workflow
save(bt_tune, bt_workflow, file = "bt_tune.rda")
