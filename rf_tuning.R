# Random forest tuning ---

# load package(s) ---
library(tidyverse)
library(tidymodels)

# Seed
set.seed(2023)

# load required objects ----
load("data_setup_median_pca.rda.nosync.rda")

# define model
rf_model <- rand_forest(
  mode = "regression",
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("ranger")

# set-up tuning grid ----
rf_params <- parameters(rf_model) %>%
  update(mtry = mtry(range = c(2, 5)))

# define grid
rf_grid <- grid_regular(rf_params, levels = 10)

# Random forest workflow ----
rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(data_recipe)

# Tuning/Fitting ----
rf_tune <- rf_workflow %>%
  tune_grid(resamples = data_fold, grid = rf_grid)

# Write out results & workflow
save(rf_tune, rf_workflow, file = "rf_res.rda")
