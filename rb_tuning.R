# radial basis function tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(kernlab)

# Seed
set.seed(2023)

# load required objects ----
load("data_setup_median_pca.rda.nosync.rda")

# Define model ----
rb_model <- svm_rbf(
  mode = "regression",
  cost = tune(),
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab")

# set-up tuning grid ----
rb_params <- parameters(rb_model)

# define tuning grid
rb_grid <- grid_regular(rb_params, levels = 3)

# workflow ----
rb_workflow <- workflow() %>%
  add_model(rb_model) %>%
  add_recipe(data_recipe)

rb_tune <- rb_workflow %>% 
  tune_grid(resamples = data_fold, grid = rb_grid, control = stacks::control_stack_grid())

# Write out results & workflow
save(rb_tune, rb_workflow, file = "rb_tune.rda")
