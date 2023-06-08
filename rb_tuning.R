# radial basis function tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(kernlab)
library(doParallel)

# Seed
set.seed(2023)

# load required objects ----
load("data_setup_logy_median_pca.rda.nosync.rda")

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
rb_grid <- grid_regular(rb_params, levels = 5)

# workflow ----
rb_workflow <- workflow() %>%
  add_model(rb_model) %>%
  add_recipe(data_recipe)

rb_tune <- rb_workflow %>% 
  tune_grid(resamples = data_fold, grid = rb_grid, control = control_grid(save_pred = TRUE,
                                                                                  save_workflow = TRUE,
                                                                                  parallel_over = "everything"))

# Write out results & workflow
save(rb_tune, rb_workflow, file = "rb_tune.rda")
