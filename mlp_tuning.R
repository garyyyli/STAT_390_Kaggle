# multilayer perceptron tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(kernlab)

# Seed
set.seed(2023)

# load required objects ----
load("data_setup_logy_median_pca.rda.nosync.rda")

# Define model ----
mlp_model <- mlp(
  mode = "regression",
  hidden_units = tune(),
  penalty = tune()
) %>% 
  set_engine("nnet")

# set-up tuning grid ----
mlp_params <- parameters(mlp_model)

# define tuning grid
mlp_grid <- grid_regular(mlp_params, levels = 5)

# workflow ----
mlp_workflow <- workflow() %>%
  add_model(mlp_model) %>%
  add_recipe(data_recipe)

# Tuning/fitting ----
#tic("Multilayer Perceptron")

mlp_tune <- mlp_workflow %>% 
  tune_grid(resamples = data_fold, grid = mlp_grid, control = stacks::control_stack_grid())

# Write out results & workflow
save(mlp_tune, mlp_workflow, file = "mlp_tune.rda")
