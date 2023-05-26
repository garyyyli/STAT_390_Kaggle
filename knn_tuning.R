# Knn tuning ----

# load package(s) ----
library(tidyverse)
library(tidymodels)

# seed
set.seed(2023)

# load required objects ----
load("data_setup_median_pca.rda.nosync.rda")

# define model ----
knn_model <- nearest_neighbor(
  mode = "regression",
  neighbors = tune()
) %>%
  set_engine("kknn")

# set-up tuning grid ----
knn_params <- parameters(knn_model)

# define grid
knn_grid <- grid_regular(knn_params, levels = 10)

# workflow ----
knn_workflow <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(data_recipe)

# tuning/fit the model
knn_tune <- knn_workflow %>%
  tune_grid(
    resamples = data_fold,
    grid = knn_grid
  )

# write out results & workflow
save(knn_tune, knn_workflow, file = "knn_res.rda")
