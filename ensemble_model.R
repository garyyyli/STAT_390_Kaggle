# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info ----
load("mlp_tune.rda.nosync.rda")
load("rb_tune.rda.nosync.rda")
load("bt_tune.rda.nosync.rda")

# Load split data object & get testing data
load("test_dat.rda.nosync.rda")

test_dat <- test_dat

# Create data stack ----
data_stack <- stacks() %>% 
  add_candidates(mlp_tune) %>% 
  add_candidates(rb_tune) %>% 
  add_candidates(bt_tune)

# peak at data stack
data_stack

# as_tibble(wildfires_data_stack)

# Fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

# Blend predictions using penalty defined above (tuning step, set seed)
set.seed(2023)
data_blend <- data_stack %>% 
  blend_predictions(penalty = blend_penalty)

# Save blended model stack for reproducibility & easy reference (Rmd report)
save(data_blend, file = "data_blend.rda")

# Explore the blended model stack
data_blend

# autoplot(data_blend, type = "weights") +
#   theme_minimal()
# 
# autoplot(data_blend) +
#   theme_minimal()
# 
# autoplot(data_blend, type = "members") +
#   theme_minimal() +
#   geom_jitter()

# fit the ensemble to entire training set ----
data_model_stack <- data_blend %>% 
  fit_members()

# Save trained ensemble model for reproducibility & easy reference (Rmd report)
save(data_model_stack, file = "data_model_stack.rda")

# Explore and assess trained ensemble model
data_model_stack

# inspect member models
# collect_parameters(data_model_stack, "svm_res") %>% 
#   filter(coef > 0)
# 
# collect_parameters(data_model_stack, "knn_res") %>% 
#   filter(coef > 0)
# 
# collect_parameters(data_model_stack, "lin_reg_res")

# assessment
test_dat <- data_model_stack %>% 
  predict(test_dat) %>% 
  bind_cols(test_dat)

# ggplot(data = data_test, mapping = aes(burned, .pred)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0) +
#   geom_smooth() +
#   theme_minimal() +
#   coord_obs_pred()

# getting predictions for all member models and ensemble
# with the true value
member_preds <- data_model_stack %>% 
  predict(test_dat, members = TRUE) %>% 
  bind_cols(test_dat %>% select(id)) %>% 
  select(id, .pred) %>%
  rename(Id = id) %>%
  rename(y = .pred)

write_csv(member_preds, "output6.csv")

# member_preds %>% 
#   map_dfr(rmse, truth = y, data = member_preds) %>% 
#   mutate(model = colnames(member_preds)) %>% 
#   filter(model != "burned") %>% 
#   arrange(.estimate)






