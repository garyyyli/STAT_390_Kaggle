# Load packages
library(tidyverse)
library(tidymodels)
library(stringr)
library(janitor)
library(knitr)
library(modelr)
library(skimr)
library(hardhat)
library(naniar)
library(caret)
#library(RANN)
#library(mice)
#library(doMC)


# set seed
set.seed(2468)

# load datasets
test_dat <- read_csv("data/test.csv.nosync.csv") %>%
  clean_names()

# inputting NA values as column median values
test_dat <- data.frame(test_dat)
all_column_median <- apply(test_dat, 2, median, na.rm=TRUE)
for(i in colnames(test_dat)){
  test_dat[,i][is.na(test_dat[,i])] <- all_column_median[i]
}

train_dat <- read_csv("data/train.csv.nosync.csv") %>%
  clean_names()

# inputting NA values as column median values
train_dat <- data.frame(train_dat)
all_column_median <- apply(train_dat, 2, median, na.rm=TRUE)
for(i in colnames(train_dat)){
  train_dat[,i][is.na(train_dat[,i])] <- all_column_median[i]
}

miss_var_summary(train_dat) %>% 
  filter(n_miss > 0)
miss_var_summary(test_dat) %>% 
  filter(n_miss > 0)

# constant_cols <- apply(train_dat, 2, function(x) length(unique(x)) == 1)
# zero_cols <- apply(train_dat, 2, function(x) all(x == 0))
# train_dat <- train_dat[, !(constant_cols | zero_cols)]
# dropped_idy_train_dat <- train_dat %>% 
#   select(-id, -y)
# 
# my_pca <- prcomp(dropped_idy_train_dat, scale. = TRUE)
# 
# my_pca.var <- my_pca$sdev ^ 2
# propve <- my_pca.var / sum(my_pca.var)
# which(cumsum(propve) >= 0.90)[1]
# 
# updated_train_dat <- data.frame(my_pca$x[, 0:83], y = train_dat$y)

# train_dat <- na.omit(train_dat)
# test_dat <- na.omit(test_dat)

set_categorical <- train_dat %>% summarise_all(n_distinct)
set_categorical <- as_tibble(cbind(nms = names(set_categorical), t(set_categorical)))
set_categorical$V2 = as.numeric(as.character(set_categorical$V2))

updated_set_categorical <- set_categorical %>% 
  mutate(is_categorical = case_when(V2 < 30 ~ T,
                                    V2 >=30 ~ F)) %>% 
  filter(is_categorical == TRUE)

categorical_variables <- updated_set_categorical$nms

train_dat$x019 = as.character(train_dat$x019)
train_dat$x025 = as.character(train_dat$x025)
train_dat$x034 = as.character(train_dat$x034)
train_dat$x036 = as.character(train_dat$x036)
train_dat$x046 = as.character(train_dat$x046)
train_dat$x063 = as.character(train_dat$x063)
train_dat$x046 = as.character(train_dat$x046)
train_dat$x063 = as.character(train_dat$x063)
train_dat$x076 = as.character(train_dat$x076)
train_dat$x082 = as.character(train_dat$x082)
train_dat$x089 = as.character(train_dat$x089)
train_dat$x115 = as.character(train_dat$x115)
train_dat$x118 = as.character(train_dat$x118)
train_dat$x137 = as.character(train_dat$x137)
train_dat$x150 = as.character(train_dat$x150)
train_dat$x163 = as.character(train_dat$x163)
train_dat$x167 = as.character(train_dat$x167)
train_dat$x190 = as.character(train_dat$x190)
train_dat$x195 = as.character(train_dat$x195)
train_dat$x255 = as.character(train_dat$x255)
train_dat$x277 = as.character(train_dat$x277)
train_dat$x297 = as.character(train_dat$x297)
train_dat$x303 = as.character(train_dat$x303)
train_dat$x309 = as.character(train_dat$x309)
train_dat$x312 = as.character(train_dat$x312)
train_dat$x381 = as.character(train_dat$x381)
train_dat$x385 = as.character(train_dat$x385)
train_dat$x388 = as.character(train_dat$x388)
train_dat$x403 = as.character(train_dat$x403)
train_dat$x404 = as.character(train_dat$x404)
train_dat$x405 = as.character(train_dat$x405)
train_dat$x413 = as.character(train_dat$x413)
train_dat$x423 = as.character(train_dat$x423)
train_dat$x439 = as.character(train_dat$x439)
train_dat$x453 = as.character(train_dat$x453)
train_dat$x454 = as.character(train_dat$x454)
train_dat$x465 = as.character(train_dat$x465)
train_dat$x502 = as.character(train_dat$x502)
train_dat$x511 = as.character(train_dat$x511)
train_dat$x516 = as.character(train_dat$x516)
train_dat$x518 = as.character(train_dat$x518)
train_dat$x532 = as.character(train_dat$x532)
train_dat$x543 = as.character(train_dat$x543)
train_dat$x556 = as.character(train_dat$x556)
train_dat$x561 = as.character(train_dat$x561)
train_dat$x567 = as.character(train_dat$x567)
train_dat$x569 = as.character(train_dat$x569)
train_dat$x577 = as.character(train_dat$x577)
train_dat$x586 = as.character(train_dat$x586)
train_dat$x594 = as.character(train_dat$x594)
train_dat$x611 = as.character(train_dat$x611)
train_dat$x612 = as.character(train_dat$x612)
train_dat$x617 = as.character(train_dat$x617)
train_dat$x621 = as.character(train_dat$x621)
train_dat$x643 = as.character(train_dat$x643)
train_dat$x648 = as.character(train_dat$x648)
train_dat$x658 = as.character(train_dat$x658)
train_dat$x670 = as.character(train_dat$x670)
train_dat$x699 = as.character(train_dat$x699)
train_dat$x703 = as.character(train_dat$x703)
train_dat$x724 = as.character(train_dat$x724)
train_dat$x725 = as.character(train_dat$x725)
train_dat$x727 = as.character(train_dat$x727)
train_dat$x734 = as.character(train_dat$x734)
train_dat$x735 = as.character(train_dat$x735)
train_dat$x742 = as.character(train_dat$x742)
train_dat$x743 = as.character(train_dat$x743)
train_dat$x761 = as.character(train_dat$x761)

# train_dat <- train_dat %>%
#   mutate_if(is.character, as.factor)
# 
# preProcValues <- preProcess(as.matrix(train_dat),
#                             method = c("knnImpute"),
#                             k = 5,
#                             knnSummary = mean)
# 
# impute_training_info <- predict(preProcValues, train_dat, na.action = na.pass)
# 
# procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)
# for(i in procNames$col){
#   impute_training_info[i] <- impute_training_info[i]*preProcValues$std[i]+preProcValues$mean[i] 
# }

# mice_imputed <- data.frame(
#   original = train_dat,
#   imputed_cart = complete(mice(train_dat, method = "cart"))$missing_variables$variable
# )
# 
# mice_imputed
# 
# miss_var_summary(train_dat) %>% 
#   filter(n_miss > 0)
# 
# miss_var_summary(impute_training_info) %>% 
#   filter(n_miss > 0)

train_dat <- as_tibble(train_dat)
train_dat <- train_dat %>% 
  mutate_if(is.character, as.factor)

test_dat <- as_tibble(test_dat)

# folding training data
data_fold <- vfold_cv(train_dat, v = 10, repeats = 5, strata = y)

# creating the recipe
# data_recipe <- recipe(y ~ . , data = train_dat) %>%
#   step_rm(id) %>%
#   step_zv(all_predictors()) %>%
#   step_other(all_nominal(), -all_outcomes(), threshold = 0.2) %>%
#   step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
#   step_normalize(all_numeric(), -y)

data_recipe <- recipe(y ~ . , data = train_dat) %>%
  step_rm(id) %>%
  #step_other(all_numeric_predictors(), threshold = 0.2) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -y) %>%
  step_pca(all_numeric_predictors(), threshold = 0.9) #%>%
  #step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)

# prepping and baking recipe
data_recipe %>%
  prep(train_dat) %>%
  juice()

# saving set up code for tuning
save(test_dat, train_dat, data_fold, data_recipe, file = "data_setup_median_pca.rda")

# creating the KNN recipe
data_recipe <- recipe(y ~ . , -id, data = train_dat) %>%
  step_rm(id) %>%
  step_impute_knn(neighbors = 5) %>% 
  step_other(all_nominal(), -all_outcomes(), threshold = 0.2) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -y)

# prepping and baking recipe
data_recipe %>%
  prep(train_dat) %>%
  bake(new_data = NULL)

# saving set up code for tuning
save(test_dat, train_dat, data_fold, data_recipe, file = "data_knn_setup.rda")

# model evaluation (RF)
load(file = "rf_res.rda")

tune_results <- tibble(
  model_type = c("Random Forest"),
  tune_info = list(rf_tune),
  assessment_info = map(tune_info, collect_metrics),
  best_model = map(tune_info, ~ select_best(.x, metric = "rmse"))
)

tune_results %>%
  select(model_type, assessment_info) %>%
  group_by(model_type) %>%
  unnest(assessment_info) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  rename("Model Type" = model_type)

rf_workflow_tuned <- rf_workflow %>%
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

rf_results <- fit(rf_workflow_tuned, train_dat)

finalresults <- rf_results %>%
  predict(new_data = test_dat) %>%
  bind_cols(test_dat %>% select(id)) %>%
  select(id, .pred) %>%
  rename(Id = id) %>%
  rename(y = .pred)

write_csv(finalresults, "output.csv")

# model evaluation (KNN)
load(file = "knn_res.rda")

tune_results <- tibble(
  model_type = c("KNN"),
  tune_info = list(knn_tune),
  assessment_info = map(tune_info, collect_metrics),
  best_model = map(tune_info, ~ select_best(.x, metric = "rmse"))
)

tune_results %>%
  select(model_type, assessment_info) %>%
  group_by(model_type) %>%
  unnest(assessment_info) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  rename("Model Type" = model_type)

knn_workflow_tuned <- knn_workflow %>%
  finalize_workflow(select_best(knn_tune, metric = "rmse"))

knn_results <- fit(knn_workflow_tuned, train_dat)

finalresults <- knn_results %>%
  predict(new_data = test_dat) %>%
  bind_cols(test_dat %>% select(id)) %>%
  select(id, .pred) %>%
  rename(Id = id) %>%
  rename(y = .pred)

write_csv(finalresults, "output2.csv")

# model evaluation (BT)
load(file = "bt_tune.rda")

tune_results <- tibble(
  model_type = c("BT"),
  tune_info = list(bt_tune),
  assessment_info = map(tune_info, collect_metrics),
  best_model = map(tune_info, ~ select_best(.x, metric = "rmse"))
)

tune_results %>%
  select(model_type, assessment_info) %>%
  group_by(model_type) %>%
  unnest(assessment_info) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  rename("Model Type" = model_type)

bt_workflow_tuned <- bt_workflow %>%
  finalize_workflow(select_best(bt_tune, metric = "rmse"))

bt_results <- fit(bt_workflow_tuned, train_dat)

finalresults <- bt_results %>%
  predict(new_data = test_dat) %>%
  bind_cols(test_dat %>% select(id)) %>%
  select(id, .pred) %>%
  rename(Id = id) %>%
  rename(y = .pred)

write_csv(finalresults, "output3.csv")

# model evaluation (MLP)
load(file = "mlp_tune.rda")

tune_results <- tibble(
  model_type = c("mlp"),
  tune_info = list(mlp_tune),
  assessment_info = map(tune_info, collect_metrics),
  best_model = map(tune_info, ~ select_best(.x, metric = "rmse"))
)

tune_results %>%
  select(model_type, assessment_info) %>%
  group_by(model_type) %>%
  unnest(assessment_info) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  rename("Model Type" = model_type)

mlp_workflow_tuned <- mlp_workflow %>%
  finalize_workflow(select_best(mlp_tune, metric = "rmse"))

mlp_results <- fit(mlp_workflow_tuned, train_dat)

finalresults <- mlp_results %>%
  predict(new_data = test_dat) %>%
  bind_cols(test_dat %>% select(id)) %>%
  select(id, .pred) %>%
  rename(Id = id) %>%
  rename(y = .pred)

write_csv(finalresults, "output4.csv")


