library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)

## Read in Training Data

trainData <- vroom('train.csv') %>%
  mutate(ACTION = factor(ACTION))

## Read in Test Data

testData <- vroom('test.csv')

## EDA
#####
#
# library(ggplot2)
# library(ggmosaic)
# library(forcats)
# 
# trainData_plot <- trainData_plot %>%
#   mutate(ROLE_DEPTNAME = fct_lump_n(factor(ROLE_DEPTNAME), 10),
#          ACTION = factor(ACTION),
#          RESOURCE = fct_lump_n(factor(RESOURCE), 10))
# 
# ggplot(trainData_plot, aes(x = ROLE_DEPTNAME, fill = ACTION)) +
#   geom_bar(position = "fill")
# 
# ggplot(trainData_plot, aes(x = RESOURCE, fill = ACTION)) +
#   geom_bar(position = "fill")
#
#####

## Recipe with Dummy Variables
#####
#
# amazon_recipe <- recipe(ACTION ~ ., data = trainData) %>%
#   step_mutate_at(all_numeric_predictors(), fn = factor) %>% 
#   step_other(all_factor_predictors(), threshold = 0.001) %>%
#   step_dummy(all_factor_predictors())
# 
# prep <- prep(amazon_recipe)
# baked <- bake(prep, new_data = trainData)
#
#####

## Recipe with Target Encoding

target_recipe <- recipe(ACTION ~ ., data = trainData) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% 
  step_other(all_factor_predictors(), threshold = 0.001) %>%
  step_lencode_mixed(all_factor_predictors(), outcome = vars(ACTION)) %>%
  step_normalize(all_factor_predictors())

target_prep <- prep(target_recipe)
bake(target_prep, new_data = trainData)

## Logistic Regression Model - Score: 0.80913
#####
#
# library(glmnet)
# 
# log_reg_model <- logistic_reg() %>%
#   set_engine("glm")
# 
# ### Workflow
# 
# log_reg_workflow <- workflow() %>%
#   add_recipe(amazon_recipe) %>%
#   add_model(log_reg_model) %>%
#   fit(data=trainData)
# 
# ### Predictions
# 
# log_reg_predictions <- predict(log_reg_workflow,
#                                new_data=testData,
#                                type="prob")
# 
# ### Kaggle
# 
# log_reg_kaggle_submission <- log_reg_predictions %>%
#   bind_cols(., testData) %>%
#   select(id, .pred_1) %>% 
#   rename(Action=.pred_1) %>%
#   rename(Id=id)
# 
# vroom_write(x=log_reg_kaggle_submission, file="./LogRegPreds.csv", delim=',')
#
#####

## Penalized Logistic Regression - Score: 0.78320
#####
# 
# library(glmnet)
#
# preg_mod <- logistic_reg(mixture=tune(), penalty=tune()) %>%
#   set_engine("glmnet")
# 
# preg_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(preg_mod)
# 
# ### Grid of values to tune over
# 
# tuning_grid <- grid_regular(penalty(),
#                             mixture(),
#                             levels = 5)
# 
# ### Split data for CV
# 
# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# ### Run the CV
# 
# CV_results <- preg_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics(metric_set(roc_auc)))
# 
# ### Find Best Tuning Parameters
# 
# bestTune <- CV_results %>%
#   select_best(metric="roc_auc")
# print(bestTune)
# 
# ### Finalize the Workflow & fit it
# 
# final_wf <-
#   preg_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)
# 
# ### Predict
# 
# pen_reg_predictions <- final_wf %>%
#   predict(new_data = testData, type="prob")
# 
# ### Kaggle
# 
# pen_reg_kaggle_submission <- pen_reg_predictions %>%
#   bind_cols(., testData) %>%
#   select(id, .pred_1) %>% 
#   rename(Action=.pred_1) %>%
#   rename(Id=id)
# 
# vroom_write(x=pen_reg_kaggle_submission, file="./PenRegPreds.csv", delim=',')
#
#####

## Regression Trees - Score: 0.87309
#####
# 
# library(rpart)
#
# tree_mod <- rand_forest(mtry=tune(),
#                         min_n=tune(),
#                         trees=500) %>%
#   set_engine("ranger") %>%
#   set_mode("classification")
# 
# tree_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(tree_mod)
# 
# ### Grid of values to tune over
# 
# tuning_grid <- grid_regular(mtry(range=c(1,9)),
#                             min_n(),
#                             levels=5)
#   
# ### CV
#   
# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# CV_results <- tree_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics(metric_set(roc_auc)))
# 
# ### Find best tuning parameters
# 
# bestTune <- CV_results %>%
#   select_best(metric="roc_auc")
# 
# ### Finalize workflow
# 
# final_wf <-
#   tree_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)
# 
# ### Predict
# 
# tree_predictions <- final_wf %>%
#   predict(new_data = testData, type="prob")
# 
# ### Kaggle
# 
# tree_kaggle_submission <- tree_predictions %>%
#   bind_cols(., testData) %>%
#   select(id, .pred_1) %>% 
#   rename(Action=.pred_1) %>%
#   rename(Id=id)
# 
# vroom_write(x=tree_kaggle_submission, file="./RegTreePreds.csv", delim=',')
#
#####

## K-Nearest Neighbors - Score: 0.80905
#####
# 
# library(kknn)
# 
# knn_model <- nearest_neighbor(neighbors=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("kknn")
# 
# knn_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(knn_model)
# 
# ### Tuning Parameters
# 
# tuning_grid <- grid_regular(neighbors())
# 
# ### CV
# 
# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# CV_results <- knn_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics(metric_set(roc_auc)))
# 
# ### Find best K
# 
# bestTune <- CV_results %>%
#   select_best(metric="roc_auc")
# 
# ### Finalize Workflow
# 
# final_wf <-
#   knn_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)
# 
# ### Predict
# 
# knn_predictions <- final_wf %>%
#   predict(knn_workflow, new_data=testData, type="prob")
# 
# ### Kaggle
# 
# knn_kaggle_submission <- knn_predictions %>%
#   bind_cols(., testData) %>%
#   select(id, .pred_1) %>% 
#   rename(Action=.pred_1) %>%
#   rename(Id=id)
# 
# vroom_write(x=knn_kaggle_submission, file="./KNNTreePreds.csv", delim=',')
# 
#####

## Naive Bayes - Score: 0.75864
#####

# library(discrim)
# library(naivebayes)
# 
# nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("naivebayes")
# 
# nb_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(nb_model)
# 
# ### Grid of values to tune over
# 
# tuning_grid <- grid_regular(Laplace(),
#                             smoothness(),
#                             levels=5)
# 
# ### CV
# 
# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# CV_results <- nb_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics(metric_set(roc_auc)))
# 
# ### Find best tuning parameters
# 
# bestTune <- CV_results %>%
#   select_best(metric="roc_auc")
# 
# ### Finalize Workflow
# 
# final_wf <-
#   nb_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)
# 
# ### Predict
# 
# nb_predictions <- final_wf %>%
#   predict(nb_workflow, new_data=testData, type="prob")
# 
# ### Kaggle
# 
# nb_kaggle_submission <- nb_predictions %>%
#   bind_cols(., testData) %>%
#   select(id, .pred_1) %>% 
#   rename(Action=.pred_1) %>%
#   rename(Id=id)
# 
# vroom_write(x=nb_kaggle_submission, file="./NBTreePreds.csv", delim=',')
#
#####

## Trying an MLP (Neural Networks)

### Installing Packages

install.packages("remotes")
remotes::install_github("rstudio/tensorflow")

reticulate::install_python()

keras::install_keras()

### New Recipe

nn_recipe <- recipe(ACTION ~ ., data = trainData) %>%
  update_role(MGR_ID, new_role="id") %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% 
  step_other(all_factor_predictors(), threshold = 0.001) %>%
  step_lencode_mixed(all_factor_predictors(), outcome = vars(ACTION)) %>%
  step_normalize(all_factor_predictors()) %>%
  step_range(all_numeric_predictors(), min=0, max=1)

nn_prep <- prep(nn_recipe)
bake(nn_prep, new_data = trainData)

### Neural Network Model

nn_model <- mlp(hidden_units = tune(),
                epochs = 50) %>%
  set_engine("keras") %>%
  set_mode("classification")

nn_workflow <- workflow() %>%
  add_recipe(nn_recipe) %>%
  add_model(nn_model)

### Tuning

nn_tuneGrid <- grid_regular(hidden_units(range=c(1, 50)),
                            levels=5)

folds <- vfold_cv(trainData, v = 5, repeats = 1)

CV_results <- nn_workflow %>%
  tune_grid(resamples=folds,
            grid=nn_tuneGrid,
            metrics(metric_set(roc_auc)))

bestTune <- CV_results %>%
  select_best(metric="roc_auc")

final_wf <-
  nn_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)

### Predict

nn_predictions <- final_wf %>%
  predict(nn_workflow, new_data=testData, type="prob")

### Graph of CV

CV_results %>% collect_metrics() %>%
  filter(.metric=="roc_auc") %>%
  ggplot(aes(x=hidden_units, y=mean)) + geom_line()
