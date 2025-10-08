library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)

## Read in Training Data

trainData <- vroom('train.csv') %>%
  mutate(ACTION = factor(ACTION))

## Read in Test Data

testData <- vroom('test.csv')

#####

# ## EDA
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

#####

## Recipe

amazon_recipe <- recipe(ACTION ~ ., data = trainData) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% 
  step_other(all_factor_predictors(), threshold = 0.001) %>%
  step_dummy(all_factor_predictors())

prep <- prep(amazon_recipe)
baked <- bake(prep, new_data = trainData)

## Logistic Regression Model

log_reg_model <- logistic_reg() %>%
  set_engine("glm")

### Workflow

log_reg_workflow <- workflow() %>%
  add_recipe(amazon_recipe) %>%
  add_model(log_reg_model) %>%
  fit(data=trainData)

### Predictions

log_reg_predictions <- predict(log_reg_workflow,
                               new_data=testData,
                               type="prob")

### Kaggle

log_reg_kaggle_submission <- log_reg_predictions %>%
  bind_cols(., testData) %>%
  select(id, .pred_1) %>% 
  rename(Action=.pred_1) %>%
  rename(Id=id)

vroom_write(x=log_reg_kaggle_submission, file="./LogRegPreds.csv", delim=',')