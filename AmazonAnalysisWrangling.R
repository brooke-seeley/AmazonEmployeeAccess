library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(ggmosaic)
library(embed)
library(forcats)

## Read in Training Data

trainData <- vroom('train.csv')

## Read in Test Data

testData <- vroom('test.csv')

## EDA

trainData_plot <- trainData_plot %>%
  mutate(ROLE_DEPTNAME = fct_lump_n(factor(ROLE_DEPTNAME), 10),
         ACTION = factor(ACTION),
         RESOURCE = fct_lump_n(factor(RESOURCE), 10))

ggplot(trainData_plot, aes(x = ROLE_DEPTNAME, fill = ACTION)) +
  geom_bar(position = "fill")

ggplot(trainData_plot, aes(x = RESOURCE, fill = ACTION)) +
  geom_bar(position = "fill")

## Recipe

my_recipe <- recipe(ACTION ~ ., data = trainData) %>%
  step_mutate(ACTION = factor(ACTION)) %>%  # target as factor
  step_mutate_at(all_of(c('RESOURCE', 'MGR_ID', 'ROLE_ROLLUP_1', 
                          'ROLE_ROLLUP_2', 'ROLE_DEPTNAME', 'ROLE_TITLE', 
                          'ROLE_FAMILY_DESC', 'ROLE_FAMILY', 'ROLE_CODE')), 
                 fn = factor) %>% 
  step_other(all_of(c('RESOURCE', 'MGR_ID', 'ROLE_ROLLUP_1', 'ROLE_ROLLUP_2', 
                      'ROLE_DEPTNAME', 'ROLE_TITLE', 'ROLE_FAMILY_DESC', 
                      'ROLE_FAMILY', 'ROLE_CODE')), threshold = 0.001) %>%
  step_dummy(all_of(c('RESOURCE', 'MGR_ID', 'ROLE_ROLLUP_1', 
                      'ROLE_ROLLUP_2', 'ROLE_DEPTNAME', 'ROLE_TITLE', 
                      'ROLE_FAMILY_DESC', 'ROLE_FAMILY', 'ROLE_CODE')))
prep <- prep(my_recipe)
baked <- bake(prep, new_data = trainData)