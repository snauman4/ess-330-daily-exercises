# Samantha Nauman
# Date: 3/25/2025
# Daily Exercise 15

# load the tidymodels package and the penguins dataset
library(tidymodels) 
library(palmerpenguins)
library(ranger)
library(dplyr)
data("penguins")
glimpse("penguins")

# set a seed
set.seed(123)

# split the data into training/testing sets 70/30 
penguins <- drop_na(penguins)
penguins_strata <- initial_split(penguins, prop = .7)

# extract the training and test tibbles into unique objects
train_strata <- training(penguins_strata)
test_strata <- testing(penguins_strata)

# create 10-fold cross validation dataset based on the training data
penguins_folds <-
  vfold_cv(train_strata, v = 10,
           strata = species)

# define multinomial logistic regression model, there are 3 species types
multinom_model <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")

# define random forest model
rand_forest_model <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification") 

# make workflow_set to compare both models
penguins_wf_set <- 
  workflow_set(
    preproc = list(species ~ .),
    models = list(multinom = multinom_model, rf = rand_forest_model)
  )

# fit both models with 10-fold cross validation.
library(workflowsets)
penguins_res <- penguins_wf_set %>%
  workflow_map("fit_resamples", resamples = penguins_folds, 
               control = control_resamples(save_pred = TRUE))

# metrics
penguins_res_metrics <- collect_metrics(penguins_res)

# compare
penguins_res_metrics <- penguins_res_metrics %>%
  rename(metric = .metric)
accuracy_comparison <-
  penguins_res_metrics %>%
  filter(metric == "accuracy")

# print comparison
print(accuracy_comparison)
  