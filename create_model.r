library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggridges)



# CREATE MODELS ----------------------------------
load(file="data/feature_set.rdata")

tidymodels_prefer()
#
summary(feature_set)

feature_set <- feature_set %>% drop_na()

set.seed(4321)
rates_split <- initial_split(feature_set,prop=0.80,strata = "fut_ret")
rates_train <- training(rates_split)
rates_test <- testing(rates_split)

# test stratification
rates_train %>% ggplot(aes(fut_ret)) +
  geom_density() +
  geom_density(data=rates_test,aes(fut_ret),color = "red")


# linear regresssion
lm_model <- linear_reg() %>%
  set_engine("stan")


lm_form_fit <-
  lm_model %>%
  fit(fut_ret ~ ., data = rates_train)


rates_test %>% bind_cols(predict(lm_form_fit,rates_test)) %>%
  ggplot(aes(fut_ret,.pred)) + geom_point()


# random forest
rf_model <- rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression")


rf_form_fit <-
  rf_model %>%
  fit(fut_ret ~ ., data = rates_train)

# plot predictions
rates_test %>%
  bind_cols(predict(rf_form_fit,rates_test)) %>%
  ggplot(aes(fut_ret,.pred)) + geom_point()

cor(rates_test$fut_ret,predict(rf_form_fit,rates_test))


# use whole set
rates_test %>%
  bind_cols(predict(rf_form_fit,rates_test)) %>%
  ggplot(aes(fut_ret,.pred)) + geom_point()


