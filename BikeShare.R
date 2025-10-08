library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(DataExplorer)
library(patchwork)
library(glmnet)
library(ranger)
setwd("~/GitHub/BikeShare")

# First 19 Days of the month (train) vs. last portion of month (test)
train_data <- vroom("train.csv")
test_data <- vroom("test.csv")

# Method for Avoiding dollar signs
weather <- pull(train_data, weather)

# Using class functions for EDA -------------------------------------------
glimpse(train_data)
plot_intro(train_data)
skim(train_data)

# glimpse(test_data)
# plot_intro(test_data)
# skim(test_data)

# Data Explorer in Particular ---------------------------------------------
plot_intro(train_data)
plot_correlation(train_data)
plot_bar(train_data)
plot_histogram(train_data)
plot_missing(train_data)
GGally::ggpairs(train_data)

# plot_intro(test_data)
# plot_correlation(test_data)
# plot_bar(test_data)
# plot_histogram(test_data)
# plot_missing(test_data)
# GGally::ggpairs(test_data)

# Looking at Weather alone ------------------------------------------------

## Glimpse shows weather is a double, but it needs to be a factor.
Weather_fr<- as.factor(pull(train_data, weather))
## Only 1 data point has weather at 4
V <- sum(weather == 4)

PA <- ggplot(data = train_data, aes(x = as.factor(weather))) + geom_bar() + 
  xlab("Weather Type") + ylab("Count")

PB <- ggplot(data = train_data, aes(x = windspeed)) + geom_histogram() + 
  xlab("Windspeed") + ylab("Count")

PC <- ggplot(data = train_data, aes(x= humidity)) + geom_histogram() + 
  xlab("Humidity") + ylab("Count")

PD <- ggplot(data = train_data, aes(x = temp, y = atemp)) + geom_point() + 
  xlab("Temperature") + ylab("Perceived Temperature")

(PA + PB)/(PC + PD)


# Feature Engineering -----------------------------------------------------

# Cleaning Section

Clean_train <- train_data %>%    # Creating new clean data object
  select(-casual, -registered) %>% # Selecting everything but casual/registered
  mutate(count = log(count)) # Changing count to be the log version of itself

# Recipe Section 

my_recipe <- recipe(count ~ season + holiday + workingday +  # Define recipe
                      weather + temp + atemp + humidity + windspeed + 
                      datetime, data = train_data) %>% 
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>% # Weather 4 to 3
  step_mutate(weather=factor(weather, levels = c(1,2,3))) %>% # Weather to ftr
  step_time(datetime, features = c("hour")) %>%
  step_mutate(season=factor(season, levels = c(1,2,3,4))) %>% # Season to ftr
  step_rm(temp)
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data = test_data)  

# LM section from class ---------------------------------------------------

my_linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_linear_model) %>%
  fit(data=Clean_train)

lin_preds <- predict(bike_workflow, new_data = test_data)


kaggle_submission <- lin_preds %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 

# Score 1.01404



# Penalized Regression ----------------------------------------------------

my_recipe <- recipe(count ~ season + holiday + workingday +  # Define recipe
                      weather + temp + atemp + humidity + windspeed + 
                      datetime, data = train_data) %>%
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>% # Weather 4 to 3
  step_mutate(weather=factor(weather, levels = c(1,2,3))) %>% # Weather to ftr
  step_time(datetime, features = c("hour")) %>%
  step_mutate(season=factor(season, levels = c(1,2,3,4))) %>% # Season to ftr
  step_rm(temp) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_normalize(all_numeric_predictors())

preg_model <- linear_reg(penalty=0, mixture=.5) %>%
  set_engine("glmnet")
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data = Clean_train)
lin_preds <- predict(preg_wf, new_data = test_data)

kaggle_submission <- lin_preds %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 


# My Results --------------------------------------------------------------


VP <- c("00","01","10","11", "0.5") 
Score <- c(1.02299, 1.0234, 1.06889, 1.41486, 1.07042)
Results <- bind_cols(data.frame(VP), data.frame(Score))


# Better Coding? ----------------------------------------------------------

library(tibble)

Results <- c("00","01","10","11","02") %>%
  as.data.frame() %>%
  bind_cols(c(1.02299, 1.0234, 1.06889, 1.41486, 1.07042)) %>%
  setNames(c("VP", "Score"))

Results <- tibble(VP = c("00","01","10","11","02"),
                  Score = c(1.02299, 1.0234, 1.06889, 1.41486, 1.07042))

ggplot(data = Results , aes(x = VP, y = Score), group = 1) + geom_point()


# Tuning ------------------------------------------------------------------

my_recipe <- recipe(count ~ season + holiday + workingday +  # Define recipe
                      weather + temp + atemp + humidity + windspeed + 
                      datetime, data = train_data) %>% 
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>% # Weather 4 to 3
  step_mutate(weather=factor(weather, levels = c(1,2,3))) %>% # Weather to ftr
  step_time(datetime, features = c("hour")) %>%
  step_mutate(season=factor(season, levels = c(1,2,3,4))) %>% # Season to ftr
  step_rm(datetime, temp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) 
  


preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model)

grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 16)

folds <- vfold_cv(Clean_train, v = 5, repeats=1)

CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <-
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=Clean_train)

final_wf %>%
  predict(new_data = test_data)


kaggle_submission <- final_wf %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 


# Regression Trees --------------------------------------------------------

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")


grid_of_tuning_params <- grid_regular(
  tree_depth(),
  cost_complexity(),
  min_n(),
  levels = 4
)

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)



folds <- vfold_cv(Clean_train, v = 5, repeats=1)

CV_results <- bike_workflow %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <-
  bike_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=Clean_train)

predictions <- predict(final_wf, new_data = test_data)

kaggle_submission <- predictions %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 



# Forests -----------------------------------------------------------------
my_recipe <- recipe(count ~ season + holiday + workingday +  # Define recipe
                      weather + temp + atemp + humidity + windspeed + 
                      datetime, data = train_data) %>% 
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>% # Weather 4 to 3
  step_mutate(weather=factor(weather, levels = c(1,2,3))) %>% # Weather to ftr
  step_time(datetime, features = c("hour")) %>%
  step_mutate(season=factor(season, levels = c(1,2,3,4))) %>% # Season to ftr
  step_rm(datetime, temp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) 

my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=1000) %>%
  set_engine("ranger")%>% 
  set_mode("regression")


grid_of_tuning_params <- grid_regular(mtry(range=c(1,7)),
                                      min_n(),
                                      levels = 4)

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)



folds <- vfold_cv(Clean_train, v = 5, repeats=1)

CV_results <- bike_workflow %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <-
  bike_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=Clean_train)

predictions <- predict(final_wf, new_data = test_data)

kaggle_submission <- predictions %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 




# Boosting ----------------------------------------------------------------

my_recipe <- recipe(count ~ season + holiday + workingday +  # Define recipe
                      weather + temp + atemp + humidity + windspeed + 
                      datetime, data = train_data) %>% 
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>% # Weather 4 to 3
  step_mutate(weather=factor(weather, levels = c(1,2,3))) %>% # Weather to ftr
  step_time(datetime, features = c("hour")) %>%
  step_mutate(season=factor(season, levels = c(1,2,3,4))) %>% # Season to ftr
  step_rm(datetime, temp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) 

library(bonsai)
library(lightgbm)

boost_model <- boost_tree(tree_depth = tune(),
                          trees = tune(),
                          learn_rate = tune()) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")


bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(boost_model)

folds <- vfold_cv(Clean_train, v = 5, repeats=1)

grid_of_tuning_params <- grid_regular(
  tree_depth(range = c(1, 10)),
  learn_rate(range = c(-3, -0.1)),
  trees(range = c(10, 50)),
  levels = 4
)

CV_results <- bike_workflow %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <-
  bike_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=Clean_train)

predictions <- predict(final_wf, new_data = test_data)

kaggle_submission <- predictions %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 



# Bart --------------------------------------------------------------------


bart_model <- bart(trees = tune()) %>%
  set_engine("dbarts") %>%
  set_mode("regression")

my_recipe <- recipe(count ~ season + holiday + workingday +  # Define recipe
                      weather + temp + atemp + humidity + windspeed + 
                      datetime, data = train_data) %>% 
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>% # Weather 4 to 3
  step_mutate(weather=factor(weather, levels = c(1,2,3))) %>% # Weather to ftr
  step_time(datetime, features = c("hour")) %>%
  step_mutate(season=factor(season, levels = c(1,2,3,4))) %>% # Season to ftr
  step_rm(datetime, temp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) 

library(bonsai)
library(lightgbm)

boost_model <- boost_tree(tree_depth = tune(),
                          trees = tune(),
                          learn_rate = tune()) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")


bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(bart_model)

folds <- vfold_cv(Clean_train, v = 10, repeats=2)

grid_of_tuning_params <- grid_regular(
  trees(range = c(10, 50)),
  levels = 4
)

CV_results <- bike_workflow %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae))

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <-
  bike_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=Clean_train)

predictions <- predict(final_wf, new_data = test_data)

kaggle_submission <- predictions %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 



# H2O Online Model --------------------------------------------------------

library(agua)

h2o::h2o.init()

auto_model <- auto_ml() %>%
  set_engine("h2o", max_models = 3) %>%
  set_mode("regression")

automl_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(auto_model) %>%
  fit(data = Clean_train)

predictions <- predict(automl_wf, new_data = test_data)

kaggle_submission <- predictions %>%
  bind_cols(.,test_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = exp(count)) %>%
  mutate(count = pmax(0,count)) %>%
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",") 

# Old --------------------------------------------------------------------


# MD <- fit(my_linear_model, formula = count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + datetime, data = train_data)
#   
# bike_predictions <- predict(MD, new_data = test_data)
# bike_predictions
# 
# 
# kaggle_submission <- bike_predictions %>%
#   bind_cols(.,test_data) %>%
#   select(datetime, .pred) %>%
#   rename(count = .pred) %>%
#   mutate(count = pmax(0,count)) %>%
#   mutate(datetime = as.character(format(datetime)))
# 
# vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")  

# Score 1.46851

