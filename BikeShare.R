library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(DataExplorer)
library(patchwork)

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

PC <- ggplot(data = train_data, aes()) + geom_bar() + 
  xlab("Humidity") + ylab("Count")

PD <- ggplot(data = train_data, aes(x = temp, y = atemp)) + geom_point() + 
  xlab("Temperature") + ylab("Perceived Temperature")

(PA + PB)/(PC + PD)
