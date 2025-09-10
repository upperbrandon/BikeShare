library(tidyverse)
library(tidymodels)
library(vroom)

# First Day of the month
train_data <- vroom("train.csv")

#
test_data <- vroom("test.csv")

# Method for Avoiding dollar signs
pull(train_data, weather)