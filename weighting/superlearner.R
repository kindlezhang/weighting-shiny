library(SuperLearner)
library(xgboost)
library(ranger)
library(tidyverse)

data <- read_csv("weighting/data/example.csv")

data <- data[, -1]  |>
  select(c(age_grp, gender, race, citizen, educ, marital, BMI, response))

outcome <- data$response

# Create a dataframe to contain our explanatory variables.
data <- subset(data, select = -response)

# Check structure of our dataframe.
str(data)
# Review our dimensions.
dim(data)

set.seed(1)

sl <- 
  SuperLearner(Y = outcome, X = data, family = binomial(),
    SL.library = c("SL.mean", "SL.glmnet", "SL.ranger"))

# Review how long it took to run the SuperLearner:
sl$times$everything