library(SuperLearner)
library(rpart)      # CART
library(xgboost)    # XGBoost
library(ranger)
library(tidyverse)

# data <- read_csv("weighting/data/example.csv")

# data <- data[, -1]  |>
#   select(c(age_grp, gender, race, citizen, educ, marital, BMI, response, smoke, HBP))

# outcome <- data$response

# # Create a dataframe to contain our explanatory variables.
# data <- subset(data, select = -response)

# # Check structure of our dataframe.
# str(data)
# # Review our dimensions.
# dim(data)

# set.seed(1)

# sl <- 
#   SuperLearner(Y = outcome, X = data, family = binomial(),
#     SL.library = c("SL.mean", "SL.glmnet", "SL.ranger"))

# # Review how long it took to run the SuperLearner:
# sl$times$everything



transname = function(method_list){

  SL_method_list = c("SL.mean")

  for (method in method_list) {
    if (method == "propensity score") {
      SL_method_list <- append(SL_method_list, "SL.glm")
    }
    else if (method == "Cart") {
      SL_method_list <- append(SL_method_list, "SL.rpart")
    }
    else if (method == "Random Forest") {
      SL_method_list <- append(SL_method_list, "SL.ranger")
    }
    else if (method == "XGBoost") {
      SL_method_list <- append(SL_method_list, "SL.xgboost")
    }
  } 
  
  SL_method_list
}


SL_plot = function(df, outcome, methods, cv = False){

  (num_cores = RhpcBLASctl::get_num_cores())

  # Use 2 of those cores for parallel SuperLearner.
  # Replace "2" with "num_cores" (without quotes) to use all cores.
  options(mc.cores = num_cores)

  df <- df %>% select(-tail(names(.), 2))

  response = df[[outcome]]

  methods = transname(methods)

  data <- df[, !(names(df) %in% outcome)]

  set.seed(1, "L'Ecuyer-CMRG")

  # sl <- 
  # SuperLearner(Y = outcome, X = data, family = binomial(),
  #   SL.library = methods)
  

  system.time({
    # This will take about 2x as long as the previous SuperLearner.
    cv_sl = CV.SuperLearner(Y = response, X = data, family = binomial(),
                            # For a real analysis we would use V = 10.
                            V = 5,
                            parallel = "multicore",
                            SL.library = c("SL.mean", "SL.glm", "SL.rpart", "SL.ranger", "SL.xgboost"))
  })

  return (cv_sl)
}

review_weights = function(cv_sl) {
  meta_weights = coef(cv_sl)
  means = colMeans(meta_weights)
  sds = apply(meta_weights, MARGIN = 2,  FUN = sd)
  mins = apply(meta_weights, MARGIN = 2, FUN = min)
  maxs = apply(meta_weights, MARGIN = 2, FUN = max)
  # Combine the stats into a single matrix.
  sl_stats = cbind("mean(weight)" = means, "sd" = sds, "min" = mins, "max" = maxs)
  # Sort by decreasing mean weight.
  sl_stats[order(sl_stats[, 1], decreasing = TRUE), ]
}

# data <- read_csv("weighting/data/example.csv")

# data <- data[, -1]  |>
#   select(c(age_grp, gender, race, citizen, educ, marital, BMI, response, smoke, HBP))

# a = SL_plot(df = data, outcome = "response", methods = c("propensity score"))

# plot(a) + theme_bw()

# summary(a)

# print(review_weights(a), digits = 3)
