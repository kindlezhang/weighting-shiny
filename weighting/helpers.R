library(survey)

propensity_score_glm <- function(data, response_var, weight_var, categorical_vars, continuous_vars) {
  # Check if the required columns are present
  if (!all(c(response_var, weight_var, categorical_vars, continuous_vars) %in% names(data))) {
    stop("One or more specified columns are not present in the data.")
  }

  if(length(intersect(continuous_vars, categorical_vars)) > 0) {
    stop("Continuous variables should not overlap with categorical variables.")
  }
  
  data = data[,c(categorical_vars, weight_var, continuous_vars, response_var)]

  # Convert categorical variables to factors
  data[categorical_vars] <- lapply(data[categorical_vars], as.factor)
  
  # Create a formula for the model
  formula <- as.formula(paste(response_var, "~", paste(c(categorical_vars,continuous_vars), collapse = " + ")))
  
  # Fit a propensity score model
  ps_model <- glm(formula, data = data, family = binomial(link = "logit"))
  
  # Calculate propensity scores
  pw <- predict(ps_model, type = "response")
  data$WT_psw = 1 / pw
  data$WT_psw[response_var == 0] = 0
  data$adjW_psw = data$weight * data$WT_psw
 
  return(data)
}

propensity_score_stratification <- function(data, response_var, weight_var, categorical_vars, continuous_vars) {
  # Check if the required columns are present
  if (!all(c(response_var, weight_var, categorical_vars, continuous_vars) %in% names(data))) {
    stop("One or more specified columns are not present in the data.")
  }

  if(length(intersect(continuous_vars, categorical_vars)) > 0) {
    stop("Continuous variables should not overlap with categorical variables.")
  }
  
  data = data[,c(categorical_vars, weight_var, continuous_vars, response_var)]

  # Convert categorical variables to factors
  data[categorical_vars] <- lapply(data[categorical_vars], as.factor)
  
  # Create a formula for the model
  formula <- as.formula(paste(response_var, "~", paste(c(categorical_vars,continuous_vars), collapse = " + ")))
  
  # Fit a propensity score model
  ps_model <- glm(formula, data = data, family = binomial(link = "logit"))
  
  # Calculate propensity scores
  pw <- predict(ps_model, type = "response")
  
  pw_class = cut(pw, breaks=quantile(pw,probs=seq(0,1, by=0.2)), 
               include.lowest=TRUE, labels = c(1,2,3,4,5), dig.lab = 7)

  ## Estimated unweighted response rate in each stratum
  pw_rps = group_by(as.data.frame(list("pw_class" = pw_class, "weight" = weight_var, "missing" = data[[response_var]])), pw_class) %>% 
    mutate(
      group_n_sum = length(weight),
      m = sum(missing) / group_n_sum
      ) %>% 
    ungroup() %>% dplyr::select(m) %>% 
    unlist(use.names = FALSE)


  data$WT_pss = 1 / pw_rps
  ## Construct the new weight using the unweighted response rate
  data$adjW_pss = data[[weight_var]] * data$WT_pss
  
  return(data)
}

chaid_method = function(data, response_var, weight_var, categorical_vars) {

  library(CHAID)
  set.seed(2025)

  # Check if the required columns are present
  if (!all(c(response_var, weight_var, categorical_vars) %in% names(data))) {
    stop("One or more specified columns are not present in the data.")
  }

  data = data[,c(categorical_vars, weight_var, response_var)]

  # Convert categorical variables to factors
  data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

  # Create a formula for the model
  formula <- as.formula(paste0("factor(", response_var, " == 1) ~ ", paste(categorical_vars, collapse = " + ")))

  chaidobj <- chaid(formula, data = data)

  classes <- predict(chaidobj)

  #### Calculate weight by using the unweighted response rate in each terminal node of the tree
  chaid_pw = data.frame(data, rclass = attributes(classes)$names) %>% 
    group_by(rclass) %>% 
    mutate(
      m = sum(.data[[response_var]])/length(.data[[weight_var]]),
    ) %>% 
    ungroup() %>% dplyr::select(m) %>% unlist(use.names = FALSE)

  data$rclass = attributes(classes)$names

  data$WT_chaid = 1 / chaid_pw

  ## Construct the new weight
  data$adjW_chaid = data$weight * data$WT_chaid
  
  # return(list(
  #   data = data,
  #   chaid_model = chaidobj
  # ))

  return(data)
}

CART_method = function(data, response_var, weight_var, categorical_vars){

  library(rpart)
  set.seed(2025)

  # Check if the required columns are present
  if (!all(c(response_var, weight_var, categorical_vars) %in% names(data))) {
    stop("One or more specified columns are not present in the data.")
  }

  data = data[,c(categorical_vars, weight_var, response_var)]

  # Convert categorical variables to factors
  data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

  # Create a formula for the model

  formula <- as.formula(paste0("factor(", response_var, " == 1) ~ ", paste(categorical_vars, collapse = " + ")))
  
  cart.tree = rpart(formula, data = data, method = "class")

  ## CART fitted values and assign weights
  cart.fitted = predict(cart.tree, type="prob")

  data$cart.wt = 1/cart.fitted[,2]
  data$adjW_cart = data[[weight_var]] * data$cart.wt
  
  return(data)
}

BART_method = function(data, response_var, weight_var, categorical_vars, continuous_vars){

  library(BART)
  set.seed(2025)

  # Check if the required columns are present
  if (!all(c(response_var, weight_var, categorical_vars, continuous_vars) %in% names(data))) {
    stop("One or more specified columns are not present in the data.")
  }

  if(length(intersect(continuous_vars, categorical_vars)) > 0) {
    stop("Continuous variables should not overlap with categorical variables.")
  }

  # delete rows with missing values in the covariates
  # bart_data = data[complete.cases(data[c(weight_var, categorical_vars, continuous_vars, response_var)]), ]
  bart_data = data[, c(weight_var, categorical_vars, continuous_vars, response_var)]

  y_train = bart_data[[response_var]]
  X_train = bart_data[, c(categorical_vars, continuous_vars)]

  # use Probit BART
  fit_bart = pbart(x.train = as.data.frame(X_train), y.train = y_train)
 
  bart_data$WT_probit = 1 / fit_bart$prob.train.mean
  bart_data$WT_probit[response_var == 0] = 0

  # Construct the new weight
  bart_data$adjW_probit = bart_data[[weight_var]] * bart_data$WT_probit

  return(bart_data)
}

xgboost_method = function(data, response_var, weight_var, categorical_vars, continuous_vars){

  library(xgboost)
  set.seed(2025)

  # Check if the required columns are present
  if (!all(c(response_var, weight_var, categorical_vars, continuous_vars) %in% names(data))) {
    stop("One or more specified columns are not present in the data.")
  }

  if(length(intersect(continuous_vars, categorical_vars)) > 0) {
    stop("Continuous variables should not overlap with categorical variables.")
  }

  # Prepare data for xgboost
  model_data = data %>%
    select(all_of(c(continuous_vars, categorical_vars, weight_var, response_var))) %>%
    na.omit()  

  # Convert categorical variables to factors
  y = model_data[[response_var]]
  X = model.matrix(~ . -1, data = model_data %>% select(-c(response_var, weight_var)))  


  dtrain = xgb.DMatrix(data = X, label = y)

  # Train the XGBoost model
  xgb_fit = xgboost(data = dtrain,
                    objective = "binary:logistic",
                    nrounds = 100,
                    max_depth = 3,
                    eta = 0.1,
                    verbose = 0)

  pred_probs = predict(xgb_fit, newdata = dtrain)

  model_data$WT_xgboost = 1 / pred_probs
  model_data$WT_xgboost[response_var == 0] = 0

  ## Construct the new weight
  model_data$adjW_xgboost = model_data[[weight_var]] * model_data$WT_xgboost

  return(model_data)
}