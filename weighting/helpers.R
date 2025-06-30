library(survey)

propensity_score_adjustment <- function(data, response_var, weight_var, categorical_vars) {
  # Check if the required columns are present
  if (!all(c(response_var, weight_var, categorical_vars) %in% names(data))) {
    stop("One or more specified columns are not present in the data.")
  }
  
  # Convert categorical variables to factors
  data[categorical_vars] <- lapply(data[categorical_vars], as.factor)
  
  # Create a formula for the model
  formula <- as.formula(paste("!is.na(", response_var, ") ~", paste(categorical_vars, collapse = " + ")))
  
  # Fit a propensity score model
  ps_model <- glm(formula, data = data, family = binomial)
  
  # Calculate propensity scores
  data$propensity_score <- predict(ps_model, type = "response")
  
#   # Create a survey design object with the weights
#   survey_design <- svydesign(ids = ~1, data = data, weights = ~get(weight_var))
  
#   # Adjust weights using the propensity scores
#   adjusted_weights <- with(survey_design, weights * (1 / propensity_score))
  
#   # Add adjusted weights to the original data
#   data$adjusted_weight <- adjusted_weights
  
  return(data)
}


