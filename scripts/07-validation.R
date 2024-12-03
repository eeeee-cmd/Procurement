#### Preamble ####
# Purpose: Validate the regression models through out of sample testing and RMSE
# Author: Deyi Kong
# Date: December 3rd, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, `here`, 'randomForest', and 'arrow' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj


# load libraries
library(tidyverse)
library(here)
library(caret)
library(arrow)
library(tibble)
library(rstanarm)

# load data
cleaned_data <- read_parquet(here::here("data/analysis_data/procurement_cleaned.parquet"))

# data processing
cleaned_data <- cleaned_data %>%
  mutate(
    Contract = as.factor(Contract),
    Buyer = as.factor(Buyer),
    Supplier = as.factor(Supplier),
    AwardDate = as.Date(AwardDate),
    StartDate = as.Date(StartDate),
    EndDate = as.Date(EndDate)
  )

# split the data into training (80%) and testing (20%) sets
set.seed(123) # Set seed for reproducibility
train_index <- createDataPartition(cleaned_data$Amount, p = 0.8, list = FALSE)
train_data <- cleaned_data[train_index, ]
test_data <- cleaned_data[-train_index, ]

# linear model for microsoft contract dollar amount based on various predictors
lm_model <- lm(Amount ~ Contract + Buyer + ContractDays + PhaseDays,
               data = cleaned_data)

# Make predictions on the testing set using the linear model
predictions_lm <- predict(lm_model, newdata = test_data)

# Extract actual values for RMSE calculation
actual_values <- test_data$Amount

# Calculate RMSE (Root Mean Squared Error) for the linear model
rmse_lm <- sqrt(mean((actual_values - predictions_lm)^2, na.rm = TRUE))

# Save the linear model results into a parquet file
lm_results <- tibble(
  Model = "Linear Regression",
  RMSE = rmse_lm,
  Coefficients = list(coef(lm_model)),
  Predictions = list(predictions_lm)
)

# Save linear model results to a parquet file
write_parquet(lm_results, here::here("data/analysis_data/lm_model_results.parquet"))

# Fit a Bayesian model using rstanarm
bayesian_model <- stan_glm(
  Amount ~ Contract + Buyer + ContractDays + PhaseDays,
  data = cleaned_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),   # Prior for coefficients
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),  # Prior for intercept
  prior_aux = exponential(rate = 1, autoscale = TRUE),  # Prior for residual SD
  seed = 123,
  iter = 4000,  # Increase the total number of iterations
  warmup = 1000  # Allocate half the iterations for warm-up
)

# Make predictions on the testing set using the Bayesian model
predictions_bayes <- predict(bayesian_model, newdata = test_data)

# Calculate RMSE for the Bayesian model
rmse_bayes <- sqrt(mean((actual_values - predictions_bayes)^2, na.rm = TRUE))

# Save the Bayesian model results into a parquet file
bayesian_results <- tibble(
  Model = "Bayesian Model",
  RMSE = rmse_bayes,
  Coefficients = list(coef(bayesian_model)),
  Predictions = list(predictions_bayes)
)

# Save Bayesian model results to a parquet file
write_parquet(bayesian_results, here::here("data/analysis_data/bayesian_model_results.parquet"))

# Print the RMSE results for both models
print(tibble(
  Model = c("Linear Regression", "Bayesian Model"),
  RMSE = c(rmse_lm, rmse_bayes)
))

