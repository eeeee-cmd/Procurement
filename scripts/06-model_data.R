#### Preamble ####
# Purpose: Run a linear regression model to predict 
# Author: Deyi Kong
# Date: December 2nd, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, `rstanarm`, `arrow`, `randomForest` and 'here' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(here)
library(arrow)
library(randomForest)
library(rstanarm)

# convert some columns to the appropriate data types
cleaned_data <- read_parquet(here::here("data/analysis_data/procurement_cleaned.parquet"))
cleaned_data <- cleaned_data %>%
  mutate(
    ContractType = as.factor(ContractType),
    Buyer = as.factor(Buyer),
    Supplier = as.factor(Supplier)
  )

# full linear model for contract dollar amount based on various predictors
lm_model_full <- lm(Amount ~ Contract + Buyer + ContractDays + PhaseDays + Month,
                 data = cleaned_data
)

# linear model for contract dollar amount based on various predictors
lm_model <- lm(Amount ~ ContractType + BuyerCleaned + ContractDays + PhaseDays + Month,
                 data = cleaned_data
               )

# Random Forest model for regression (predicting a continuous outcome)
rf_model <- randomForest(Amount ~ ContractType + BuyerCleaned + ContractDays + PhaseDays + Month,
                         data = cleaned_data, ntree = 500)

# full Bayesian model
bayesian_model_full <- stan_glm(
  Amount ~ Contract + Buyer + ContractDays + PhaseDays + Month,
  data = cleaned_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2, autoscale = TRUE),   # Prior for coefficients
  prior_intercept = normal(location = 0, scale = 2, autoscale = TRUE),  # Prior for intercept
  prior_aux = exponential(rate = 1, autoscale = TRUE),  # Prior for residual SD
  seed = 123,
  iter = 4000,  # Increase the total number of iterations
  warmup = 1000  # Allocate half the iterations for warm-up
)

# Bayesian model
bayesian_model <- stan_glm(
  Amount ~ ContractType + BuyerCleaned + ContractDays + PhaseDays + Month,
  data = cleaned_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2, autoscale = TRUE),   # Prior for coefficients
  prior_intercept = normal(location = 0, scale = 2, autoscale = TRUE),  # Prior for intercept
  prior_aux = exponential(rate = 1, autoscale = TRUE),  # Prior for residual SD
  seed = 123,
  iter = 4000,  # Increase the total number of iterations
  warmup = 1000  # Allocate half the iterations for warm-up
)

# save modified data for the model as a CSV + Parquet and save models as RDS
write_csv(cleaned_data, here::here("data/analysis_data/data.csv"))
write_parquet(cleaned_data, here::here("data/analysis_data/data.parquet"))
saveRDS(lm_model_full, here::here("models/lm_model_full.rds"))
saveRDS(bayesian_model_full, here::here("models/bayesian_model_full.rds"))
saveRDS(lm_model, here::here("models/lm_model.rds"))
saveRDS(rf_model, here::here("models/rf_model.rds"))
saveRDS(bayesian_model, here::here("models/bayesian_model.rds"))

