#### Preamble ####
# Purpose: Run a linear regression model to predict 
# Author: Deyi Kong
# Date: December 3rd, 2024
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
lm_model <- lm(Amount ~ Contract + Buyer + ContractDays + PhaseDays,
                 data = cleaned_data
)

# full Bayesian model
bayesian_model_full <- stan_glm(
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

# Bayesian model
bayesian_model <- stan_glm(
  Amount ~ ContractType + BuyerCleaned + ContractDays + PhaseDays,
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
saveRDS(lm_model, here::here("models/lm_model.rds"))
saveRDS(bayesian_model, here::here("models/bayesian_model.rds"))
saveRDS(bayesian_model_full, here::here("models/bayesian_model_full.rds"))

