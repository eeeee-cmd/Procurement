#### Preamble ####
# Purpose: Run a linear regression model to predict 
# Author: Deyi Kong
# Date: November 28th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, `arrow`, `randomForest` and 'here' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(here)
library(arrow)
library(randomForest)

# convert some columns to the appropriate data types
cleaned_data <- read_parquet(here::here("data/analysis_data/procurement_cleaned.parquet"))
cleaned_data <- cleaned_data %>%
  mutate(
    ContractType = as.factor(ContractType),
    Buyer = as.factor(Buyer),
    Supplier = as.factor(Supplier)
  )

# linear model for microsoft contract dollar amount based on various predictors
lm_model_m <- lm(Amount ~ ContractType + Buyer + ContractDays + PhaseDays,
                 data = cleaned_data
                 %>% filter(Supplier == "Microsoft")
                 )
# linear model for bell contract dollar amount based on various predictors
lm_model_b <- lm(Amount ~ ContractType + Buyer + ContractDays + PhaseDays,
                  data = cleaned_data
                 %>% filter(Supplier == "Bell")
                 )

# Train a Random Forest model for regression (predicting a continuous outcome)
rf_model <- randomForest(Amount ~ ContractType + Buyer + ContractDays + PhaseDays,
                         data = cleaned_data, ntree = 500)

# save modified data for the model as a CSV + Parquet and save models as RDS
write_csv(cleaned_data, here::here("data/analysis_data/data.csv"))
write_parquet(cleaned_data, here::here("data/analysis_data/data.parquet"))
saveRDS(lm_model_m, here::here("models/lm_model_microsoft.rds"))
saveRDS(lm_model_b, here::here("models/lm_model_bell.rds"))
saveRDS(rf_model, here::here("models/rf_model.rds"))

