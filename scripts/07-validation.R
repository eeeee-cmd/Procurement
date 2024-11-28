#### Preamble ####
# Purpose: Validate the regression models through out of sample testing and RMSE
# Author: Deyi Kong
# Date: November 24th, 2024
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
library(randomForest)

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
lm_model <- lm(Amount ~ ContractType + Buyer + ContractDays + PhaseDays,
                 data = cleaned_data)

# Make predictions on the testing set
predictions <- predict(lm_model, newdata = test_data)

# Extract actual values for RMSE calculation
actual_values <- test_data$Amount

# Calculate RMSE (Root Mean Squared Error)
rmse_value <- sqrt(mean((actual_values - predictions)^2, na.rm = TRUE))

# Store RMSE results in a tibble
rmse_results <- tibble(
  Model = "Linear Regression",
  RMSE = rmse_value
)

# Train a Random Forest model on the training set
rf_model <- randomForest(Amount ~ ContractType + Buyer + ContractDays + PhaseDays, data = train_data, ntree = 500)

# Make predictions on the testing set
predictions_rf <- predict(rf_model, newdata = test_data)

# Extract actual values for RMSE calculation
actual_values_rf <- test_data$Amount

# Calculate RMSE (Root Mean Squared Error) for Random Forest
rmse_rf <- sqrt(mean((actual_values_rf - predictions_rf)^2, na.rm = TRUE))

# Store RMSE results in a tibble (or data.frame if you don't want to use tibble)
rmse_results_rf <- tibble(
  Model = "Random Forest",
  RMSE = rmse_rf
)

# Save RMSE results to a parquet file
write_parquet(rmse_results, here::here("data/analysis_data/lm_rmse_results.parquet"))
write_parquet(rmse_results_rf, here::here("data/analysis_data/rf_rmse_results.parquet"))

