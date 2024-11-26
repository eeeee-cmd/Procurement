#### Preamble ####
# Purpose: Validate the regression models through out of sample testing and RMSE
# Author: Deyi Kong
# Date: November 24th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, 'caret', and 'here' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj


# load libraries
library(tidyverse)
library(here)
library(caret)

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
  ) %>%
  mutate(PollRecency = as.numeric(Sys.Date() - StartDate))
  

# split the data into training (70%) and testing (30%) sets
set.seed(123) # Set seed for reproducibility
train_index <- createDataPartition(cleaned_data$CandidateName, p = 0.7, list = FALSE)
train_data <- cleaned_data[train_index, ]
test_data <- cleaned_data[-train_index, ]



write_parquet(rmse_results, here::here("data/analysis_data/rmse_results.parquet"))
