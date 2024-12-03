#### Preamble ####
# Purpose: 
# Author: Deyi Kong
# Date: December 2nd, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `plumber`, `rstanarm`, and `tidyverse` packages must be installed
# Any other information needed? No

# Load libraries
library(plumber)
library(rstanarm)
library(tidyverse)

# Load model
model <- readRDS("bayesian_model.rds")

# Define model version
version <- "0.0.1"

# Define variables
variables <- list(
  ContractType = "The category of the contract (only License/Maintenance or Computer Services).",
  BuyerCleaned = "The buyer of the contract (only Employment and Social Development Canada, or Global Affairs Canada).",
  ContractDays = "The duration of the contract",
  PhaseDays = "The duration of the preparatory phase."
)

#* @param ContractType
#* @param BuyerCleaned
#* @param ContractDays
#* @param PhaseDays
#* @get /predict_value

predict_value <- function( 
    ContractType = "License/Maintenance", 
    BuyerCleaned = "Global Affairs Canada", 
    ContractDays = 123, 
    PhaseDays = 30
    ) {
  # Convert inputs to appropriate types
  ContractType <- as.character(ContractType)
  BuyerCleaned <- as.character(BuyerCleaned)
  ContractDays <- as.numeric(ContractDays)
  PhaseDays <- as.numeric(PhaseDays)
  
  #Prepare the payload as a data frame
  payload <- data.frame(
    ContractType = ContractType,
    BuyerCleaned = BuyerCleaned,
    ContractDays = ContractDays,
    PhaseDays = PhaseDays
  )
  
## Extract Posterior samples ##
  # Convert to matrix for easier manipulation
  posterior_samples <- as.matrix(model)
  
  # Define the generative process for prediction
  alpha <- posterior_samples[, "(Intercept)"]
  beta_ContractType <- posterior_samples[, "ContractType"]
  beta_BuyerCleaned <- posterior_samples[, "BuyerCleaned"]
  beta_ContractDays <- posterior_samples[, "ContractDays"]
  beta_PhaseDays <- posterior_samples[, "PhaseDays"]
  
  # Compute the predicted value for the observation
  predicted_value <- alpha + 
    beta_ContractType * ifelse(payload$ContractType == "License/Maintenance", 1, 0) + 
    beta_BuyerCleaned * ifelse(payload$BuyerCleaned == "Employment and Social Development Canada", 1, 0) + 
    beta_ContractDays * payload$ContractDays + 
    beta_PhaseDays * payload$PhaseDays
  
  # Predict
  mean_prediction <- mean(predicted_value)
  
  #Store the output
  result <- list(
    "estimated_value" = mean_prediction
  )
  return(result)
}

