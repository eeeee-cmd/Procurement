#### Preamble ####
# Purpose: This script reads and cleans raw procurement data,
# performing various transformations, and outputs the cleaned data to a CSV file.
# Author: Deyi Kong
# Date: November 24th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, 'here', and 'arrow' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(here)
library(arrow)

# read data
data <- read_csv(here::here("data/raw_data/procurement.csv"))

# Data cleaning by renaming and selecting specific columns, date formatting, filtering invalid entries
cleaned_data <- tryCatch(
  {
    data %>%
      select(
        contract,
        buyer,
        supplier,
        amount,
        award_date,
        start_date,
        end_date
      ) %>%
      rename(
        Supplier = supplier,
        Contract = contract,
        Buyer = buyer,
        Amount = amount,
        AwardDate = award_date,
        StartDate = start_date,
        EndDate = end_date
      ) %>%
      mutate(
        # Specify the format of the date for proper parsing
        StartDate = mdy(StartDate), # Use mdy() for MM/DD/YYYY format
        AwardDate = mdy(AwardDate), 
        EndDate = mdy(EndDate), 
      ) %>%
      filter(!is.na(StartDate), !is.na(AwardDate), !is.na(EndDate))
  }, # error handling to mitigate any cleaning issues
  error = function(e) {
    message("An error occurred during data cleaning: ", e)
    NULL
  }
)


#unique(cleaned_data$Contract)

# add preparatory phase and contract total days
cleaned_data <- cleaned_data %>%
  mutate(PreparatoryPhase = as.numeric(StartDate - AwardDate),
         ContractTime = as.numeric(EndDate - StartDate))

# MIGHT GOING TO ADD SOME CLEANING PROCESS ABOUT CONTRACT CATEGORY

# save cleaned data as new csv and parquet if successful, print error if not
if (!is.null(cleaned_data)) {
  write_csv(cleaned_data, here::here("data/analysis_data/procurement_cleaned.csv"))
  write_parquet(cleaned_data, here::here("data/analysis_data/procurement_cleaned.parquet"))
  print(head(cleaned_data))
} else {
  message("Cleaned data not available for saving due to an error.")
}

