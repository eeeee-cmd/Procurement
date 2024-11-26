#### Preamble ####
# Purpose: This script reads and cleans raw procurement data,
# performing various transformations, and outputs the cleaned data to a CSV file.
# Author: Deyi Kong
# Date: November 25th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, 'here', and 'arrow' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(here)
library(arrow)

# Read data
data <- read_csv(here::here("data/raw_data/procurement.csv"))

# # Check if the contract region are all federal, then we will exclude it from the cleaned data
# table(data$region)

# Data cleasupplier# Data cleaning by renaming and selecting specific columns, date formatting, filtering invalid entries
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
        # To replace missing (NA) start date with the value of awarded date column
        StartDate = case_when(
          is.na(StartDate) ~ AwardDate,
          TRUE ~ StartDate
        )
      ) %>%
      mutate(
        # Specify the format of the date for proper parsing
        StartDate = mdy(StartDate), # Use mdy() for MM/DD/YYYY format
        AwardDate = mdy(AwardDate), 
        EndDate = mdy(EndDate)
      ) %>%
      mutate(
        # Change the $format of dollar amount into numeric numbers
        Amount = as.numeric(gsub("[\\$,]", "", Amount))
      ) %>%
      # Drop contracts where EndDate is before StartDate or AwardDate due to error with unknown reason
      filter(!(EndDate <= StartDate | EndDate <= AwardDate))
  }, # error handling to mitigate any cleaning issues
  error = function(e) {
    message("An error occurred during data cleaning: ", e)
    NULL
  }
)

# Add preparatory phase and contract total days
cleaned_data <- cleaned_data %>%
  mutate(PreparatoryPhase = as.numeric(StartDate - AwardDate),
         ContractDays = as.numeric(EndDate - StartDate))

# # Summary all the variables
# sort(table(cleaned_data$Contract), decreasing = T)
# sort(table(cleaned_data$Buyer), decreasing = T)
# sort(table(cleaned_data$Supplier), decreasing = T)
# summary(cleaned_data$Amount)
# summary(cleaned_data$StartDate)
# summary(cleaned_data$AwardDate)
# summary(cleaned_data$EndDate)

# Standardize the supplier's name due to capitalization and notation differences
# Remove all spaces first
cleaned_data$Supplier <- toupper(gsub("\\s+", "", cleaned_data$Supplier))

# Remove unnecessary symbols
cleaned_data$Supplier <- gsub("[.,]", "", cleaned_data$Supplier)

# Define a custom function for substitutions
clean_supplier_names <- function(supplier) {
  substitutions <- c(
    "MICROSOFT" = "Microsoft",
    "CANADA" = " Canada",
    "THROUGHSOFTCHOICE/SSC" = "Through softchoice/SSC",
    "THROUGHSSC" = "Through softchoice/SSC",
    "LICENSING" = " Licensing",
    "CORPORATION" = " Corp.",
    "CORP" = " Corp.",
    "COMPANY" = " Co.",
    "CO" = " Co.",
    "MIICROSOFT" = "Microsoft",
    "LICENCING" = " Licensing",
    "CANAADA" = " Canada",
    "INC" = " Inc.",
    "GP" = ", GP",
    "GIP" = ", GIP",
    ".-TORONTO-POBOX9433" = "."
  )
  for (pattern in names(substitutions)) {
    supplier <- gsub(pattern, substitutions[pattern], supplier)
  }
  return(supplier)
}

# Apply the function to clean supplier names
cleaned_data <- cleaned_data %>%
  mutate(Supplier = clean_supplier_names(Supplier))

sort(table(cleaned_data$Supplier), decreasing = T)

# MIGHT GOING TO ADD SOME CLEANING PROCESS ABOUT CONTRACT CATEGORY

# Save cleaned data as new csv and parquet if successful, print error if not
if (!is.null(cleaned_data)) {
  write_csv(cleaned_data, here::here("data/analysis_data/procurement_cleaned.csv"))
  write_parquet(cleaned_data, here::here("data/analysis_data/procurement_cleaned.parquet"))
  print(head(cleaned_data))
} else {
  message("Cleaned data not available for saving due to an error.")
}

