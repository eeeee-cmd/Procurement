#### Preamble ####
# Purpose: This script reads and cleans raw procurement data,
# performing various transformations, and outputs the cleaned data to a CSV file.
# Author: Deyi Kong
# Date: December 2nd, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, 'here', and 'arrow' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(dplyr)
library(tidyverse)
library(here)
library(arrow)
library(lubridate)

# Read data
data <- read_csv(here::here("data/raw_data/procurement_microsoft.csv"))

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
      filter(!(EndDate <= StartDate | EndDate <= AwardDate)) %>%
      # Drop Na for amount
      filter(!is.na(Amount) & !is.na(StartDate)) %>%
      # drop the significant large contract
      filter(Amount != 102260150)
  }, # error handling to mitigate any cleaning issues
  error = function(e) {
    message("An error occurred during data cleaning: ", e)
    NULL
  }
)

# Add preparatory phase and contract total days
cleaned_data <- cleaned_data %>%
  mutate(
    PreparatoryPhase = as.numeric(StartDate - AwardDate),
    ContractDays = as.numeric(EndDate - StartDate),
    PhaseDays = abs(PreparatoryPhase)
    ) %>%
  # Filter out the contracts before 2020
  mutate(
    Year = as.integer(format(as.Date(AwardDate), "%Y")),
    Month = as.integer(format(as.Date(AwardDate), "%m"))
    ) %>%
  filter(Year != 2019)

# # Summary all the variables
# sort(table(cleaned_data$Buyer), decreasing = T)
# sort(table(cleaned_data$Supplier), decreasing = T)
 summary(cleaned_data)

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
    "LICENSING" = " Licensing",
    "CORPORATION" = " Corp.",
    "CORP" = " Corp.",
    "COMPANY" = " Co.",
    "CO" = " Co.",
    "INC" = " Inc.",
    "LTD" = " Ltd.",
    "MIICROSOFT" = "Microsoft",
    "LICENCING" = " Licensing",
    "CANAD" = " Canada",
    "THROUGHSOFTCHOICE/SSC" = "Through softchoice/SSC",
    "THROUGHSSC" = "Through softchoice/SSC",
    "GP" = ", GP",
    "GIP" = ", GIP",
    ".-TORONTO-POBOX9433" = ".",
    "OF Canada" = "OF",
    "OF" = " of Canada",
    "Canada-OTTAWAON" = "Canada",
    "CANAADA" = " Canada"
  )
  for (pattern in names(substitutions)) {
    supplier <- gsub(pattern, substitutions[pattern], supplier)
  }
  return(supplier)
}

# Apply the function to clean supplier names
cleaned_data <- cleaned_data %>%
  mutate(Supplier = clean_supplier_names(Supplier))

# # Check variables
# sort(table(cleaned_data$Supplier), decreasing = T)
# summary(cleaned_data)

# Categorize based on keywords
cleaned_data <- cleaned_data %>%
  mutate(
    ContractType = case_when( 
      grepl("License/Maintenance fees", cleaned_data$Contract) ~ "License/Maintenance Fees Related", 
      grepl("LICENSE/MAINTENANCE FEES", cleaned_data$Contract) ~ "License/Maintenance Fees Related", 
      grepl("LIC/MAINT FEES", cleaned_data$Contract) ~ "License/Maintenance Fees Related", 
      grepl("Lic/Maint fees", cleaned_data$Contract) ~ "License/Maintenance Fees Related", 
      grepl("License and Maintenance Fees", cleaned_data$Contract) ~ "License/Maintenance Fees Related", 
      grepl("Computer services", cleaned_data$Contract) ~ "Computer Services Related",
      grepl("Computer Services", cleaned_data$Contract) ~ "Computer Services Related",
      grepl("COMPUTER SERVICES", cleaned_data$Contract) ~ "Computer Services Related",
      grepl("production and operation", cleaned_data$Contract) ~ "Production and Operation Related",
      grepl("Production and Operations", cleaned_data$Contract) ~ "Production and Operation Related",
      grepl("Distributed Computing Environment", cleaned_data$Contract) ~ "Distributed Computing Environment Related",
      grepl("distributed computing environment", cleaned_data$Contract) ~ "Distributed Computing Environment Related",
      grepl("DCE", cleaned_data$Contract) ~ "Distributed Computing Environment Related",
      grepl("Voice communications services", cleaned_data$Contract) ~ "Voice Communications Services Related",
      grepl("Voice Communications Services", cleaned_data$Contract) ~ "Voice Communications Services Related",
      grepl("Voice communication services", cleaned_data$Contract) ~ "Voice Communications Services Related",
      grepl("Equipment", cleaned_data$Contract) ~ "Equipment Related",
      grepl("equipment", cleaned_data$Contract) ~ "Equipment Related",
      grepl("COMP EQ", cleaned_data$Contract) ~ "Equipment Related",
      grepl("EQUIPMENT", cleaned_data$Contract) ~ "Equipment Related",
      grepl("consultants", cleaned_data$Contract) ~ "Consulting Related",
      grepl("Consultants", cleaned_data$Contract) ~ "Consulting Related",
      grepl("CONSULTANTS", cleaned_data$Contract) ~ "Consulting Related",
      grepl("consulting", cleaned_data$Contract) ~ "Consulting Related",
      grepl("database access", cleaned_data$Contract) ~ "Database Access Services Related",
      grepl("Database Access", cleaned_data$Contract) ~ "Database Access Services Related",
      grepl("Application software", cleaned_data$Contract) ~ "Database Access Services Related",
      grepl("Data Communications Services", cleaned_data$Contract) ~ "Data Communications Services Related",
      grepl("Data communications services", cleaned_data$Contract) ~ "Data Communications Services Related",
      grepl("installations", cleaned_data$Contract) ~ "Installations Related",
      grepl("Installatons", cleaned_data$Contract) ~ "Installations Related",
      grepl("Software", cleaned_data$Contract) ~ "Software Related",
      grepl("SOFTWARE", cleaned_data$Contract) ~ "Software Related",
      grepl("software", cleaned_data$Contract) ~ "Software Related",
      TRUE ~ "Other"
      )) %>%
  mutate(
    BuyerCleaned = case_when( 
      grepl("Employment and Social Development Canada", cleaned_data$Buyer) ~ "Employment and Social Development Canada", 
      grepl("Global Affairs Canada", cleaned_data$Buyer) ~ "Global Affairs Canada", 
      grepl("National Defence", cleaned_data$Buyer) ~ "National Defence", 
      grepl("Natural Resources Canada", cleaned_data$Buyer) ~ "Natural Resources Canada",
      grepl("National Research Council Canada", cleaned_data$Buyer) ~ "National Research Council Canada",  
      grepl("Transport Canada", cleaned_data$Buyer) ~ "Transport Canada",
      grepl("Indigenous Services Canada", cleaned_data$Buyer) ~ "Indigenous Services Canada",
      grepl("Natural Sciences and Engineering Research Council of Canada", cleaned_data$Buyer) ~ "Natural Sciences and Engineering Research Council of Canada",  
      grepl("Shared Services Canada", cleaned_data$Buyer) ~ "Shared Services Canada",  
      grepl("Social Sciences and Humanities Research Council of Canada", cleaned_data$Buyer) ~ "Social Sciences and Humanities Research Council of Canada",  
      grepl("Health Canada", cleaned_data$Buyer) ~ "Health Canada",
      TRUE ~ "Other"
    ))

# # Check contractrs
# sort(table(cleaned_data$ContractType), decreasing = T)

# Save cleaned data as new csv and parquet if successful, print error if not
if (!is.null(cleaned_data)) {
  write_csv(cleaned_data, here::here("data/analysis_data/procurement_cleaned.csv"))
  write_parquet(cleaned_data, here::here("data/analysis_data/procurement_cleaned.parquet"))
  print(head(cleaned_data))
} else {
  message("Cleaned data not available for saving due to an error.")
}

