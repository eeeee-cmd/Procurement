#### Preamble ####
# Purpose: This script reads and cleans raw procurement data,
# performing various transformations, and outputs the cleaned data to a CSV file.
# Author: Deyi Kong
# Date: November 28th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, 'here', and 'arrow' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(here)
library(arrow)

# Read data
microsoft_data <- read_csv(here::here("data/raw_data/procurement_microsoft.csv"))
# bell_data <- read_csv(here::here("data/raw_data/procurement_bell.csv"))
# 
# # Combine data
# data <- rbind(microsoft_data, bell_data)


data <- microsoft_data

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
      filter(!(EndDate <= StartDate | EndDate <= AwardDate)) %>%
      # Drop Na for amount
      filter(!is.na(Amount))
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
    Year = as.integer(format(as.Date(AwardDate), "%Y"))
    ) %>%
  filter(Year != 2019) %>%
  select(-Year)  # Remove Year column after filtering

# # Summary all the variables
# sort(table(cleaned_data$Buyer), decreasing = T)
# sort(table(cleaned_data$Supplier), decreasing = T)
# summary(cleaned_data)

# # Standardize the supplier's name due to capitalization and notation differences
# # Remove all spaces first
# cleaned_data$Supplier <- toupper(gsub("\\s+", "", cleaned_data$Supplier))
# 
# # Remove unnecessary symbols
# cleaned_data$Supplier <- gsub("[.,]", "", cleaned_data$Supplier)
# 
# # Define a custom function for substitutions
# clean_supplier_names <- function(supplier) {
#   substitutions <- c(
#     "MICROSOFT" = "Microsoft",
#     "BELL" = "Bell",
#     "CANADA" = " Canada",
#     "TELEPHONE" = " Telephone",
#     "LICENSING" = " Licensing",
#     "CORPORATION" = " Corp.",
#     "CORP" = " Corp.",
#     "COMPANY" = " Co.",
#     "CO" = " Co.",
#     "INC" = " Inc.",
#     "LTD" = " Ltd.",
#     "MOBILITÉ" = " Mobility",
#     "MIICROSOFT" = "Microsoft",
#     "LICENCING" = " Licensing",
#     "THEBell" = "Bell",
#     "CANAD" = " Canada",
#     "MEDIA Inc." = "MEDIA",
#     "MEDIA" = " Media Inc.",
#     "MOBILITY" = " Mobility",
#     "DISTRIBUTION" = " Distribution",
#     "ALIANT" = " Aliant",
#     "REGIONAL" = " Regional",
#     "EXPRESSVU" = " Express VU",
#     "THROUGHSOFTCHOICE/SSC" = "Through softchoice/SSC",
#     "THROUGHSSC" = "Through softchoice/SSC",
#     "\\( Telephone-NORTHYORK\\)" = " Telephone",
#     "MTS-WINNIPEGMAN" = "BellMTS",
#     "BellMTS Inc." = "BellMTS",
#     "MTS Inc." = "BellMTS",
#     "MTS" = " MTS Inc.",
#     "GP" = ", GP",
#     "GIP" = ", GIP",
#     "WIRELESS" = " Wireless",
#     ".-TORONTO-POBOX9433" = ".",
#     "OF Canada" = "OF",
#     "OF" = " of Canada",
#     "Canada-OTTAWAON" = "Canada",
#     "CANAADA" = " Canada",
#     "Inc. Co.MMUNICATIONS Inc." = "Inc."
#   )
#   for (pattern in names(substitutions)) {
#     supplier <- gsub(pattern, substitutions[pattern], supplier)
#   }
#   return(supplier)
# }
# 
# # Apply the function to clean supplier names
# cleaned_data <- cleaned_data %>%
#   mutate(Supplier = clean_supplier_names(Supplier))
#
# # Check variables
# sort(table(cleaned_data$Supplier), decreasing = T)
# summary(cleaned_data)

# 
# Separate Microsoft and Bell based on keywords
cleaned_data <- cleaned_data %>%
  mutate(
    Supplier = case_when( 
      grepl("Microsoft", cleaned_data$Supplier) ~ "Microsoft", 
      grepl("MIICROSOFT", cleaned_data$Supplier) ~ "Microsoft", 
      grepl("MICROSOFT", cleaned_data$Supplier) ~ "Microsoft", 
      grepl("BELL", cleaned_data$Supplier) ~ "Bell", 
      grepl("Bell", cleaned_data$Supplier) ~ "Bell", 
      grepl("bell", cleaned_data$Supplier) ~ "Bell",
      grepl("MTS", cleaned_data$Supplier) ~ "Bell",
      TRUE ~ cleaned_data$Supplier
    )) %>%
  mutate(
    Buyer = case_when( 
      grepl("Employment and Social Development Canada", cleaned_data$Buyer) ~ "Employment and Social Development Canada", 
      grepl("Global Affairs Canada", cleaned_data$Buyer) ~ "Global Affairs Canada", 
      grepl("MICROSOFT", cleaned_data$Buyer) ~ "Microsoft", 
      grepl("National Defence", cleaned_data$Buyer) ~ "National Defence", 
      grepl("Natural Resources Canada", cleaned_data$Buyer) ~ "Natural Resources Canada", 
      grepl("Transport Canada", cleaned_data$Buyer) ~ "Transport Canada",
      grepl(" Indigenous Services Canada", cleaned_data$Buyer) ~ " Indigenous Services Canada",
      TRUE ~ "Other"
    ))
# # Check contractrs
# sort(table(cleaned_data$Supplier), decreasing = T)

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
      ))

# # Check contractrs
# sort(table(cleaned_data$ContractType), decreasing = T)

# # Subset data for Bell and Microsoft
# bell_buyers <- unique(cleaned_data$Buyer[cleaned_data$Supplier == "Bell"])
# microsoft_buyers <- unique(cleaned_data$Buyer[cleaned_data$Supplier == "Microsoft"])
# # Find common buyers
# common_buyers <- intersect(bell_buyers, microsoft_buyers)
# # Keep the data only with common buyers
# cleaned_data <- cleaned_data %>%
#   filter(Buyer %in% common_buyers)

# sort(table(cleaned_data$Buyer), decreasing = T)

# Save cleaned data as new csv and parquet if successful, print error if not
if (!is.null(cleaned_data)) {
  write_csv(cleaned_data, here::here("data/analysis_data/procurement_cleaned.csv"))
  write_parquet(cleaned_data, here::here("data/analysis_data/procurement_cleaned.parquet"))
  print(head(cleaned_data))
} else {
  message("Cleaned data not available for saving due to an error.")
}

