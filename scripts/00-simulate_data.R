#### Preamble ####
# Purpose: Simulate data 
# Author: Deyi Kong
# Date: November 28th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(arrow)

# set seed for reproducability
set.seed(123)

# define possible values to simulate data
contract <- c("License/Maintenance fees", "Communication Services", "Computer Services", "Application software", "Production and operations")
buyer <- c("Shared Services Canada", "Employment and Social Development Canada", "Royal Canadian Mounted Police", "Global Affairs Canada", "National Defence")
supplier <- c("MICROSOFT CANADA INC.", "BELL TELEPHONE CO. OF CANADA", "MICROSOFT CANADA CO.", "BELL CANADA")
n <- 100

# generate random start and end dates for each poll
award_dates <- sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), n, replace = TRUE)
start_dates <- award_dates + sample(1:30, n, replace = TRUE)
end_dates <- start_dates + sample(1:300, n, replace = TRUE)

# create a simulated data frame of polls with various attributes
procurement_data <- data.frame(
  contract = sample(contract, n, replace = TRUE),
  buyer = sample(buyer, n, replace = TRUE),
  supplier = sample(supplier, n, replace = TRUE),
  amount = sample(40000:450000, n, replace = TRUE),
  award_date = award_dates,
  start_date = start_dates,
  end_date = end_dates,
  preparatory_phase = as.numeric(start_dates - award_dates),
  contract_days = as.numeric(end_dates - start_dates)
) %>%
  mutate(
    phase_days = abs(preparatory_phase)
  )

# write the cleaned and simulated polling data to a CSV file
write_csv(procurement_data, here::here("data/simulated_data/simulated_data.csv"))
write_parquet(procurement_data, here::here("data/simulated_data/simulated_data.parquet"))

