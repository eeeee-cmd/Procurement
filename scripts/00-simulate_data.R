#### Preamble ####
# Purpose: Simulate data 
# Author: Deyi Kong
# Date: November 24th, 2024
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
contract <- c("Contract 1", "Contract 2", "Contract 3", "Contract 4", "Contract 5")
buyer <- c("Buyer A", "Buyer B", "Buyer C", "Buyer D")
n <- 100

# generate random start and end dates for each poll
start_dates <- sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), n, replace = TRUE)
end_dates <- start_dates + sample(1:30, n, replace = TRUE)

# create a simulated data frame of polls with various attributes
procurement_data <- data.frame(
  contract = sample(contract, n, replace = TRUE),
  buyer = sample(buyer, n, replace = TRUE),
  start_date = start_dates,
  end_date = end_dates
)

# write the cleaned and simulated polling data to a CSV file
write_csv(procurement_data, here::here("data/simulated_data/simulated_data.csv"))
write_parquet(procurement_data, here::here("data/simulated_data/simulated_data.parquet"))
