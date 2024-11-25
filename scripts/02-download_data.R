#### Preamble ####
# Purpose: This script loads the raw procurement buyer data from a CSV file for further analysis.
# Author: Deyi Kong
# Date: November 24th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load library
library(readr)

# Read the raw presidential polls data
data <- read_csv(here::here("data/raw_data/procurement.csv"))
