#### Preamble ####
# Purpose: This script loads the raw procurement buyer data from a CSV file for further analysis.
# Author: Deyi Kong
# Date: November 28th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load library
library(readr)

# Read the raw federal procurement data of Microsoft and Bell
microsoft_data <- read_csv(here::here("data/raw_data/procurement_microsoft.csv"))
bell_data <- read_csv(here::here("data/raw_data/procurement_bell.csv"))
