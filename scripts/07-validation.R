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


write_parquet(rmse_results, here::here("data/analysis_data/rmse_results.parquet"))
