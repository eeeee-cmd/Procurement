#### Preamble ####
# Purpose: Perform exploratory data analysis on cleaned data
# Author: Deyi Kong
# Date: November 24th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, 'janitor', 'here', and 'reshape2' packages
# must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(janitor)
library(here)
library(reshape2)
library(arrow)

# read cleaned data
cleaned_data <- read_parquet(here::here("data/analysis_data/procurement_cleaned.parquet")) |>
  clean_names()

# quick overview of data structure
glimpse(cleaned_data)

# print first few rows of data
head(cleaned_data)

# print last few rows of data
tail(cleaned_data)

# randomly sample 6 rows
cleaned_data |>
  slice_sample(n = 6)

# summary of the dataset
summary(cleaned_data)

# number of NA values in each column
missing_data <- cleaned_data |>
  summarise(across(everything(), ~ sum(is.na(.))))
missing_data

