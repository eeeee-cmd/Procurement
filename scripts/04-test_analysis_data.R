#### Preamble ####
# Purpose: This script performs a series of validation tests on procurement data.
# Author: Deyi Kong
# Date: November 25th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, `here`, `arrow`, and `testthat` packages must be installed.
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)
library(here)
library(arrow)
library(testthat)

# load data
cleaned_data <- read_parquet(here::here("data/analysis_data/procurement_cleaned.parquet"))

# create list for results
test_results <- list()

# test structure of data
test_results$structure <- test_that("Data structure", {
  expect_true(is.data.frame(cleaned_data), info = "clean_data is not a data frame.")
  expect_equal(ncol(cleaned_data), 10, info = "clean_data does not have the correct number of columns.")
  expect_equal(nrow(cleaned_data), 948, info = "clean_data does not have the correct number of rows.")
})

# test if column names match expected names
test_results$column_names_test <- test_that("Column names", {
  expected_colnames <- c(
     "Contract", "Buyer", "Supplier", "Amount", "AwardDate", "StartDate", "EndDate", "PreparatoryPhase", "ContractDays", "PhaseDays"
  )
  expect_equal(names(cleaned_data), expected_colnames, info = "Column names do not match expected names.")
})

# test if certain columns contain NA values
test_results$na_values_test <- test_that("NA values", {
  expect_false(any(is.na(cleaned_data$Amount)), info = "Contract amount contains NA values.")
  expect_false(any(is.na(cleaned_data$StartDate)), info = "StartDate contains NA values.")
  expect_false(any(is.na(cleaned_data$AwardDate)), info = "AwardDate contains NA values.")
  expect_false(any(is.na(cleaned_data$EndDate)), info = "EndDate contains NA values.")
})

# test if StartDate is before EndDate
test_results$date_logic_test <- test_that("Date logic", {
  expect_true(all(cleaned_data$StartDate <= cleaned_data$EndDate, na.rm = TRUE),
    info = "Some StartDate values are not before or equal to EndDate."
  )
})

print(test_results)

