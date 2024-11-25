#### Preamble ####
# Purpose: This script reads in the simulated data, performs a series of validation tests
# to ensure the data structure, column names, and values are correct, and reports the results.
# Author: Deyi Kong
# Date: November 25th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, 'arrow', and `testthat` packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(dplyr)
library(arrow)
library(testthat)

# read the simulated data
data <- read_parquet(here::here("data/simulated_data/simulated_data.parquet"))

# create list to store test results
test_results <- list()

# test if the data structure is correct
test_results$structure <- test_that("Data structure", {
  expect_true(is.data.frame(data), info = "procurement_data is not a data frame.")
  expect_equal(ncol(data), 9, info = "procurement_data does not have the correct number of columns.")
  expect_equal(nrow(data), 100, info = "procurement_data does not have the correct number of rows.")
})

# test if the column names match the expected column names
test_results$column_names <- test_that("Column names", {
  expected_colnames <- c(
    "contract", "buyer", "supplier", "amount", "award_date", "start_date", "end_date", "preparatory_phase", "contract_days"
  )
  expect_equal(colnames(data), expected_colnames, info = "Column names do not match expected names.")
})

# test if the amount values are within valid range (40000 to 450000)
test_results$amount <- test_that("amount", {
  expect_true(all(data$amount >= 40000 & data$amount <= 450000), info = "Contract amount values are out of range (40000 to 450000).")
})

# test if the preparatory phase (days) is within the valid range (1 to 30)
test_results$preparatory_phase <- test_that("Preparatory Phase", {
  expect_true(all(data$preparatory_phase >= 1 & data$preparatory_phase <= 30), info = "numeric_grade values are out of range (1 to 30).")
})

# test if the award dates are logically before the start dates
test_results$award_logic <- test_that("Award Date logic", {
  expect_true(all(data$award_date < data$start_date), info = "Some award_date are not before their corresponding start_dates")
})

# test if the start dates are logically before the end dates
test_results$date_logic <- test_that("Date logic", {
  expect_true(all(data$start_date < data$end_date), info = "Some start_dates are not before their corresponding end_dates.")
})

# print the test results
print(test_results)
