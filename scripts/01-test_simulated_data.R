#### Preamble ####
# Purpose: This script reads in the simulated data, performs a series of validation tests
# to ensure the data structure, column names, and values are correct, and reports the results.
# Author: Deyi Kong
# Date: November 24th, 2024
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
  expect_equal(ncol(data), 25, info = "procurement_data does not have the correct number of columns.")
  expect_equal(nrow(data), 100, info = "procurement_data does not have the correct number of rows.")
})

# test if the column names match the expected column names
test_results$column_names <- test_that("Column names", {
  expected_colnames <- c(
    "contract", "buyer", "start_date", "end_date"
  )
  expect_equal(colnames(data), expected_colnames, info = "Column names do not match expected names.")
})


# print the test results
print(test_results)
