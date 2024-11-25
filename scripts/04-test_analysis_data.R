#### Preamble ####
# Purpose: This script performs a series of validation tests on procurement data.
# Author: Deyi Kong
# Date: November 24th, 2024
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
  expect_equal(ncol(cleaned_data), 18, info = "clean_data does not have the correct number of columns.")
  expect_equal(nrow(cleaned_data), 15829, info = "clean_data does not have the correct number of rows.")
})

# test if column names match expected names
test_results$column_names_test <- test_that("Column names", {
  expected_colnames <- c(
    "poll_id", "pollster_id", "Pollster", "Party", "sponsors",
    "NumericGrade", "PollScore", "Methodology",
    "State", "StartDate", "EndDate",
    "SampleSize", "Population", "ElectionDate",
    "Stage", "CandidateName", "Percentage", "PollRecency"
  )
  expect_equal(names(cleaned_data), expected_colnames, info = "Column names do not match expected names.")
})

# test if certain columns contain NA values
test_results$na_values_test <- test_that("NA values", {
  expect_false(any(is.na(cleaned_data$Percentage)), info = "Percentage contains NA values.")
  expect_false(any(is.na(cleaned_data$CandidateName)), info = "CandidateName contains NA values.")
})

# test if NumericGrade and PollScore values are within expected range
test_results$numeric_grade_pollscore_test <- test_that("Numeric grade and PollScore values", {
  expect_true(all(cleaned_data$NumericGrade >= 0 & cleaned_data$NumericGrade <= 3, na.rm = TRUE),
    info = "NumericGrade values are out of expected range (0 to 3)."
  )
  expect_true(all(cleaned_data$PollScore >= -2 & cleaned_data$PollScore <= 2, na.rm = TRUE),
    info = "PollScore values are out of expected range (-2 to 2)."
  )
})

# test if sample size is within expected range
test_results$sample_size_test <- test_that("Sample size", {
  expect_true(all(cleaned_data$SampleSize >= 100 & cleaned_data$SampleSize <= 30000, na.rm = TRUE),
    info = "SampleSize values are out of expected range (100 to 30000)."
  )
})

# test if StartDate is before EndDate
test_results$date_logic_test <- test_that("Date logic", {
  expect_true(all(cleaned_data$StartDate <= cleaned_data$EndDate, na.rm = TRUE),
    info = "Some StartDate values are not before or equal to EndDate."
  )
})

# test if ElectionDate is within expected range
test_results$election_date_test <- test_that("Election date range", {
  expect_true(
    all(cleaned_data$ElectionDate >= as.Date("2024-11-01") &
      cleaned_data$ElectionDate <= as.Date("2024-11-08")),
    info = "ElectionDate values are out of expected range (2024-11-01 to 2024-11-08)."
  )
})

# print results
print(test_results)
