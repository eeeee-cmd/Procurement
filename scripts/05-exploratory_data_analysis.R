#### Preamble ####
# Purpose: Perform exploratory data analysis on cleaned data
# Author: Deyi Kong
# Date: December 2nd, 2024
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

# Sum the total amount group by year
yearly_amount <- year_data %>%
  group_by(Year) %>%
  summarize(TotalAmount = sum(Amount, na.rm = TRUE)) %>%
  mutate(TotalAmount = round(TotalAmount / 1000000)) # Convert to millions

# Plot the amount in each year
ggplot(yearly_amount, aes(x = as.factor(Year), y = TotalAmount)) +
  geom_bar(stat = "identity", fill = "darkorange", color = "black", alpha = 0.7) +
  labs(
    title = "Total Contract Amounts by Year for Microsoft",
    x = "Year",
    y = "Total Amount ($million)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))

# create plot for Distribution of Poll Scores by candidate
ggplot(cleaned_data, aes(x = phase_days)) +
  geom_histogram(
    bins = 30,
    fill = "salmon",
    color = "black",
    alpha = 0.7
  ) +
  labs(
    x = "Preparatory Phase (day)",
    y = "Frequency"
  ) +
  theme_minimal()

# create plot for Distribution of Poll Scores by candidate
ggplot(cleaned_data, aes(x = contract_days)) +
  geom_histogram(
    bins = 15,
    fill = "steelblue",
    color = "black",
    alpha = 0.7
  ) +
  labs(
    x = "Contract Performance Phase (day)",
    y = "Frequency"
  ) +
  theme_minimal()
