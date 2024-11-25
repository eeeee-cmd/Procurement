#### Preamble ####
# Purpose: Run a linear regression model to predict 
# Author: Deyi Kong
# Date: November 24th, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` and 'here' packages must be installed
# Any other information needed? Make sure you are in the `Procurement` rproj

# load libraries
library(tidyverse)



# save modified data for the model as a CSV + Parquet and save models as RDS
write_csv(cleaned_data, here::here("data/analysis_data/data.csv"))
write_parquet(cleaned_data, here::here("data/analysis_data/data.parquet"))
saveRDS(lm_model_1, here::here("models/lm_model_1.rds"))
