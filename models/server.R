#### Preamble ####
# Purpose: Initialize and run the server
# Author: Deyi Kong
# Date: December 2nd, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `plumber` packages must be installed
# Any other information needed? No

# Load libraries
library(plumber)

#### API Setup ####
# Plumb the Plumber API from the plumber.R file, which contains route definitions
serve_model <- plumb("models/plumber.R")

#### Run the API ####
# Start the API server to listen for incoming requests
serve_model$run(port = 8000)