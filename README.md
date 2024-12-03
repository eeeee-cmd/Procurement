# Procurement Dynamics: Analyzing Federal Contract Awards to Microsoft

## Overview

This repository provides readers with all the necessary data, R scripts, and files to understand and reproduce an analysis on Federal Procurement Supplier.

The study analyzes the Federal Procurement Supplier - Microsoft. I utilizes linear regression model to predict the contract dollar amount outcomes. The findings indicate an increasing trend of total contract amount each year. The results emphasize the role of procurement dynamics awards to Microsoft.

The data used in this analysis comes from a combination of publicly available data for Federal Procurement. The analysis leverages R and several libraries. The dataset covers the Supplier, Microsoft, in Federal Procurement Procedure. Obtained from: https://theijf.org/procurement/supplier/microsoft?region=.


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from https://theijf.org/procurement/supplier/microsoft?region=.
-   `data/analysis_data` contains the cleaned datasets that were constructed.
-   `data/simulated_data` contains the simulated data that were constructed.
-   `models` contains fitted models and model API. 
-   `other` contains details about LLM chat interactions and sketches.
-   `other/shiny_app` contains the shiny application.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download, clean, test data, exploratory analysis, modelling and validation.


## Statement on LLM usage

LLMs such as ChatGPT were used to provide procurement context, summarize figures, as well as some coding help. Full usage can be found on `other/llm_usage/usage.txt`.
