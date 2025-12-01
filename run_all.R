# ==============================================================================
# MASTER SCRIPT: Euro Area Economic Analysis
# ==============================================================================
# Author:   Ibraheem Saqib Ellahi
# Date:     November 2025
# Purpose:  Execute the complete analysis pipeline from raw data to final results.
#
# Directory Structure:
#   Root/
#   ├── run_all.R             (This script)
#   ├── code/                 (Analysis scripts)
#   │   ├── 01_data_cleaning.R
#   │   ├── 02_data_visualization.R
#   │   └── 03_regressions.R
#   ├── data/                 (Raw and processed data)
#   └── output/               (Results)
# ==============================================================================

# 1. Setup Environment
# ------------------------------------------------------------------------------
# Clear workspace to ensure reproducibility
rm(list = ls())

# Set the working directory to the project root (where this script is located)
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Install/Load 'pacman' to manage dependencies for all child scripts
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tidyr, broom, kableExtra, knitr, 
               fixest, ggplot2, purrr, ggrepel, lubridate, readxl, stringr)

# Ensure data and output directories exist in the project root
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("output")) dir.create("output")

message("Starting Analysis Pipeline...")

# 2. Run Data Cleaning
# ------------------------------------------------------------------------------
# Note: Scripts are now sourced from the 'code/' folder
message("\n[1/3] Running Data Cleaning (code/01_data_cleaning.R)...")
if (file.exists("code/01_data_cleaning.R")) {
  source("code/01_data_cleaning.R", echo = TRUE)
  message(">> Data Cleaning Complete. 'data/final_data.csv' created.")
} else {
  stop("Error: code/01_data_cleaning.R not found.")
}

# 3. Run Data Visualization
# ------------------------------------------------------------------------------
message("\n[2/3] Running Visualization & Summary Stats (code/02_data_visualization.R)...")
if (file.exists("code/02_data_visualization.R")) {
  source("code/02_data_visualization.R", echo = TRUE)
  message(">> Visualization Complete. Plots saved to 'output/'.")
} else {
  stop("Error: code/02_data_visualization.R not found.")
}

# 4. Run Regressions
# ------------------------------------------------------------------------------
message("\n[3/3] Running Regression Analysis (code/03_regressions.R)...")
if (file.exists("code/03_regressions.R")) {
  source("code/03_regressions.R", echo = TRUE)
  message(">> Regression Analysis Complete. Tables saved to 'output/'.")
} else {
  stop("Error: code/03_regressions.R not found.")
}

# 5. Final Message
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("SUCCESS: All scripts executed successfully.\n")
cat("Results are available in the 'output/' folder.\n")
cat("==============================================================================\n")