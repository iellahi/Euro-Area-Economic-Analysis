# Euro Area Economic Analysis

This project analyzes the transmission of monetary policy shocks to sovereign yields across the Euro Area, exploring the role of government and private sector debt. The analysis includes data cleaning, summary statistics, time-series and panel regressions, and interaction models.

## Project Structure

The analysis is organized into three sequential scripts, controlled by a master execution script:

* **`run_all.R`**: Master script that executes the entire pipeline.
* **`01_data_cleaning.R`**: Merges EA-MPD (monetary shocks), Eurostat (yields), and ECB (debt) data.
* **`02_data_visualization.R`**: Generates summary statistics and time-series plots.
* **`03_regressions.R`**: Runs all regression tasks (Tasks 1–5) and exports LaTeX tables.
* **`data/`**: Directory for raw input files and the generated `final_data.csv`.
* **`output/`**: Directory where all results (figures `.png` and tables `.tex`) are saved.

## Instructions to Reproduce Results

1.  **Prepare Data:**
    Ensure the following raw data files are in the `data/` folder:
    * `Dataset_EA-MPD.xlsx`
    * `estat_irt_lt_mcby_m.tsv`
    * `ECB Data Portal_20251124224945.csv` (Government Debt)
    * `ECB Data Portal_20251125213045.csv` (Resident Debt)
    * `ECB Data Portal_20251125214629.csv` (Household Loans)

2.  **Run the Analysis:**
    Open `run_all.R` in RStudio or run it from the command line:
    ```R
    source("run_all.R")
    ```
    This will automatically install missing packages (via `pacman`), process the data, and generate all outputs in the `output/` folder.

## Data Dependencies

The analysis relies on the following external datasets:
* **Euro Area Monetary Policy Event-Study Database (EA-MPD):** Monetary policy shock series (OIS-2Y).
* **Eurostat:** Maastricht criterion interest rates (monthly).
* **ECB Data Portal:** Annual Government Debt/GDP, Resident Holdings of Debt, and Household Debt metrics.

## Software and Package Requirements

* **R Version:** 4.0.0 or higher recommended.
* **Required Packages:**
    The script uses `pacman` to manage dependencies. The following packages will be installed automatically if missing:
    * `readr`, `readxl`, `dplyr`, `tidyr`, `stringr`, `lubridate` (Data Manipulation)
    * `fixest`, `broom` (Regression Analysis)
    * `ggplot2`, `ggrepel` (Visualization)
    * `knitr`, `kableExtra` (Table Export)

## Estimated Runtime

* **Total Runtime:** < 1 minute on a standard laptop.
    * Data Cleaning: ~10 seconds
    * Visualization: ~5 seconds
    * Regressions: ~5-10 seconds
