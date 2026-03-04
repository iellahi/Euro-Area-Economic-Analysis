# ==============================================================================
# Economic Data Processing
# ==============================================================================
# Purpose: Process raw EA-MPD, ECB, and Eurostat data into a single monthly panel.
# Output:  data/final_data.csv

# 1. Load Libraries (using pacman for robust dependency management)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, readr, dplyr, tidyr, lubridate, stringr)

# ==============================================================================
# STEP 1: Process Monetary Policy Shocks (EA-MPD)
# ==============================================================================
# Goal: Aggregate daily shock data (OIS_2Y) to a monthly level.

file_path_mpd <- "data/Dataset_EA-MPD.xlsx"

# Import as text to safely handle mixed date formats (Excel serial vs Strings)
raw_mpd <- read_excel(file_path_mpd, sheet = "Monetary Event Window", col_types = "text")

# Helper function to parse mixed dates
clean_mpd_dates <- function(x) {
  dates <- as.Date(rep(NA, length(x)))
  
  # Case A: Excel Serial Numbers (e.g., "36167")
  is_serial <- str_detect(x, "^[0-9]+$") & !is.na(x)
  if (any(is_serial)) {
    dates[is_serial] <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")
  }
  
  # Case B: Text Dates (e.g., "14-12-20")
  is_text <- !is_serial & !is.na(x)
  if (any(is_text)) {
    dates[is_text] <- dmy(x[is_text], quiet = TRUE)
  }
  return(dates)
}

# Apply date cleaning
mpd_data <- raw_mpd %>%
  mutate(clean_date = clean_mpd_dates(date))

# Fix chronological year issues (e.g., "2020" appearing after 2023 in the raw data)
fixed_dates <- mpd_data$clean_date
for (i in 2:length(fixed_dates)) {
  curr <- fixed_dates[i]
  prev <- fixed_dates[i-1]
  
  if (!is.na(curr) && !is.na(prev)) {
    # If the year jumps back to 2020 but we were already at 2023+, correct it
    if (year(curr) == 2020 && year(prev) >= 2023) {
      guess_year <- year(prev)
      year(curr) <- guess_year
      
      # If fixing the year still leaves it chronologically behind (e.g. Jan vs Dec), add 1 year
      if (curr < prev) year(curr) <- guess_year + 1
      
      fixed_dates[i] <- curr
    }
  }
}
mpd_data$clean_date <- fixed_dates

# Aggregate to Monthly Level
df_mps_monthly <- mpd_data %>%
  mutate(
    OIS_2Y = as.numeric(OIS_2Y),
    month_date = floor_date(clean_date, "month") # Align to 1st of month
  ) %>%
  filter(!is.na(month_date)) %>%
  group_by(month_date) %>%
  summarise(mps_shock_ois2y = sum(OIS_2Y, na.rm = TRUE)) %>%
  ungroup()


# ==============================================================================
# STEP 2: Process Annual Country Data (ECB)
# ==============================================================================
# Goal: Merge Govt Debt, Resident Holdings, and HH Debt into one annual dataset.

# Helper function to clean ECB column names (extracts the 2-letter country code)
clean_ecb_names <- function(df, pattern) {
  names(df) <- names(df) %>% str_replace_all(pattern, "\\1")
  return(df)
}

# 2A. Government Debt to GDP
# Note: We map 'I9' (EA19) to 'EA' because it has the long-term historical data (since 1995).
# 'I8' (EA20) is newer and often only has recent data.
df_gov_debt <- read_csv("data/ECB Data Portal_20251124224945.csv", show_col_types = FALSE) %>%
  clean_ecb_names(".*\\.N\\.([A-Z0-9]{2})\\.W0.*") %>%
  select(-`TIME PERIOD`) %>%
  pivot_longer(cols = -DATE, names_to = "country", values_to = "val") %>%
  mutate(year = year(as.Date(DATE)), gov_debt_gdp = val / 100) %>%
  mutate(country = recode(country, "GR" = "EL", "I9" = "EA")) %>% 
  select(country, year, gov_debt_gdp)

# 2B. Resident Holdings of Govt Debt
df_resident_debt <- read_csv("data/ECB Data Portal_20251125213045.csv", show_col_types = FALSE) %>%
  clean_ecb_names(".*GFS\\.A\\.N\\.([A-Z0-9]{2})\\..*") %>%
  select(-`TIME PERIOD`) %>%
  pivot_longer(cols = -DATE, names_to = "country", values_to = "val") %>%
  mutate(year = year(as.Date(DATE)), hh_held_gov_debt_gdp = val / 100) %>%
  mutate(country = recode(country, "GR" = "EL", "I9" = "EA")) %>%
  select(country, year, hh_held_gov_debt_gdp)

# 2C. Household Debt (Loans) to GDP
# Note: Filter for Q4 (Dec 31) to get the annual snapshot from quarterly data
df_hh_debt <- read_csv("data/ECB Data Portal_20251125214629.csv", show_col_types = FALSE) %>%
  clean_ecb_names(".*QSA\\.Q\\.N\\.([A-Z0-9]{2})\\..*") %>%
  filter(grepl("-12-31", DATE)) %>%
  select(-`TIME PERIOD`) %>%
  pivot_longer(cols = -DATE, names_to = "country", values_to = "val") %>%
  mutate(year = year(as.Date(DATE)), hh_debt_gdp = val / 100) %>%
  mutate(country = recode(country, "GR" = "EL", "I9" = "EA")) %>%
  select(country, year, hh_debt_gdp)

# 2D. Merge Annual Metrics
df_annual_macro <- df_gov_debt %>%
  full_join(df_resident_debt, by = c("country", "year")) %>%
  full_join(df_hh_debt, by = c("country", "year")) %>%
  mutate(hh_debt_to_gov_debt = hh_debt_gdp / gov_debt_gdp) %>%
  select(country, year, gov_debt_gdp, hh_held_gov_debt_gdp, hh_debt_to_gov_debt)


# ==============================================================================
# STEP 3: Process Sovereign Yields (Eurostat) - The "Spine"
# ==============================================================================
# Goal: Create the base Country-Month panel.

# Import TSV, reading all columns as character to handle flags like "3.5 b"
raw_yields <- read_tsv("data/estat_irt_lt_mcby_m.tsv", col_types = cols(.default = "c"))
colnames(raw_yields)[1] <- "ind"

df_yields_monthly <- raw_yields %>%
  # Split the index column (e.g., "M,MCBY,AT") to extract country code
  separate(ind, into = c("freq", "int_rt", "geo"), sep = ",") %>%
  # Pivot wide dates to long format
  pivot_longer(cols = -c(freq, int_rt, geo), names_to = "date_str", values_to = "val_raw") %>%
  mutate(
    date = ym(str_trim(date_str)),
    # Extract numeric value, ignoring flags
    sovereign_yield = as.numeric(str_extract(val_raw, "-?[0-9.]+"))
  ) %>%
  filter(!is.na(sovereign_yield)) %>%
  select(country = geo, date, sovereign_yield)


# ==============================================================================
# STEP 4: Final Merge and Winsorize
# ==============================================================================
# Goal: Merge MPS (by date) and Annual Macro (by country-year) into Yields.

# Define Winsorization Function (caps outliers at 1st/99th percentile)
winsorize_x <- function(x, cut = 0.01) {
  lower <- quantile(x, probs = cut, na.rm = TRUE)
  upper <- quantile(x, probs = 1 - cut, na.rm = TRUE)
  pmin(pmax(x, lower), upper)
}

final_data <- df_yields_monthly %>%
  mutate(year = year(date)) %>%
  # Merge 1: Add Monetary Policy Shocks (Left Join on Date)
  left_join(df_mps_monthly, by = c("date" = "month_date")) %>%
  # Merge 2: Add Annual Macro Data (Left Join on Country + Year)
  left_join(df_annual_macro, by = c("country", "year")) %>%
  # Winsorize key variables to handle outliers
  mutate(
    mps_shock_ois2y      = winsorize_x(mps_shock_ois2y),
    sovereign_yield      = winsorize_x(sovereign_yield),
    gov_debt_gdp         = winsorize_x(gov_debt_gdp),
    hh_held_gov_debt_gdp = winsorize_x(hh_held_gov_debt_gdp),
    hh_debt_to_gov_debt  = winsorize_x(hh_debt_to_gov_debt)
  ) %>%
  arrange(country, date) %>%
  select(country, date, year, sovereign_yield, mps_shock_ois2y, 
         gov_debt_gdp, hh_held_gov_debt_gdp, hh_debt_to_gov_debt)


# ==============================================================================
# STEP 5: Save Final Dataset
# ==============================================================================
write.csv(final_data, "data/final_data.csv", row.names = FALSE)