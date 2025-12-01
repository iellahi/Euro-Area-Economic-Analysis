# ==============================================================================
# Summary Statistics and Data Visualization
# ==============================================================================
# Purpose: Generate summary stats (winsorized) and time-series plots.
# Input:   data/final_data.csv
# Output:  output/summary_statistics.tex, output/*.png plots

# 1. Load Libraries (using pacman for efficiency)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tidyr, knitr, ggplot2, lubridate)

# Ensure output directory exists
if (!dir.exists("output")) {
  dir.create("output")
}

# 2. Load Data
final_data <- read_csv("data/final_data.csv", show_col_types = FALSE)

# ==============================================================================
# PART A: Summary Statistics (Winsorized)
# ==============================================================================

# Helper function: Winsorize at 1st and 99th percentile
winsorize_x <- function(x, cut = 0.01) {
  lower <- quantile(x, probs = cut, na.rm = TRUE)
  upper <- quantile(x, probs = 1 - cut, na.rm = TRUE)
  pmin(pmax(x, lower), upper)
}

# Apply winsorization to key numeric variables
final_data_clean <- final_data %>%
  mutate(
    mps_shock_ois2y      = winsorize_x(mps_shock_ois2y),
    sovereign_yield      = winsorize_x(sovereign_yield),
    gov_debt_gdp         = winsorize_x(gov_debt_gdp),
    hh_held_gov_debt_gdp = winsorize_x(hh_held_gov_debt_gdp),
    hh_debt_to_gov_debt  = winsorize_x(hh_debt_to_gov_debt)
  )

# Calculate summary statistics
summary_stats <- final_data_clean %>%
  select(
    mps_shock_ois2y, 
    sovereign_yield, 
    gov_debt_gdp, 
    hh_held_gov_debt_gdp, 
    hh_debt_to_gov_debt
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean         = mean(Value, na.rm = TRUE),
    Median       = median(Value, na.rm = TRUE),
    SD           = sd(Value, na.rm = TRUE),
    IQR          = IQR(Value, na.rm = TRUE),
    Observations = sum(!is.na(Value))
  ) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3)))

# Save table to LaTeX
latex_table <- kable(
  summary_stats, 
  format = "latex", 
  booktabs = TRUE, 
  caption = "Summary Statistics across Country-Months (Winsorized)", 
  label = "tab:summary_stats"
)
cat(latex_table, file = "output/summary_statistics.tex")
message("Table saved: output/summary_statistics.tex")


# ==============================================================================
# PART B: Data Visualization
# ==============================================================================

# Filter for key regions: Germany (DE), Greece (EL), Euro Area (EA)
plot_data <- final_data %>%
  filter(country %in% c("DE", "EL", "EA")) %>%
  mutate(country_name = case_when(
    country == "DE" ~ "Germany",
    country == "EL" ~ "Greece",
    country == "EA" ~ "Euro Area",
    TRUE ~ country
  ))

# Define a shared theme for consistency
custom_theme <- theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# --- Plot 1: Nominal Sovereign Yields ---
p1 <- ggplot(plot_data, aes(x = date, y = sovereign_yield, color = country_name)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Nominal Sovereign Yields Over Time",
    subtitle = "Comparing Germany, Greece, and the Euro Area",
    x = "Date",
    y = "Yield (%)",
    color = "Region"
  ) +
  custom_theme

ggsave("output/yields_time_series.png", plot = p1, width = 10, height = 6)
print(p1)

# --- Plot 2: Government Debt to GDP ---
p2 <- ggplot(plot_data, aes(x = date, y = gov_debt_gdp * 100, color = country_name)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Government Debt to GDP Over Time",
    subtitle = "Comparing Germany, Greece, and the Euro Area",
    x = "Date",
    y = "Debt to GDP (%)",
    color = "Region"
  ) +
  custom_theme

ggsave("output/debt_gdp_time_series.png", plot = p2, width = 10, height = 6)
print(p2)

message("Plots saved to 'output/' folder.")