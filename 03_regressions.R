# ==============================================================================
# Euro Area Economic Analysis
# ==============================================================================
# Purpose: Perform regression analysis on MPS, Yields, and Debt.
# Tasks:   1. Time-series regression (DE & ES)
#          2. Panel regression (All Countries)
#          3. Cross-sectional Beta plots
#          4. Interaction Regressions (Continuous)
#          5. Interaction Regressions (Dummy)
# Inputs:  data/final_data.csv
# Outputs: output/*.tex (Tables), output/*.png (Plots)

# 1. Load Libraries (using pacman for efficiency)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tidyr, broom, kableExtra, knitr, 
               fixest, ggplot2, purrr, ggrepel, lubridate)

# Ensure output directory exists
if (!dir.exists("output")) dir.create("output")

# 2. Load Data
#    (Assumes data/final_data.csv was created/winsorized in the previous step)
final_data <- read_csv("data/final_data.csv", show_col_types = FALSE)

# ------------------------------------------------------------------------------
# PART A: Task 1 - Time-Series Regression (Germany & Spain)
# ------------------------------------------------------------------------------
# Specification: Y_{c,t+1} = beta * MPS_t + gamma * X_{c,t} + epsilon

# Prepare Data
reg_data_task1 <- final_data %>%
  filter(country %in% c("DE", "ES")) %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    lead_yield = lead(sovereign_yield), # Y_{t+1}
    debt_gdp   = gov_debt_gdp           # Control
  ) %>%
  filter(!is.na(lead_yield), !is.na(mps_shock_ois2y), !is.na(debt_gdp)) %>%
  ungroup()

# Run Regressions
models_task1 <- reg_data_task1 %>%
  group_by(country) %>%
  nest() %>%
  mutate(
    model  = purrr::map(data, ~ lm(lead_yield ~ mps_shock_ois2y + debt_gdp, data = .x)),
    tidied = purrr::map(model, tidy)
  ) %>%
  unnest(tidied) %>%
  select(country, term, estimate, std.error, statistic, p.value) %>%
  mutate(
    term = recode(term, 
                  "(Intercept)" = "Intercept",
                  "mps_shock_ois2y" = "MPS Shock (OIS 2Y)",
                  "debt_gdp" = "Govt Debt/GDP")
  )

# Save Table
latex_table1 <- kable(
  models_task1,
  format = "latex", booktabs = TRUE, digits = 3,
  caption = "Time-Series Regression of Sovereign Yields on MPS and Debt",
  col.names = c("Country", "Variable", "Estimate", "Std. Error", "t-stat", "p-value"),
  label = "tab:task1_regression"
) %>%
  collapse_rows(columns = 1, valign = "top") %>%
  footnote(
    general = c(
      "Specification: $Y_{c,t+1} = \\beta MPS_t + \\gamma X_{c,t} + \\epsilon_{c,t}$.",
      "Sample: Monthly obs for Germany (DE) and Spain (ES).",
      "Data: Winsorized at 1st/99th percentiles."
    ),
    general_title = "Notes: ", threeparttable = TRUE, escape = FALSE
  )

save_kable(latex_table1, file = "output/regression_task1.tex")
message("Task 1 Table saved.")


# ------------------------------------------------------------------------------
# PART B: Task 2 - Panel Regression
# ------------------------------------------------------------------------------
# Specification: Y_{c,t+1} = beta * MPS_t + alpha_c + alpha_t + epsilon

# Prepare Data
reg_data_panel <- final_data %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    lead_yield = lead(sovereign_yield),
    cal_month  = month(date) # Seasonality FE
  ) %>%
  filter(!is.na(lead_yield), !is.na(mps_shock_ois2y)) %>%
  ungroup()

# Run Model
model_task2 <- feols(lead_yield ~ mps_shock_ois2y | country + cal_month, 
                     data = reg_data_panel, cluster = "country")

# Save Table
model_stats2 <- tidy(model_task2) %>%
  mutate(term = "MPS Shock (OIS 2Y)") %>%
  select(term, estimate, std.error, statistic, p.value)

latex_table2 <- kable(
  model_stats2,
  format = "latex", booktabs = TRUE, digits = 3,
  caption = "Panel Regression of Sovereign Yields on Monetary Policy Shocks",
  col.names = c("Variable", "Estimate", "Std. Error", "t-stat", "p-value"),
  label = "tab:task2_panel"
) %>%
  footnote(
    general = c(
      "Specification: $Y_{c,t+1} = \\beta MPS_t + \\alpha_c + \\alpha_t + \\epsilon_{c,t}$.",
      "FEs: Country ($\\alpha_c$) and Calendar Month ($\\alpha_t$).",
      paste0("Sample: ", nobs(model_task2), " observations. Clustered SEs.")
    ),
    general_title = "Notes: ", threeparttable = TRUE, escape = FALSE
  )

save_kable(latex_table2, file = "output/regression_task2.tex")
message("Task 2 Table saved.")


# ------------------------------------------------------------------------------
# PART C: Task 3 - Beta Estimates & Plots
# ------------------------------------------------------------------------------
# Goal: Estimate beta_c for all countries and plot against 2015 debt levels.

# 1. Estimate Betas
beta_estimates <- final_data %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(lead_yield = lead(sovereign_yield)) %>%
  filter(!is.na(lead_yield), !is.na(mps_shock_ois2y), !is.na(gov_debt_gdp), n() > 24) %>% 
  nest() %>%
  mutate(
    model = map(data, ~ lm(lead_yield ~ mps_shock_ois2y + gov_debt_gdp, data = .x)),
    tidied = map(model, tidy)
  ) %>%
  unnest(tidied) %>%
  filter(term == "mps_shock_ois2y") %>%
  select(country, beta = estimate, std.error) %>%
  mutate(
    ci_lower = beta - 1.96 * std.error,
    ci_upper = beta + 1.96 * std.error
  )

# 2. Get 2015 Sorting Variables
sorting_data_2015 <- final_data %>%
  filter(year == 2015) %>%
  group_by(country) %>%
  summarise(
    debt_gdp_2015 = mean(gov_debt_gdp, na.rm = TRUE),
    hh_ratio_2015 = mean(hh_debt_to_gov_debt, na.rm = TRUE)
  )

# 3. Merge and Plot
plot_data_task3 <- inner_join(beta_estimates, sorting_data_2015, by = "country")

plot_theme <- theme_minimal() +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(color = "gray40"))

# Plot A: Govt Debt
p_debt <- ggplot(plot_data_task3, aes(x = debt_gdp_2015, y = beta)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.02, color = "gray60") +
  geom_point(size = 3, color = "dodgerblue4") +
  geom_text_repel(aes(label = country)) +
  labs(title = "MPS Sensitivity vs. Govt Debt (2015)", x = "Govt Debt/GDP (2015)", y = "Beta") +
  scale_x_continuous(labels = scales::percent) + plot_theme

# Plot B: HH/Govt Ratio
p_hh <- ggplot(plot_data_task3, aes(x = hh_ratio_2015, y = beta)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.05, color = "gray60") +
  geom_point(size = 3, color = "firebrick") +
  geom_text_repel(aes(label = country)) +
  labs(title = "MPS Sensitivity vs. HH/Govt Debt Ratio (2015)", x = "HH/Govt Debt Ratio", y = "Beta") +
  plot_theme

ggsave("output/beta_vs_debt_gdp.png", plot = p_debt, width = 8, height = 6)
ggsave("output/beta_vs_hh_ratio.png", plot = p_hh, width = 8, height = 6)
message("Task 3 Plots saved.")


# ------------------------------------------------------------------------------
# PART D: Task 4 - Interaction Regressions (Continuous)
# ------------------------------------------------------------------------------
# Equation: Y = b1(MPS*X) + b2(MPS) + b3(X) + FEs

m1 <- feols(lead_yield ~ mps_shock_ois2y * gov_debt_gdp | country + cal_month, 
            data = reg_data_panel, cluster = "country")
m2 <- feols(lead_yield ~ mps_shock_ois2y * hh_held_gov_debt_gdp | country + cal_month, 
            data = reg_data_panel, cluster = "country")
m3 <- feols(lead_yield ~ mps_shock_ois2y * hh_debt_to_gov_debt | country + cal_month, 
            data = reg_data_panel, cluster = "country")

var_dict4 <- c("mps_shock_ois2y" = "MPS", "gov_debt_gdp" = "Gov Debt", 
               "hh_held_gov_debt_gdp" = "HH-Held", "hh_debt_to_gov_debt" = "HH Ratio")

latex_table4 <- etable(m1, m2, m3, tex = TRUE, dict = var_dict4, digits = 3,
                       title = "Interaction Regressions (Continuous Debt Metrics)",
                       label = "tab:task4", fitstat = c("n", "ar2"))

sink("output/regression_task4.tex"); cat(latex_table4); sink()
message("Task 4 Table saved.")


# ------------------------------------------------------------------------------
# PART E: Task 5 - Interaction Regressions (Dummy)
# ------------------------------------------------------------------------------
# Dummy = 1 if X_{ct} > Median(X_{ct})

reg_data_dummy <- reg_data_panel %>%
  group_by(date) %>%
  mutate(
    d_gov_debt = ifelse(gov_debt_gdp > median(gov_debt_gdp, na.rm=TRUE), 1, 0),
    d_hh_held  = ifelse(hh_held_gov_debt_gdp > median(hh_held_gov_debt_gdp, na.rm=TRUE), 1, 0),
    d_hh_ratio = ifelse(hh_debt_to_gov_debt > median(hh_debt_to_gov_debt, na.rm=TRUE), 1, 0)
  ) %>% ungroup()

m4 <- feols(lead_yield ~ mps_shock_ois2y * d_gov_debt | country + cal_month, 
            data = reg_data_dummy, cluster = "country")
m5 <- feols(lead_yield ~ mps_shock_ois2y * d_hh_held | country + cal_month, 
            data = reg_data_dummy, cluster = "country")
m6 <- feols(lead_yield ~ mps_shock_ois2y * d_hh_ratio | country + cal_month, 
            data = reg_data_dummy, cluster = "country")

var_dict5 <- c("d_gov_debt" = "High Debt (D)", "d_hh_held" = "High HH-Held (D)", 
               "d_hh_ratio" = "High Ratio (D)", "mps_shock_ois2y" = "MPS")

latex_table5 <- etable(m4, m5, m6, tex = TRUE, dict = var_dict5, digits = 3,
                       title = "Interaction Regressions (High-Debt Dummies)",
                       label = "tab:task5", fitstat = c("n", "ar2"))

sink("output/regression_task5.tex"); cat(latex_table5); sink()
message("Task 5 Table saved.")
message("All Tasks Complete.")