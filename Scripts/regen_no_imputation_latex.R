# Standalone regeneration of the "no imputation" SI stargazer LaTeX table.
# Reproduces exactly the model fits + stargazer call from Analysis.R (lines ~1741-1903).
# Loads pre-imputation data, applies the same transforms, refits M1-M4 on Wave-7-only data.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(stargazer)
})

# Helper used in Analysis.R: coerce a factor 0/1 outcome to numeric 0/1
bin_to_num <- function(x) as.numeric(as.character(x))

# --- Load the pre-imputation data ---
load(here("Data", "Processed", "panel_vars.Rdata"))
panel_noimp <- panel_vars
rm(panel_vars)

# --- Apply the same transformations as in the main analysis ---
panel_noimp <- panel_noimp %>%
  mutate(edu_5 = fct_relevel(edu_5,
                             "Less than high school", "High school",
                             "Some college", "College",
                             "Graduate or prof. degree"),
         income_6 = fct_relevel(income_6,
                                "Less than $20,000", "$20,000-$40,000",
                                "$40,000-$60,000", "$60,000-$80,000",
                                "$80,000-$100,000", "$100,000 and over"),
         inc_heat_perceived_6 = fct_relevel(inc_heat_perceived_6,
                                            "$0 per month", "$1-$24 per month",
                                            "$25-$49 per month", "$50-$99 per month",
                                            "$100 or more per month", "I don't know"),
         inc_gas_perceived_6 = fct_relevel(inc_gas_perceived_6,
                                           "$0 per month", "$1-$24 per month",
                                           "$25-$49 per month", "$50-$99 per month",
                                           "$100 or more per month", "I don't know"))

panel_noimp <- panel_noimp %>%
  mutate(inc_heat_perceived_4 = case_when(
           inc_heat_perceived_6 == "$0 per month" | inc_heat_perceived_6 == "I don't know" ~ "$0 per month",
           inc_heat_perceived_6 == "$1-$24 per month" | inc_heat_perceived_6 == "$25-$49 per month" ~ "$1-$50 per month",
           inc_heat_perceived_6 == "$50-$99 per month" ~ "$50-$99 per month",
           inc_heat_perceived_6 == "$100 or more per month" ~ "$100 or more per month"),
         inc_gas_perceived_4 = case_when(
           inc_gas_perceived_6 == "$0 per month" | inc_gas_perceived_6 == "I don't know" ~ "$0 per month",
           inc_gas_perceived_6 == "$1-$24 per month" | inc_gas_perceived_6 == "$25-$49 per month" ~ "$1-$50 per month",
           inc_gas_perceived_6 == "$50-$99 per month" ~ "$50-$99 per month",
           inc_gas_perceived_6 == "$100 or more per month" ~ "$100 or more per month")) %>%
  mutate(inc_heat_perceived_4 = as.factor(inc_heat_perceived_4),
         inc_gas_perceived_4 = as.factor(inc_gas_perceived_4))

panel_noimp <- panel_noimp %>%
  mutate(inc_heat_perceived_4 = fct_relevel(inc_heat_perceived_4,
                                            "$0 per month", "$1-$50 per month",
                                            "$50-$99 per month", "$100 or more per month"),
         inc_gas_perceived_4 = fct_relevel(inc_gas_perceived_4,
                                           "$0 per month", "$1-$50 per month",
                                           "$50-$99 per month", "$100 or more per month"))

# --- Filter to Wave 7 respondents only -- NO imputation ---
wave7IDs_noimp <- panel_noimp %>% filter(wave == "wave7") %>% pull(responseid)
sample_noimp <- panel_noimp %>%
  filter(responseid %in% wave7IDs_noimp) %>%
  filter(wave == "wave7")

# M1: Baseline
fit0a_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative, data = sample_noimp)
# M2: Perceived costs
fit1c_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative +
                    inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num +
                    gasprice_change_perceived_num, data = sample_noimp)
# M3: Actual costs with interactions
fit3a_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative +
                    owner + home_size_num +
                    fossil_home + fossil_water + fossil_stove +
                    home_size_num * fossil_home +
                    bill_elec_num + bill_diesel_num +
                    drive + vehicle_num + km_driven_num +
                    drive * km_driven_num, data = sample_noimp)
# M4: Full model
fit4a_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative +
                    inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num +
                    gasprice_change_perceived_num +
                    owner + home_size_num +
                    fossil_home + fossil_water + fossil_stove +
                    home_size_num * fossil_home +
                    bill_elec_num + bill_diesel_num +
                    drive + vehicle_num + km_driven_num +
                    drive * km_driven_num, data = sample_noimp)

# --- Output LaTeX table for SI (identical call to Analysis.R) ---
stargazer(fit0a_noimp, fit1c_noimp, fit3a_noimp, fit4a_noimp,
          type = "latex", style = "ajps",
          title = "Determinants of opposition to carbon pricing (no imputation)",
          dep.var.labels = c("Opposition to carbon pricing"),
          covariate.labels = c("Education: High school", "Education: Some college", "Education: College", "Education: Graduate",
                               "Income: 20,000-40,000", "Income: 40,000-60,000", "Income: 60,000-80,000", "Income: 80,000-100,000", "Income: 100,000 and over",
                               "Rural (dummy)",
                               "Left-right: 0-1 (1 is far right)",
                               "Conservative (dummy)",
                               "Perceived inc. heating: 1-50 per month", "Perceived inc. heating: 50-99 per month", "Perceived inc. heating: 100 or more per month",
                               "Perceived inc. gas: 1-50 per month", "Perceived inc. gas: 50-99 per month", "Perceived inc. gas: 100 or more per month",
                               "Perceived increase in overall costs (due to CP)",
                               "Perceived increase in gas prices (cents/liter)",
                               "Home owner (dummy)",
                               "Home size (square ft.)",
                               "Home heating is fossil fuels (dummy)",
                               "Water heating is fossil fuels (dummy)",
                               "Fossil fuel stove (dummy)",
                               "Monthly electricity bill",
                               "Monthly gasoline/diesel bill",
                               "Drives to work (dummy)",
                               "Number of vehicles owned",
                               "Yearly kilometers driven",
                               "Home size * fossil home",
                               "Drives to work * Kilometers driven"),
          single.row = TRUE,
          se = NULL,
          keep.stat = c("n", "adj.rsq"),
          label = "table:noimputation",
          out = here("Results", "pricing_no_imputation.tex"))
