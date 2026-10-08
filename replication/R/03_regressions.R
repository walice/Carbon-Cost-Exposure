# 03_regressions.R
# Step 3: linear probability models of carbon-pricing opposition (Table 1 and
# SI tables), logit comparison, models of perceived costs, nested-model F-tests.
# Input : data/derived/panel_imputed.rds
# Output: output/tables/Table1_full_model.tex, pricing_*.txt,
#         SI_baseline_support_vs_oppose.tex, SI_logit_AME.csv,
#         SI_cost_perceptions.tex, perceived_*.txt; F-test output is in the run log.
#         Fitted models are saved to data/derived/models.rds for 05_robustness.R.

source(here::here("R", "00_setup.R"))

# REGRESSIONS               ####
## ## ## ## ## ## ## ## ## ## ##

panel <- readRDS(here("data", "derived", "panel_imputed.rds"))

wave7IDs <- panel %>% filter(wave == "wave7") %>% pull(responseid)

sample <- panel %>% 
  filter(responseid %in% wave7IDs)


# .. Baseline model of support for carbon pricing ####
fit0a <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative, 
            data = sample)
summary(fit0a)
nobs(fit0a)

fit0b <- lm(bin_to_num(cp_support) ~ edu_5 + income_6 + rural +
              left_right_num + liberal, 
            data = sample)
summary(fit0b)
nobs(fit0b)

stargazer(fit0a, fit0b, type = "text",
          out = here("output", "tables", "pricing_base.txt"))

fit0a$AIC <- AIC(fit0a)
fit0b$AIC <- AIC(fit0b)
attr(fit0a$AIC, "names") <- "Aikake Inf. Crit."
attr(fit0b$AIC, "names") <- "Aikake Inf. Crit."

stargazer(fit0a, 
          fit0b, 
          type = "latex", style = "ajps",
          title = "Determinants of support for or opposition to carbon pricing",
          dep.var.labels = c("Oppose", "Support"),
          covariate.labels = c("Education: High school", "Education: Some college", "Education: College", "Education: Graduate or prof. degree",
                               "Income: 20,000-40,000", "Income: 40,000-60,000", "Income: 60,000-80,000", "Income: 80,000-100,000", "Income: 100,000 and over",
                               "Rural (dummy)",
                               "Left-right: 0-1 (1 is far right)",
                               "Conservative (dummy)",
                               "Liberal (dummy)"),
          model.numbers = FALSE,
          keep.stat = c("n", "adj.rsq", "f", "AIC"),
          out = here("output", "tables", "SI_baseline_support_vs_oppose.tex"))  # SI Appendix C table
# Choose fit0a - work with cp_oppose


# .. As a robustness check, fit base model with logit instead

fit0a_logit <- glm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                     left_right_num + conservative, 
                   data = sample,
                   family = binomial(link = "logit"))

# View results
summary(fit0a_logit)
nobs(fit0a_logit)

# Get AMEs to compare against LPM coefficients
library(margins)

# Get average marginal effects (AME) - comparable to LPM coefficients
ame_logit <- summary(margins(fit0a_logit))
print(ame_logit)
write.csv(ame_logit, here("output", "tables", "SI_logit_AME.csv"), row.names = FALSE)  # SI Appendix D, logit column

# .. Regress support for carbon pricing on perceived costs ####
fit1a <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural + 
              left_right_num + conservative +
              # familiar_bills_3 +
              inc_heat_perceived_num + inc_gas_perceived_num + inc_overall_perceived_num + 
              gasprice_change_perceived_num,
            data = sample)
summary(fit1a)
nobs(fit1a)
VIF(fit1a)

fit1b <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              # familiar_bills_3 +
              inc_heat_perceived_6 + inc_gas_perceived_6 + inc_overall_perceived_num + 
              gasprice_change_perceived_num,
            data = sample)
summary(fit1b)
nobs(fit1b)
VIF(fit1b)

fit1c <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              # familiar_bills_3 +
              inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num + 
              gasprice_change_perceived_num,
            data = sample)
summary(fit1c)
nobs(fit1c)
VIF(fit1c)

stargazer(fit1a, fit1b, fit1c,
          type = "text",
          out = here("output", "tables", "pricing_perceived.txt"))
AIC(fit1b,
    fit1c)
BIC(fit1b,
    fit1c)
# Choose model where inc_heat_perceived_* are inc_gas_perceived_*
# are factors instead of numeric.
# Choose model where factor are collapsed from 6 to 4.
# Choose fit1c


# .. Regress support for carbon pricing on actual costs ####
fit2a <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              owner + home_size_num +
              fossil_home + fossil_water + fossil_stove,
            data = sample)
summary(fit2a)
nobs(fit2a)
VIF(fit2a)

fit2b <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              owner + home_size_num +
              fossil_home + fossil_water + fossil_stove +
              drive + vehicle_num + km_driven_num,
            data = sample)
summary(fit2b)
nobs(fit2b)

fit2c <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              # owner + home_size_num +
              # fossil_home + fossil_water + fossil_stove +
              drive + vehicle_num + km_driven_num +
              bill_elec_num + bill_diesel_num,
            data = sample)
summary(fit2c)
nobs(fit2c)

fit2d <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              owner + home_size_num +
              fossil_home + fossil_water + fossil_stove +
              drive + vehicle_num + km_driven_num +
              bill_elec_num + bill_diesel_num,
           data = sample)
summary(fit2d)
nobs(fit2d)

stargazer(fit2a, fit2b, fit2c, fit2d, 
          type = "text",
          out = here("output", "tables", "pricing_actual.txt"))
# Choose fit2d but report all models in appendix


# .. Add interaction terms for actual costs ####
fit3a <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              owner + home_size_num +
              fossil_home + fossil_water + fossil_stove +
              home_size_num * fossil_home +
              bill_elec_num + bill_diesel_num +
              drive + vehicle_num + km_driven_num +
              drive * km_driven_num,
            data = sample)
summary(fit3a)
nobs(fit3a)

stargazer(fit2d, fit3a, 
          type = "text",
          out = here("output", "tables", "pricing_actual_interactions.txt"))
AIC(fit2d,
    fit3a)
# fit3a performs marginally better
# But only interaction between drive and km_driven_num is significant


# .. Regress carbon pricing support on a model of perceived and actual costs ####
# Include perceived and actual costs in the model
fit4a <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num +
              gasprice_change_perceived_num +
              owner + home_size_num +
              fossil_home + fossil_water + fossil_stove +
              home_size_num * fossil_home +
              bill_elec_num + bill_diesel_num +
              drive + vehicle_num + km_driven_num +
              drive * km_driven_num,
            data = sample)
summary(fit4a)
nobs(fit4a)

stargazer(fit0a,
          fit1c,
          fit3a,
          fit4a,
          type = "text",
          no.space = TRUE,
          out = here("output", "tables", "pricing_full_model.txt"))
# fit4a performs better

stargazer(fit0a,
          fit1c,
          fit3a,
          fit4a,
          type = "latex", style = "ajps",
          title = "Determinants of opposition to carbon pricing as a function of costs",
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
          # model.numbers = FALSE,
          single.row = TRUE,
          se = NULL,
          keep.stat = c("n", "adj.rsq"),
          out = here("output", "tables", "Table1_full_model.tex"))  # main text Table 1


# .. Compare nested models ####
# Variables used in the full model
vars <- c("cp_oppose",
          "edu_5",
          "income_6",
          "rural",
          "left_right_num",
          "conservative",
          "inc_heat_perceived_4",
          "inc_gas_perceived_4",
          "inc_overall_perceived_num",
          "gasprice_change_perceived_num",
          "owner",
          "home_size_num",
          "fossil_home",
          "fossil_water",
          "fossil_stove",
          "bill_elec_num",
          "bill_diesel_num",
          "drive",
          "vehicle_num",
          "km_driven_num")

# Subset data to obtain complete cases for the variables in the full model
sample_complete <- sample %>%
  select(all_of(vars)) %>%
  drop_na

# Perform F-test to compare nested models
fit0a_complete <- update(fit0a, data = sample_complete)
anova(fit0a_complete, fit4a)
# Reject the null hypothesis that the full model fit4a can be reduced 
# to the baseline model fit0a_complete

fit1c_complete <- update(fit1c, data = sample_complete)
anova(fit1c_complete, fit4a)
# Fail to reject the null hypothesis that the full model fit4a can be reduced 
# to the perceived costs model fit1c_complete

fit2d_complete <- update(fit2d, data = sample_complete)
anova(fit2d_complete, fit4a)
# Reject the null hypothesis that the full model fit4a can be reduced 
# to the actual costs model fit2d_complete

fit3a_complete <- update(fit3a, data = sample_complete)
anova(fit3a_complete, fit4a)
# Reject the null hypothesis that the full model fit4a can be reduced 
# to the actual costs model with interactions fit3a_complete





# PERCEPTIONS OF COSTS      ####
## ## ## ## ## ## ## ## ## ## ##

sample <- panel %>% 
  filter(responseid %in% wave7IDs)


# .. Model perceptions of carbon pricing on gasoline costs ####
fit5a <- lm(inc_gas_perceived_num ~ conservative + 
              income_num_mid +
              rural +
              familiar_bills_3 +
              vehicle_num + drive + km_driven_num +
              gasprice_change_perceived_num, 
            data = sample)
summary(fit5a)
nobs(fit5a)

fit5b <- lm(inc_gas_perceived_num ~ conservative + 
              income_num_mid +
              rural +
              familiar_bills_3 +
              vehicle_num + drive + km_driven_num +
              bill_diesel_num +
              gasprice_change_perceived_num, 
            data = sample)
summary(fit5b)
nobs(fit5b)

# With interactions
fit5c <- lm(inc_gas_perceived_num ~ conservative + 
              rural +
              income_num_mid +
              familiar_bills_3 +
              vehicle_num + drive + km_driven_num +
              drive * km_driven_num +
              bill_diesel_num +
              gasprice_change_perceived_num, 
            data = sample)
summary(fit5c)
nobs(fit5c)

fit5d <- lm(inc_gas_perceived_num ~ conservative + 
              rural +
              income_num_mid +
              familiar_bills_3 +
              vehicle_num + drive + km_driven_num +
              bill_diesel_num +
              conservative * bill_diesel_num +
              gasprice_change_perceived_num, 
            data = sample)
summary(fit5d)
nobs(fit5d)

stargazer(fit5a, fit5b, fit5c, fit5d, 
          type = "text",
          out = here("output", "tables", "perceived_gas_costs.txt"))
AIC(fit5a, fit5b, fit5c, fit5d)
# fit5d is marginally better

sample_complete <- sample %>%
  select(c(inc_gas_perceived_num,
           conservative,
           rural,
           income_num_mid,
           familiar_bills_3,
           vehicle_num,
           drive,
           km_driven_num,
           bill_diesel_num,
           gasprice_change_perceived_num)) %>%
  drop_na

# Perform F-test to compare nested models
fit5a_complete <- update(fit5a, data = sample_complete)
anova(fit5a_complete, fit5d)
# Reject the null hypothesis that the model can be reduced to fit5a


# .. Model perceptions of carbon pricing on heating costs ####
fit6a <- lm(inc_heat_perceived_num ~ conservative + 
              income_num_mid +
              familiar_bills_3 +
              owner + home_size_num +
              fossil_home +
              bill_elec_num, 
            data = sample)
summary(fit6a)
nobs(fit6a)

# With interactions
fit6b <- lm(inc_heat_perceived_num ~ conservative + 
              income_num_mid +
              familiar_bills_3 +
              owner + home_size_num +
              fossil_home +
              home_size_num * fossil_home +
              bill_elec_num, 
            data = sample)
summary(fit6b)
nobs(fit6b)

AIC(fit6a, fit6b)
# fit6a is marginally better
anova(fit6a, fit6b)
# Fail to reject the null hypothesis that the full model can be reduced to fit6a

stargazer(fit6a, fit6b, 
          type = "text",
          out = here("output", "tables", "perceived_heating_costs.txt"))


# .. Model perceptions of carbon pricing on overall costs ####
fit7a <- lm(inc_overall_perceived_num ~ conservative +
              income_num_mid +
              familiar_bills_3 +
              vehicle_num + drive + km_driven_num +
              rural +
              bill_diesel_num +
              gasprice_change_perceived_num +
              owner + home_size_num +
              fossil_home +
              bill_elec_num, 
            data = sample)
summary(fit7a)
nobs(fit7a)

# With interactions
fit7b <- lm(inc_overall_perceived_num ~ conservative +
              income_num_mid +
              familiar_bills_3 +
              vehicle_num + drive + km_driven_num +
              drive * km_driven_num +
              rural +
              bill_diesel_num +
              gasprice_change_perceived_num +
              owner + home_size_num +
              fossil_home +
              home_size_num * fossil_home +
              bill_elec_num, 
            data = sample)
summary(fit7b)
nobs(fit7b)

AIC(fit7a, fit7b)
# fit7a is marginally better
anova(fit7a, fit7b)
# Fail to reject the null hypothesis that the model can be reduced to fit6a

stargazer(fit5d, fit6a, fit7a,
          type = "text",
          out = here("output", "tables", "perceived_overall_costs.txt"))

stargazer(fit5d,
          fit6a,
          fit7a,
          type = "latex", style = "ajps",
          title = "Determinants of the perceptions of the costs of carbon pricing",
          dep.var.labels = c("Gasoline costs", "Heating costs", "Overall costs"),
          covariate.labels = c("Conservative (dummy)",
                               "Rural (dummy)",
                               "Household income",
                               "Household bills: Somewhat familiar", "Household bills: Very familiar",
                               "Number of vehicles owned",
                               "Drives to work (dummy)",
                               "Yearly kilometers driven",
                               "Monthly gasoline/diesel bill",
                               "Perceived inc. in gas prices (cents/liter)",
                               "Conservative * Monthly diesel bill",
                               "Home owner (dummy)",
                               "Home size (square ft.)",
                               "Home heating is fossil fuels (dummy)",
                               "Monthly electricity bill"),
                               # model.numbers = FALSE,
          # multicolumn = FALSE,
          keep.stat = c("n", "adj.rsq"),
          out = here("output", "tables", "SI_cost_perceptions.tex"))  # SI Appendix B table





# Save the fitted models and the analysis sample for the robustness script
saveRDS(list(fit0a = fit0a, fit1c = fit1c, fit3a = fit3a, fit4a = fit4a,
             sample = sample, wave7IDs = wave7IDs),
        file = here("data", "derived", "models.rds"))
