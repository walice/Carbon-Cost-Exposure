# 05_robustness.R
# Step 5: robustness checks added in revision -- Wave-7-only (no imputation)
# re-estimation, Conservative x perceived-cost interactions, Shapley (LMG)
# variance decomposition, provincial fixed effects, province-clustered SEs,
# and an attrition/complete-case balance table.
# Input : data/panel_vars.rds, data/derived/models.rds (from 03_regressions.R)
# Output: output/tables/pricing_no_imputation.txt, SI_no_imputation.tex,
#         pricing_interactions_SI.txt, SI_conservative_x_perceived_interactions.tex,
#         shapley_decomposition.txt, pricing_province_fe*.txt,
#         pricing_clustered_se.txt, attrition_balance.txt;
#         output/figures/shapley_m1.png, shapley_m2.png

source(here::here("R", "00_setup.R"))
models <- readRDS(here("data", "derived", "models.rds"))
fit0a <- models$fit0a; fit1c <- models$fit1c; fit3a <- models$fit3a; fit4a <- models$fit4a
sample <- models$sample; wave7IDs <- models$wave7IDs
rm(models)
panel <- readRDS(here("data", "derived", "panel_imputed.rds"))

# NOTE ON SAMPLES. Table 1 in the main text pools every wave-row of the 1008
# Wave 7 respondents (so M1 has N = 1666 respondent-wave observations). The SI
# robustness tables (interactions, Shapley decomposition, province fixed
# effects, clustered SEs) were estimated on the Wave 7 rows only, one row per
# respondent (M1: N = 831). M3 and M4 are unaffected because their covariates
# exist only in Wave 7. We therefore refit the four models on the Wave 7 rows
# before running those sections; this reproduces the SI files exactly.
sample <- sample %>% filter(wave == "wave7")
fit0a <- update(fit0a, data = sample)
fit1c <- update(fit1c, data = sample)
fit3a <- update(fit3a, data = sample)
fit4a <- update(fit4a, data = sample)

# ROBUSTNESS: NO IMPUTATION             ####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Re-estimate M1-M4 using only Wave 7 native responses
# (no values carried forward from Wave 1 or Wave 6)
# This addresses Reviewer 1 Point 6

# Load the pre-imputation data
panel_vars <- readRDS(here("data", "panel_vars.rds"))
panel_noimp <- panel_vars
rm(panel_vars)

# Apply the same transformations as in the main analysis
# (releveling factors, creating collapsed 4-level perception categories)
panel_noimp <- panel_noimp %>%
  mutate(edu_5 = fct_relevel(edu_5,
                             "Less than high school",
                             "High school",
                             "Some college",
                             "College",
                             "Graduate or prof. degree"),
         income_6 = fct_relevel(income_6,
                                "Less than $20,000",
                                "$20,000-$40,000",
                                "$40,000-$60,000",
                                "$60,000-$80,000",
                                "$80,000-$100,000",
                                "$100,000 and over"),
         inc_heat_perceived_6 = fct_relevel(inc_heat_perceived_6,
                                            "$0 per month",
                                            "$1-$24 per month",
                                            "$25-$49 per month" ,
                                            "$50-$99 per month",
                                            "$100 or more per month",
                                            "I don't know"),
         inc_gas_perceived_6 = fct_relevel(inc_gas_perceived_6,
                                           "$0 per month",
                                           "$1-$24 per month",
                                           "$25-$49 per month" ,
                                           "$50-$99 per month",
                                           "$100 or more per month",
                                           "I don't know"))

# Create collapsed 4-level perception factors
panel_noimp <- panel_noimp %>%
  mutate(inc_heat_perceived_4 = case_when(inc_heat_perceived_6 == "$0 per month" |
                                            inc_heat_perceived_6 == "I don't know" ~
                                            "$0 per month",
                                          inc_heat_perceived_6 == "$1-$24 per month" |
                                            inc_heat_perceived_6 == "$25-$49 per month" ~
                                            "$1-$50 per month",
                                          inc_heat_perceived_6 == "$50-$99 per month" ~
                                            "$50-$99 per month",
                                          inc_heat_perceived_6 == "$100 or more per month" ~
                                            "$100 or more per month"),
         inc_gas_perceived_4 = case_when(inc_gas_perceived_6 == "$0 per month" |
                                           inc_gas_perceived_6 == "I don't know" ~
                                            "$0 per month",
                                         inc_gas_perceived_6 == "$1-$24 per month" |
                                           inc_gas_perceived_6 == "$25-$49 per month" ~
                                            "$1-$50 per month",
                                         inc_gas_perceived_6 == "$50-$99 per month" ~
                                            "$50-$99 per month",
                                         inc_gas_perceived_6 == "$100 or more per month" ~
                                            "$100 or more per month")) %>%
  mutate(inc_heat_perceived_4 = as.factor(inc_heat_perceived_4),
         inc_gas_perceived_4 = as.factor(inc_gas_perceived_4))
panel_noimp <- panel_noimp %>%
  mutate(inc_heat_perceived_4 = fct_relevel(inc_heat_perceived_4,
                                            "$0 per month",
                                            "$1-$50 per month",
                                            "$50-$99 per month",
                                            "$100 or more per month"),
         inc_gas_perceived_4 = fct_relevel(inc_gas_perceived_4,
                                           "$0 per month",
                                           "$1-$50 per month",
                                           "$50-$99 per month",
                                           "$100 or more per month"))

# Filter to Wave 7 respondents only -- NO imputation
wave7IDs_noimp <- panel_noimp %>% filter(wave == "wave7") %>% pull(responseid)
sample_noimp <- panel_noimp %>%
  filter(responseid %in% wave7IDs_noimp) %>%
  filter(wave == "wave7")

# M1: Baseline model (demographics + partisanship)
fit0a_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative,
                  data = sample_noimp)
summary(fit0a_noimp)
nobs(fit0a_noimp)

# M2: Perceived costs
fit1c_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative +
                    inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num +
                    gasprice_change_perceived_num,
                  data = sample_noimp)
summary(fit1c_noimp)
nobs(fit1c_noimp)

# M3: Actual costs with interactions
fit3a_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative +
                    owner + home_size_num +
                    fossil_home + fossil_water + fossil_stove +
                    home_size_num * fossil_home +
                    bill_elec_num + bill_diesel_num +
                    drive + vehicle_num + km_driven_num +
                    drive * km_driven_num,
                  data = sample_noimp)
summary(fit3a_noimp)
nobs(fit3a_noimp)

# M4: Full model (perceived + actual costs)
fit4a_noimp <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                    left_right_num + conservative +
                    inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num +
                    gasprice_change_perceived_num +
                    owner + home_size_num +
                    fossil_home + fossil_water + fossil_stove +
                    home_size_num * fossil_home +
                    bill_elec_num + bill_diesel_num +
                    drive + vehicle_num + km_driven_num +
                    drive * km_driven_num,
                  data = sample_noimp)
summary(fit4a_noimp)
nobs(fit4a_noimp)

# Output results to text file
stargazer(fit0a_noimp,
          fit1c_noimp,
          fit3a_noimp,
          fit4a_noimp,
          type = "text",
          no.space = TRUE,
          out = here("output", "tables", "pricing_no_imputation.txt"))

# Output LaTeX table for SI
stargazer(fit0a_noimp,
          fit1c_noimp,
          fit3a_noimp,
          fit4a_noimp,
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
          out = here("output", "tables", "SI_no_imputation.tex"))

rm(panel_noimp, sample_noimp, wave7IDs_noimp)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# ROBUSTNESS: CONSERVATIVE x PERCEIVED COST INTERACTIONS ####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Tests whether partisanship moderates the effect of perceived costs
# on carbon pricing opposition (Reviewer 1, Point 9)
# Result: Interactions are jointly insignificant (F-test p=0.61 for M2, p=0.91 for M4)

# M2 with interactions: Conservative x all perceived cost variables
fit1c_int <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                  left_right_num + conservative *
                  (inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num +
                   gasprice_change_perceived_num),
                data = sample)
summary(fit1c_int)
nobs(fit1c_int)

# M4 with interactions: Full model + Conservative x perceived cost interactions
fit4a_int <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                  left_right_num + conservative *
                  (inc_heat_perceived_4 + inc_gas_perceived_4 + inc_overall_perceived_num +
                   gasprice_change_perceived_num) +
                  owner + home_size_num +
                  fossil_home + fossil_water + fossil_stove +
                  home_size_num * fossil_home +
                  bill_elec_num + bill_diesel_num +
                  drive + vehicle_num + km_driven_num +
                  drive * km_driven_num,
                data = sample)
summary(fit4a_int)
nobs(fit4a_int)

# F-tests: Do the interaction terms jointly improve model fit?
anova(fit1c, fit1c_int)  # M2: p = 0.61
anova(fit4a, fit4a_int)  # M4: p = 0.91

# Output: side-by-side comparison (original vs interaction) for SI
stargazer(fit1c, fit1c_int, fit4a, fit4a_int,
          type = "text", no.space = TRUE,
          out = here("output", "tables", "pricing_interactions_SI.txt"))

# LaTeX output for SI
stargazer(fit1c, fit1c_int, fit4a, fit4a_int,
          type = "latex", style = "ajps",
          title = "Partisanship--perceived cost interactions (robustness check)",
          column.labels = c("M2", "M2 + Int.", "M4", "M4 + Int."),
          dep.var.labels = c("Opposition to carbon pricing"),
          covariate.labels = c(
            "Education: High school", "Education: Some college", "Education: College", "Education: Graduate",
            "Income: 20,000-40,000", "Income: 40,000-60,000", "Income: 60,000-80,000", "Income: 80,000-100,000", "Income: 100,000 and over",
            "Rural (dummy)",
            "Left-right: 0-1 (1 is far right)",
            "Conservative (dummy)",
            "Perceived inc. heating: 1-50/mo", "Perceived inc. heating: 50-99/mo", "Perceived inc. heating: 100+/mo",
            "Perceived inc. gas: 1-50/mo", "Perceived inc. gas: 50-99/mo", "Perceived inc. gas: 100+/mo",
            "Perceived inc. overall costs",
            "Perceived inc. gas price (cents/L)",
            "Home owner (dummy)",
            "Home size (sq. ft.)",
            "Home heating: fossil fuels",
            "Water heating: fossil fuels",
            "Fossil fuel stove",
            "Monthly electricity bill",
            "Monthly gasoline/diesel bill",
            "Drives to work (dummy)",
            "Number of vehicles",
            "Yearly km driven",
            "Home size * fossil home",
            "Drives to work * Km driven",
            "Conservative * Heating 1-50/mo",
            "Conservative * Heating 50-99/mo",
            "Conservative * Heating 100+/mo",
            "Conservative * Gas 1-50/mo",
            "Conservative * Gas 50-99/mo",
            "Conservative * Gas 100+/mo",
            "Conservative * Overall costs",
            "Conservative * Gas price"
          ),
          single.row = TRUE, se = NULL,
          keep.stat = c("n", "adj.rsq"),
          out = here("output", "tables", "SI_conservative_x_perceived_interactions.tex"))

rm(fit1c_int, fit4a_int)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# SHAPLEY VARIANCE DECOMPOSITION (LMG)    ####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Decomposes model R-squared into the contribution of each predictor group,
# accounting for correlations between predictors (Reviewer 1, Point 7)

library(relaimpo)

# M1: Baseline model
relimp_m1 <- calc.relimp(fit0a, type = "lmg")

# M2: Perceived costs model
relimp_m2 <- calc.relimp(fit1c, type = "lmg")

# Clean names for display and figures
clean_m1 <- c(edu_5 = "Education", income_6 = "Income", rural = "Rural",
              left_right_num = "Left-Right Ideology", conservative = "Conservative")
clean_m2 <- c(edu_5 = "Education", income_6 = "Income", rural = "Rural",
              left_right_num = "Left-Right Ideology", conservative = "Conservative",
              inc_heat_perceived_4 = "Perc. Heating Cost Increase",
              inc_gas_perceived_4 = "Perc. Gas Cost Increase",
              inc_overall_perceived_num = "Perc. Overall Cost Increase",
              gasprice_change_perceived_num = "Perc. Gas Price Change")

# Save results to text file
sink(here("output", "tables", "shapley_decomposition.txt"))
cat("=== SHAPLEY (LMG) VARIANCE DECOMPOSITION ===\n\n")

cat("--- Model 1: Baseline ---\n")
cat("Total R-squared:", round(relimp_m1@R2, 4), "\n")
cat("N:", nobs(fit0a), "\n\n")
cat(sprintf("%-30s %10s %10s\n", "Predictor Group", "R2 Share", "% of R2"))
cat(paste(rep("-", 52), collapse=""), "\n")
ord1 <- order(relimp_m1@lmg, decreasing = TRUE)
for (i in ord1) {
  nm <- names(relimp_m1@lmg)[i]
  label <- ifelse(nm %in% names(clean_m1), clean_m1[nm], nm)
  cat(sprintf("%-30s %10.4f %9.1f%%\n", label,
              relimp_m1@lmg[i], relimp_m1@lmg[i] / relimp_m1@R2 * 100))
}

cat("\n\n--- Model 2: Perceived Costs ---\n")
cat("Total R-squared:", round(relimp_m2@R2, 4), "\n")
cat("N:", nobs(fit1c), "\n\n")
cat(sprintf("%-35s %10s %10s\n", "Predictor Group", "R2 Share", "% of R2"))
cat(paste(rep("-", 57), collapse=""), "\n")
ord2 <- order(relimp_m2@lmg, decreasing = TRUE)
for (i in ord2) {
  nm <- names(relimp_m2@lmg)[i]
  label <- ifelse(nm %in% names(clean_m2), clean_m2[nm], nm)
  cat(sprintf("%-35s %10.4f %9.1f%%\n", label,
              relimp_m2@lmg[i], relimp_m2@lmg[i] / relimp_m2@R2 * 100))
}
sink()

# Save figures
# M1
m1_shares <- relimp_m1@lmg
names(m1_shares) <- clean_m1[names(m1_shares)]
m1_pct <- sort(m1_shares / relimp_m1@R2 * 100, decreasing = FALSE)

png(here("output", "figures", "shapley_m1.png"), width = 800, height = 500, res = 150)
par(mar = c(5, 10, 4, 4))
bp <- barplot(m1_pct, horiz = TRUE, las = 1,
              main = "Shapley Variance Decomposition\nModel 1: Baseline",
              xlab = "% of Model R\u00b2 Explained",
              col = "steelblue", border = NA, xlim = c(0, 75))
text(m1_pct + 1.5, bp, labels = paste0(round(m1_pct, 1), "%"), cex = 0.8, adj = 0)
dev.off()

# M2
m2_shares <- relimp_m2@lmg
names(m2_shares) <- clean_m2[names(m2_shares)]
m2_pct <- sort(m2_shares / relimp_m2@R2 * 100, decreasing = FALSE)

png(here("output", "figures", "shapley_m2.png"), width = 900, height = 600, res = 150)
par(mar = c(5, 14, 4, 4))
bp2 <- barplot(m2_pct, horiz = TRUE, las = 1,
               main = "Shapley Variance Decomposition\nModel 2: Perceived Costs",
               xlab = "% of Model R\u00b2 Explained",
               col = "steelblue", border = NA, xlim = c(0, 60))
text(m2_pct + 1, bp2, labels = paste0(round(m2_pct, 1), "%"), cex = 0.7, adj = 0)
dev.off()


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# ROBUSTNESS: PROVINCIAL FIXED EFFECTS   ####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Re-estimates M1-M4 adding province dummies (ref = BC)
# Addresses adversarial critique point #13

library(sandwich)
library(lmtest)

sample$prov <- relevel(factor(sample$prov), ref = "BC")

fit0a_pfe <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                  left_right_num + conservative + prov, data = sample)

fit1c_pfe <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                  left_right_num + conservative +
                  inc_heat_perceived_4 + inc_gas_perceived_4 +
                  inc_overall_perceived_num + gasprice_change_perceived_num +
                  prov, data = sample)

fit3a_pfe <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                  left_right_num + conservative +
                  owner + home_size_num + fossil_home + fossil_water + fossil_stove +
                  home_size_num * fossil_home + bill_elec_num + bill_diesel_num +
                  drive + vehicle_num + km_driven_num + drive * km_driven_num +
                  prov, data = sample)

fit4a_pfe <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
                  left_right_num + conservative +
                  inc_heat_perceived_4 + inc_gas_perceived_4 +
                  inc_overall_perceived_num + gasprice_change_perceived_num +
                  owner + home_size_num + fossil_home + fossil_water + fossil_stove +
                  home_size_num * fossil_home + bill_elec_num + bill_diesel_num +
                  drive + vehicle_num + km_driven_num + drive * km_driven_num +
                  prov, data = sample)

# Text output
stargazer(fit0a_pfe, fit1c_pfe, fit3a_pfe, fit4a_pfe,
          type = "text", no.space = TRUE,
          out = here("output", "tables", "pricing_province_fe.txt"),
          keep = c("conservative", "rural", "left_right", "prov",
                   "inc_heat", "inc_gas", "inc_overall", "gasprice",
                   "fossil_home", "vehicle_num"),
          keep.stat = c("n", "adj.rsq"))

# LaTeX output
stargazer(fit0a_pfe, fit1c_pfe, fit3a_pfe, fit4a_pfe,
          type = "latex", style = "ajps", no.space = TRUE,
          title = "Determinants of opposition to carbon pricing (provincial fixed effects)",
          label = "table:province_fe",
          column.labels = c("M1", "M2", "M3", "M4"),
          dep.var.labels = "Opposition to carbon pricing",
          covariate.labels = c(
            "Education: High school", "Education: Some college",
            "Education: College", "Education: Graduate",
            "Income: 20,000-40,000", "Income: 40,000-60,000",
            "Income: 60,000-80,000", "Income: 80,000-100,000", "Income: 100,000+",
            "Rural (dummy)", "Left-right: 0-1 (1 is far right)",
            "Conservative (dummy)",
            "Perceived inc. heating: 1-50/mo", "Perceived inc. heating: 50-99/mo",
            "Perceived inc. heating: 100+/mo",
            "Perceived inc. gas: 1-50/mo", "Perceived inc. gas: 50-99/mo",
            "Perceived inc. gas: 100+/mo",
            "Perceived inc. overall costs", "Perceived inc. gas price (cents/L)",
            "Home owner", "Home size (sq. ft.)",
            "Home heating: fossil fuels", "Water heating: fossil fuels",
            "Fossil fuel stove", "Monthly electricity bill",
            "Monthly gasoline/diesel bill", "Drives to work",
            "Number of vehicles", "Yearly km driven",
            "Province: AB", "Province: ON", "Province: QC", "Province: SK",
            "Home size * fossil home", "Drives to work * Km driven"
          ),
          keep.stat = c("n", "adj.rsq"),
          out = here("output", "tables", "pricing_province_fe_latex.txt"))

rm(fit0a_pfe, fit1c_pfe, fit3a_pfe, fit4a_pfe)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# ROBUSTNESS: CLUSTERED STANDARD ERRORS  ####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Re-estimates M1-M4 with SEs clustered by province (5 clusters)
# Addresses adversarial critique point #12

sink(here("output", "tables", "pricing_clustered_se.txt"))
cat("=== CLUSTERED STANDARD ERRORS (province-clustered) ===\n\n")
for (mod_name in c("M1 (fit0a)", "M2 (fit1c)", "M3 (fit3a)", "M4 (fit4a)")) {
  m <- get(sub(" .*", "", tolower(mod_name)) |>
             (\(x) switch(x, m1="fit0a", m2="fit1c", m3="fit3a", m4="fit4a"))())
  cat("---", mod_name, "(N=", nobs(m), ") ---\n")
  print(round(coeftest(m, vcov = vcovCL(m, cluster = ~prov)), 4))
  cat("\n")
}
sink()



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# ATTRITION / COMPLETE-CASE BALANCE       ####
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Compares the full Wave 7 sample with the M4 complete cases (respondent-level,
# Wave 7 rows only). Two-sample t-tests: complete cases vs. dropped cases.

w7 <- panel %>% filter(wave == "wave7")
m4_ids <- unique(sample$responseid[-fit4a$na.action])
w7 <- w7 %>% mutate(in_m4 = responseid %in% m4_ids)

bal_vars <- c("Conservative voter (%)"           = "conservative",
              "Liberal voter (%)"                = "liberal",
              "Left-right ideology (0-1)"        = "left_right_num",
              "Rural residence (%)"              = "rural",
              "Female (%)"                       = "female",
              "French speaker (%)"               = "french",
              "Bachelor's degree or higher (%)"  = "bachelors",
              "Household income (mean, $CAD)"    = "income_num_mid",
              "Home owner (%)"                   = "owner",
              "Home heating: fossil fuels (%)"   = "fossil_home",
              "Drives to work (%)"               = "drive",
              "Number of vehicles (mean)"        = "vehicle_num",
              "Opposes carbon pricing (%)"       = "cp_oppose")

to_num <- function(x) if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)

sink(here("output", "tables", "attrition_balance.txt"))
cat("=== ATTRITION BALANCE TABLE ===\n")
cat(sprintf("Full Wave 7 sample: N=%d\n", nrow(w7)))
cat(sprintf("M4 complete cases:  N=%d\n", sum(w7$in_m4)))
cat(sprintf("Dropped cases:      N=%d (%.1f%%)\n\n", sum(!w7$in_m4), 100 * mean(!w7$in_m4)))
cat(sprintf("%-42s %8s %8s %8s %8s\n", "Variable", "Full", "M4", "Dropped", "p-val"))
cat(paste(rep("-", 78), collapse = ""), "\n")
for (lab in names(bal_vars)) {
  x <- to_num(w7[[bal_vars[lab]]])
  pct <- grepl("%", lab)
  scale <- if (pct) 100 else 1
  full <- mean(x, na.rm = TRUE) * scale
  m4   <- mean(x[w7$in_m4], na.rm = TRUE) * scale
  drop <- mean(x[!w7$in_m4], na.rm = TRUE) * scale
  p <- tryCatch(t.test(x[w7$in_m4], x[!w7$in_m4])$p.value, error = function(e) NA)
  stars <- ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))))
  cat(sprintf("%-42s %8.2f %8.2f %8.2f %8.3f %s\n", lab, full, m4, drop, p, stars))
}
cat(paste(rep("-", 78), collapse = ""), "\n")
cat("*** p<0.01, ** p<0.05, * p<0.10 (t-test: M4 complete vs. dropped cases)\n")
sink()
