library(here)
library(relaimpo)

load(here("Data", "Processed", "panel_imputed.Rdata"))
bin_to_num <- function(x) as.numeric(as.character(x))
sample_data <- panel[panel$wave == "wave7", ]

# ---- M1: Baseline ----
fit0a <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative, data = sample_data)

cat("Running Shapley (LMG) for M1...\n")
relimp_m1 <- calc.relimp(fit0a, type = "lmg")
cat("M1 done. R2:", round(relimp_m1@R2, 4), "\n")
cat("M1 LMG shares:\n")
print(round(relimp_m1@lmg, 4))
cat("M1 LMG as % of R2:\n")
print(round(relimp_m1@lmg / relimp_m1@R2 * 100, 1))

# ---- M2: Perceived Costs ----
fit1c <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural +
              left_right_num + conservative +
              inc_heat_perceived_4 + inc_gas_perceived_4 +
              inc_overall_perceived_num + gasprice_change_perceived_num,
            data = sample_data)

cat("\nRunning Shapley (LMG) for M2 (9 groups, 2^9 = 512 models)...\n")
relimp_m2 <- calc.relimp(fit1c, type = "lmg")
cat("M2 done. R2:", round(relimp_m2@R2, 4), "\n")
cat("M2 LMG shares:\n")
print(round(relimp_m2@lmg, 4))
cat("M2 LMG as % of R2:\n")
print(round(relimp_m2@lmg / relimp_m2@R2 * 100, 1))

# ---- Save results text ----
sink(here("Results", "shapley_decomposition.txt"))
cat("=== SHAPLEY (LMG) VARIANCE DECOMPOSITION ===\n\n")

cat("--- Model 1: Baseline ---\n")
cat("Total R-squared:", round(relimp_m1@R2, 4), "\n")
cat("N:", nobs(fit0a), "\n\n")

# Clean names for display
clean_m1 <- c(edu_5 = "Education", income_6 = "Income", rural = "Rural",
              left_right_num = "Left-Right Ideology", conservative = "Conservative")

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

clean_m2 <- c(edu_5 = "Education", income_6 = "Income", rural = "Rural",
              left_right_num = "Left-Right Ideology", conservative = "Conservative",
              inc_heat_perceived_4 = "Perc. Heating Cost Increase",
              inc_gas_perceived_4 = "Perc. Gas Cost Increase",
              inc_overall_perceived_num = "Perc. Overall Cost Increase",
              gasprice_change_perceived_num = "Perc. Gas Price Change")

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

# ---- Save figures ----

# M1 bar plot
m1_shares <- relimp_m1@lmg
names(m1_shares) <- clean_m1[names(m1_shares)]
m1_pct <- sort(m1_shares / relimp_m1@R2 * 100, decreasing = FALSE)

png(here("Figures", "shapley_m1.png"), width = 800, height = 500, res = 150)
par(mar = c(5, 10, 4, 2))
bp <- barplot(m1_pct, horiz = TRUE, las = 1,
              main = "Shapley Variance Decomposition\nModel 1: Baseline",
              xlab = "% of Model R\u00b2 Explained",
              col = "steelblue", border = NA, xlim = c(0, 75))
# Add percentage labels
text(m1_pct + 1.5, bp, labels = paste0(round(m1_pct, 1), "%"), cex = 0.8, adj = 0)
dev.off()

# M2 bar plot
m2_shares <- relimp_m2@lmg
names(m2_shares) <- clean_m2[names(m2_shares)]
m2_pct <- sort(m2_shares / relimp_m2@R2 * 100, decreasing = FALSE)

png(here("Figures", "shapley_m2.png"), width = 900, height = 600, res = 150)
par(mar = c(5, 14, 4, 4))
bp2 <- barplot(m2_pct, horiz = TRUE, las = 1,
               main = "Shapley Variance Decomposition\nModel 2: Perceived Costs",
               xlab = "% of Model R\u00b2 Explained",
               col = "steelblue", border = NA, xlim = c(0, 55))
text(m2_pct + 1, bp2, labels = paste0(round(m2_pct, 1), "%"), cex = 0.7, adj = 0)
dev.off()

cat("\nDone. Outputs saved to:\n")
cat("  Figures/shapley_m1.png\n")
cat("  Figures/shapley_m2.png\n")
cat("  Results/shapley_decomposition.txt\n")
