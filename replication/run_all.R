# run_all.R
# Runs the full replication from the harmonised panel to every table and figure.
# Usage (from the replication folder):  Rscript run_all.R
# Runtime: a few minutes on a laptop (the 1,000-split tree simulation and the
# Shapley decomposition are the slowest steps).
# All console output, including the nested-model F-tests and the logit
# average marginal effects, is written to output/run_log.txt.

here::i_am("run_all.R")
dir.create(here::here("data", "derived"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("output", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("output", "tables"), showWarnings = FALSE, recursive = TRUE)

log_file <- file(here::here("output", "run_log.txt"), open = "wt")
sink(log_file, split = TRUE)
sink(log_file, type = "message")

scripts <- c("01_impute.R", "02_pca.R", "03_regressions.R", "04_trees.R", "05_robustness.R")
for (s in scripts) {
  cat("\n\n==========", s, "==========\n\n")
  t0 <- Sys.time()
  source(here::here("R", s), echo = FALSE, local = new.env())
  cat("\n[", s, "finished in", round(difftime(Sys.time(), t0, units = "secs")), "s ]\n")
}

cat("\n\n========== sessionInfo ==========\n\n")
print(sessionInfo())
sink(type = "message"); sink()
close(log_file)
writeLines(capture.output(sessionInfo()), here::here("output", "sessionInfo.txt"))
