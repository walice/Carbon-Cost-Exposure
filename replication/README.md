# Replication package

**Partisanship and perceived costs predict carbon pricing opposition better than objective costs**
Alice Lépissier, Matto Mildenberger, Kathryn Harrison, Chloé Boutron, Erick Lachapelle
*Climatic Change* (2026), https://doi.org/10.1007/s10584-026-04280-8

This package reproduces every table and figure in the article and its Supplementary Information from a single harmonised survey file. It is the minimal reproduction archive deposited at Harvard Dataverse. The full working repository, which also contains the exploratory analysis and the raw-survey processing, is at https://github.com/walice/Carbon-Cost-Exposure.

## Quick start

Requires R (developed and tested with R 4.5.1) and the packages listed below. From this folder:

```bash
Rscript run_all.R
```

Runtime is about two minutes. All console output, including the nested-model F-tests and the logit average marginal effects, is written to `output/run_log.txt`; `output/sessionInfo.txt` records the package versions used.

To run step by step, start any R session with this folder as the working directory (paths are resolved with the `here` package from the `.here` marker file) and source `R/01_impute.R` through `R/05_robustness.R` in order.

## Contents

```
replication/
├── run_all.R                 driver: runs the five scripts in order and logs output
├── R/
│   ├── 00_setup.R            packages, helper function, plot theme (sourced by every script)
│   ├── 01_impute.R           factor releveling, 4-level perception categories, imputation
│   ├── 02_pca.R              principal components and biplots            → Figure 1
│   ├── 03_regressions.R      LPMs of opposition, logit check, cost-perception models → Table 1, S1, S2, S3
│   ├── 04_trees.R            classification tree, 1,000-split simulation, random forests → Figures 2, 3
│   └── 05_robustness.R       no-imputation, interactions, Shapley, province FE, clustered SEs, attrition → S4–S7
├── data/
│   ├── panel_vars.rds        harmonised 7-wave panel, pre-imputation (11,781 respondent-waves × 83 variables)
│   ├── panel_vars.csv        the same file as CSV (factor level order is documented in codebook.csv)
│   ├── codebook.csv          variable name, class, missing count, factor levels, variable group
│   └── derived/              created by the scripts: panel_imputed.rds, models.rds
├── output/
│   ├── figures/              created by the scripts
│   └── tables/               created by the scripts
└── supplementary/
    └── DataPreparation.R     builds panel_vars from the raw survey exports (raw files not included, see below)
```

## Data

`data/panel_vars.rds` is the analysis input. It holds one row per respondent per survey wave for the seven waves of the Canadian carbon pricing panel (February 2019 to July 2022), restricted to the 83 variables used in the paper and its exploratory analysis. Respondents are identified only by a numeric survey ID. The Wave 7 sample analysed in the article is the 1,008 respondents with `wave == "wave7"`.

The raw survey exports from the polling firm (Stata and SPSS files for the seven waves) are not part of this package. `supplementary/DataPreparation.R` is the script that produced `panel_vars` from them and is included for transparency only; it will not run without the raw files.

`01_impute.R` fills missing Wave 7 covariates for the 1,008 respondents. Time-invariant variables (language, left-right placement) are carried forward from the earliest wave in which the respondent answered; bills and cost perceptions are carried forward from Wave 6 only. The result is `data/derived/panel_imputed.rds`.

## Mapping of outputs to the article

| Article item | File in `output/` |
|---|---|
| Figure 1 (PCA biplot) | `figures/biplot_oppose_12.png`; variance shares in `tables/pca_variance_explained.txt` |
| Table 1 (M1–M4) | `tables/Table1_full_model.tex` (LaTeX), `tables/pricing_full_model.txt` (text) |
| Figure 2 (classification tree) | `figures/classification_tree.pdf`; accuracy figures in `tables/tree_accuracy.txt` |
| Figure 3 (variable importance) | `figures/varimp_oppose.png` (top), `figures/varimp_support.png` (bottom) |
| SI Table S1 (cost perceptions) | `tables/SI_cost_perceptions.tex`, `tables/perceived_*.txt` |
| SI Table S2 (support vs. opposition) | `tables/SI_baseline_support_vs_oppose.tex`, `tables/pricing_base.txt` |
| SI Table S3 (LPM vs. logit) | LPM column as Table S2; logit average marginal effects in `tables/SI_logit_AME.csv` |
| SI Table S4 (no imputation) | `tables/SI_no_imputation.tex`, `tables/pricing_no_imputation.txt` |
| SI Table S5 (Shapley decomposition) | `tables/shapley_decomposition.txt`, `figures/shapley_m1.png`, `figures/shapley_m2.png` |
| SI Table S6 (province fixed effects) | `tables/pricing_province_fe_latex.txt`, `tables/pricing_province_fe.txt` |
| SI Table S7 (province-clustered SEs) | `tables/pricing_clustered_se.txt` |
| SI codebook | `tables/codebook_SI.tex` |
| Nested-model F-tests (text) | `output/run_log.txt`, section 03_regressions.R |
| Intermediate models in the SI text (`pricing_perceived.txt`, `pricing_actual.txt`, `pricing_actual_interactions.txt`) | `tables/` |
| Complete-case balance table | `tables/attrition_balance.txt` |

Two further files are produced but are not in the article: `tables/pricing_interactions_SI.txt` (Conservative × perceived-cost interactions, jointly insignificant) and `figures/classification_tree_best_of_1000.pdf` (the tree from the best of 1,000 random splits).

## Notes on samples (read before comparing numbers)

- **Table 1, S2 and S3** are estimated on `sample`, which contains every survey-wave observation of the 1,008 Wave 7 respondents. A respondent therefore contributes one row per wave in which the outcome and covariates are observed. M1 has N = 1,666 respondent-wave observations (698 from Wave 1, 137 from Wave 6, 831 from Wave 7) from 893 distinct respondents. M3 and M4 use Wave 7 rows only because their covariates were asked only in Wave 7.
- **SI Tables S5, S6 and S7** re-estimate the four models on Wave 7 rows only (M1 N = 831). `05_robustness.R` refits the models on that subsample before those sections; this reproduces the published SI files exactly.
- **Figure 2.** The published tree (10 leaves, 706 training observations) was grown from the feature list without the two monthly-bill variables; `04_trees.R` reproduces it as `classification_tree.pdf`. The tree grown with the current, fuller feature list is saved as `classification_tree_with_bills.pdf`; its out-of-sample accuracy (69.48%) is the figure quoted in the published caption. `tables/tree_accuracy.txt` reports both.
- All random procedures (train/test split, tree growth, random forests, the 1,000-split simulation) use `set.seed(1509)` as in the working scripts.

## Software

R 4.5.1 with: caret, dendextend, ggbiplot (GitHub `vqv/ggbiplot`, commit 7325e88), kableExtra, lmtest, maptree, margins, naniar, randomForest, regclass, relaimpo, reshape2, rpart, rpart.plot, sandwich, showtext, stargazer, sysfonts, tidyverse, tree, here. Exact versions are in `output/sessionInfo.txt` after a run.

Install the CRAN packages with `install.packages()` and ggbiplot with:

```r
devtools::install_github("vqv/ggbiplot@7325e880485bea4c07465a0304c470608fffb5d9")
```

Figures use the Montserrat and Lato Google fonts through `showtext`; if the fonts cannot be downloaded the scripts fall back to the default sans-serif font and print a message. Values are unaffected.

## Citation

Lépissier A, Mildenberger M, Harrison K, Boutron C, Lachapelle E (2026) Replication data for: Partisanship and perceived costs predict carbon pricing opposition better than objective costs. Harvard Dataverse. [DOI to be inserted]
