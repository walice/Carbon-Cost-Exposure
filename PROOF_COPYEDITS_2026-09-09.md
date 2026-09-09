# Proof copyedits — "Partisanship and perceived costs predict carbon pricing opposition better than objective costs"

Climatic Change, DOI 10.1007/s10584-026-04280-8. Read against the Springer Nature Proof Editor on 2026-09-09, cross-checked against the replication code and the regenerated tables.

Items are grouped by priority. "Find" text is quoted exactly as it appears in the proof so you can search for it in the editor.

## A. Errors that change meaning — fix before submitting the proof

| # | Where | Find | Change to | Why |
|---|---|---|---|---|
| A1 | §4.2, first sentence | `with the Canadian carbon price at $5 per tonne` | `at $50 per tonne` | The 2022 federal price was CAD 50/t; the manuscript said 50. A zero was lost in typesetting. |
| A2 | Author list | Lépissier, Mildenberger, Harrison, Boutron, Lachapelle | confirm | The proof's author order differs from the LaTeX source (Lépissier, Boutron, Lachapelle, Mildenberger, Harrison). Confirm the proof order is the one you submitted. |
| A3 | Declarations → Ethics approval | `The 449 survey received a human subjects review` | `The survey received a human subjects review` | A line number from the submitted manuscript leaked into the text. |
| A4 | Data availability and Code availability | `Harvard Dataverse replication archive at [URL]` | insert the Dataverse DOI | Placeholder still present. Create the Dataverse draft now; it assigns a DOI immediately, before you publish the dataset. Use the same DOI in both statements. |
| A5 | Fig. 2 caption | `The overall out-of-sample prediction accuracy is 69.48%, compared to a majority-class baseline of 52.6%` | see note | The tree drawn in Fig. 2 (10 leaves, 706 training obs) was grown without the two monthly-bill variables. Its out-of-sample accuracy is 64.20% against a 50.6% majority baseline. The 69.48% comes from the tree grown with the bill variables, which has 14 leaves after pruning and a 55.8% baseline. Neither run produces 52.6%. Minimal fix: change the caption to `64.20%, compared to a majority-class baseline of 50.6%`. Alternative: replace the figure with the 14-leaf tree and keep 69.48% / 55.8%. |
| A6 | Table 1 and text (n = 1008) | table note | add a sample note | Table 1 pools every survey-wave observation of the 1008 Wave 7 respondents. M1 has N = 1666 respondent-wave rows (698 from Wave 1, 137 from Wave 6, 831 from Wave 7; 893 distinct respondents). SI Tables S5–S7 use Wave 7 rows only (M1 N = 831). Suggested table note: "M1 and M2 pool all survey-wave observations of Wave 7 respondents with non-missing outcome and covariates, so a respondent can contribute up to three observations; M3 and M4 use Wave 7 responses only. SI Tables S5–S7 re-estimate all models on Wave 7 responses only." The same applies to SI Table S2 and S3 (N = 1666). |
| A7 | SI Table S3 (logit column) | `Left-right: 0-1 (1 is far right) … −0.358***` | `0.358***` | The average marginal effect is positive (+0.358, SE 0.052); the minus sign is a typo. The SI is a separate file, so fix it there. |

## B. Typos and broken references in the proof

| # | Where | Find | Change to |
|---|---|---|---|
| B1 | Intro, para 1 | `will mobilize the publci to protect` | `will mobilize the public to protect` |
| B2 | Intro, para 3 | `Starting at CAD 20 per tonnes` | `Starting at CAD 20 per tonne` |
| B3 | §1, para 1 | `Sælen and Kallbekken 2011;)` | `Sælen and Kallbekken 2011)` |
| B4 | §1, para 1 | `Thalmann 2004;)` | `Thalmann 2004)` |
| B5 | §1, para 2 | `the "Yellow Vest" movement which violently protested an increase in the French carbon tax a decade later were principally residents of exurbs` | `participants in the "Yellow Vest" movement, which violently protested an increase in the French carbon tax a decade later, were principally residents of exurbs` |
| B6 | §1, para 3 | `Harrison (2013), p. 12) reported` | `Harrison (2013, p. 12) reported` |
| B7 | §1, para 3 | `once the public can better perceive the policy's material benefits Schuitema et al. (2010), Murray and Rivers (2015), Mildenberger et al. (2016), Konc et al. (2022).` | `… material benefits (Schuitema et al. 2010; Murray and Rivers 2015; Mildenberger et al. 2016; Konc et al. 2022).` |
| B8 | §4.1, para 1 | `which together explain, 24% of the variance` | `which together explain 24% of the variance` |
| B9 | §4.1, para 2 | `is different from the distribution opposition predictors` | `is different from the distribution of opposition predictors` |
| B10 | §4.2, para 3 | `In SI Section Table S1, we explore` | `In SI Table S1, we explore` |
| B11 | §4.3, para 1 | `in predicting carbon tax opposition (Table 1. We first present` | `in predicting carbon tax opposition (Table 1). We first present` |
| B12 | §4.3, para 3 | `Table SI5 also presents` | `SI Table S5 also presents` |
| B13 | §4.3, last para | `In SI Table S6 and S7 we further show` | `In SI Tables S6 and S7 we further show` |
| B14 | §4.4, para 2 | `the second-most important variables used to split the data is perceptions of the overall energy costs of carbon pricing, and the dummy variable for rurality` | `the second-most important variables used to split the data are perceptions of the overall energy costs of carbon pricing and the dummy variable for rurality` |
| B15 | Footnote 5 | `The Principal Components Analysis that was presented in section showed` | `… presented in Sect. 4.1 showed` (cross-reference dropped) |
| B16 | §4.2 and §4.3 | `$CDN2004`, `$CDN700`, etc. vs `CAD 20`, `CAD 0` in the Intro | pick one currency style (Springer house style is `CAD 2,004`) |
| B17 | §1, para 1 | `(Dresner et al. 2006; Smith et al. 2026)` | confirm | Smith et al. (2026) is cited as evidence from "European focus groups"; check that this is the intended reference and that the sentence still describes it accurately. |
| B18 | §4.4, para 2 vs §4.2 | `SI Section B` and `SI Table S1` both refer to the cost-perception models | use one form throughout, e.g. `SI Section B (Table S1)` |

## C. Reference list

| # | Entry | Problem | Fix |
|---|---|---|---|
| C1 | Breiman L (2001) Statistical modeling: the two cultures | Cited in §4.4 as the source for the random forest algorithm. The standard citation is Breiman L (2001) Random forests. Mach Learn 45(1):5–32. https://doi.org/10.1023/A:1010933404324 | Confirm which paper you intend; swap if needed. |
| C2 | Carattini S, Kallbekken S, Orlov A (2019). How to win public support for a global carbon tax | No journal, volume or pages | `Nature 565(7739):289–291. https://doi.org/10.1038/d41586-019-00124-x` |
| C3 | Harrison K (2013) … In: Tech. rep., OECD. Paris. https://doi.org/ENV/WKP(2013)10 | Malformed DOI and venue | `OECD Environment Working Papers No. 63. OECD Publishing, Paris. https://doi.org/10.1787/5k3z04gkkhkg-en` |
| C4 | Shorrocks AF et al (2013) | Single-author paper | `Shorrocks AF (2013) … based on the Shapley value` |
| C5 | Mildenberger M (2020) Carbon captured … MiT Press | Capitalization | `MIT Press` |
| C6 | Mildenberger M, Howe P, … (2016) The distribution of climate change Public opinion in Canada | Capitalization | `public opinion` |
| C7 | Hammar H, Jagers SC (2006) … The case of CO 2 tax | Spacing | `CO2 tax` |
| C8 | Baker JA III, Feldstein M, Halstead T et al (2017) … Clim Leadersh Counc 1–2 | Abbreviated publisher | `Climate Leadership Council, Washington, DC` |
| C9 | Sénit CA (2012) … IDDRI SciencesPo, Paris 20(10) | Garbled series | `IDDRI Working Paper 20/12. IDDRI/Sciences Po, Paris` |
| C10 | Benzie R (2020) … attacking carbon- pricing are | Stray space | `carbon-pricing` |
| C11 | Depraz S (2019) … Fracture (s) territoriale (s) | Stray spaces | `Fracture(s) territoriale(s)` |
| C12 | Smith EK, Mlakar Ž, Levis A et al (2026) Climate policy feasibility across europe … Nat Clim Change 1–8 | Capitalization; no volume | `across Europe`; add volume/pages or DOI if published |

## D. Supplementary Information (separate PDF, generated from the LaTeX source)

Checked in the LaTeX source `ClimateChange_2ndRevision.tex`; verify against the SI file you uploaded.

| # | Where | Find | Change to |
|---|---|---|---|
| D1 | Table S3, logit column | `−0.358***` for Left-right | `0.358***` (see A7) |
| D2 | Notes under Tables S1, S2, S3 | `The sample consists of respondents who have provided an answer for all survey waves (1 to 7).` | Not accurate. S2/S3 pool wave observations of Wave 7 respondents (see A6); S1 uses Wave 7 complete cases (N = 330, 357, 210). Suggested: "The sample consists of Wave 7 respondents with non-missing values on all variables in the model." |
| D3 | Survey instrument, Gas Cost Increase Amount Yearly | `Between 5 and 10 cents per litre` appears twice | delete the duplicate |
| D4 | Same question | `Not sure` appears twice | delete the duplicate |
| D5 | Survey instrument, Effectiveness and Fairness | `\itemWill have a negative impact …` and `\itemWill have no impact on the decisions …` | `\item Will …` (missing space; these items will not render) |
| D6 | Survey instrument, Commute Type | `\itemWalk` and `\itemThis doesn't apply to me` | `\item Walk`, `\item This doesn't apply to me` |
| D7 | Survey instrument, Next Election | `Row:` followed by `\item` lines with no `\begin{itemize}` | add `\begin{itemize} … \end{itemize}` around the party list |
| D8 | Survey instrument, income question | `\$120,000-\$159, 999` | `\$120,000-\$159,999` |
| D9 | LaTeX source | two tables carry `\label{table:baseline}` (Tables S2 and S3) | give the logit table its own label |

## E. Reproduction check (nothing to change, for your information)

Regenerated from the harmonised panel with the replication package in `replication/`:

- Table 1: all 34 reported coefficient rows, N and adjusted R² match.
- Table S1 (cost perceptions), Table S2 (support vs. opposition, incl. AIC), Table S4 (no imputation), Table S5 (Shapley), Table S6 (province FE), Table S7 (clustered SEs): identical to the files in `Results/`.
- Table S3 logit AMEs: match except for the sign typo in A7.
- Figure 1: PC1 15.1%, PC2 9.2%, 24.3% cumulative; matches the axis labels.
- Figure 3: same importance values and ordering.
- Figure 2: reproduced exactly by the tree without the bill variables (see A5).
