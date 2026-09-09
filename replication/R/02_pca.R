# 02_pca.R
# Step 2: Principal Component Analysis and biplots (main text Figure 1).
# Input : data/derived/panel_imputed.rds
# Output: output/figures/biplot_oppose_12.png (Figure 1), biplot_support_12.png,
#         output/tables/pca_variance_explained.txt

source(here::here("R", "00_setup.R"))

# PCA                       ####
## ## ## ## ## ## ## ## ## ## ##

panel <- readRDS(here("data", "derived", "panel_imputed.rds"))


# .. Select features ####
miss <- panel %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

miss7 <- panel %>%
  filter(wave == "wave7") %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

table(panel$bill_heatingoil_num, panel$wave, useNA = "ifany")
panel %>%
  select(bill_heatingoil_num) %>%
  drop_na %>%
  nrow
# 20
# Can't use the heating oil variables

table(panel$km_driven_23, panel$wave, useNA = "ifany")
panel %>%
  select(km_driven_23) %>%
  drop_na %>%
  nrow
# 469
panel %>%
  select(km_driven_23, wave) %>%
  drop_na %>%
  group_by(wave) %>%
  tally
# This is all wave 7

table(panel$familiar_bills_3, panel$wave, useNA = "ifany")
# No missing, all wave 7

table(panel$bill_diesel_13, panel$wave, useNA = "ifany")
# No missing, all wave 1

panel %>%
  select(bill_diesel_num, wave) %>%
  drop_na %>%
  group_by(wave) %>%
  tally
# wave      n
# <fct> <int>
# 1 wave6   915
# 2 wave7   930

table(panel$inc_heat_perceived_6, panel$wave, useNA = "ifany")
# No missing, data for waves 1, 4, 6, and 7

panel %>%
  select(inc_heat_perceived_num, wave) %>%
  drop_na %>%
  group_by(wave) %>%
  tally
# <fct> <int>
# 1 wave6   915
# 2 wave7   576

select_features <- c(
  "conservative",
  "liberal",
  "female",
  "french",
  "bachelors",
  "income_num_mid",
  "rural",
  "owner",
  # "home_size_num",
  "vehicle_num",
  "drive",
  # "km_driven_num",
  "left_right_num",
  "inc_heat_perceived_num",
  "inc_gas_perceived_num",
  "inc_overall_perceived_num",
  "gasprice_change_perceived_num",
  # "gasprice_change_jan_perceived_num",
  "fossil_home",
  # "renewable_home",
  # "fossil_water",
  # "renewable_water",
  # "fossil_stove",
  # "renewable_stove"
  # "bill_elec_winter_num_mid",
  # "bill_elec_summer_num_mid",
  "bill_elec_num",
  # "bill_natgas_winter_num_mid",
  # "bill_natgas_summer_num_mid",
  # "bill_natgas_num",
  # "bill_heatingoil_winter_num_mid",
  # "bill_heatingoil_num",
  # "bill_diesel_num_mid",
  "bill_diesel_num"
)

select_labels <- c("cp_support",
                   "cp_oppose",
                   "cp_strongsupport",
                   "cp_strongoppose",
                   "party_9")

sample_pca <- panel %>%
  select(all_of(c(select_features, select_labels))) %>%
  mutate_at(vars(c(select_features)), ~bin_to_num(.)) %>%
  drop_na()


# .. Estimate principal components ####
pca <- prcomp(sample_pca[, !names(sample_pca) %in% select_labels], 
              scale = TRUE, center = TRUE)
summary(pca)
capture.output(summary(pca), file = here("output", "tables", "pca_variance_explained.txt"))  # share of variance per PC (Figure 1 axis labels)

# Extract loadings from PC1
PC1 <- pca$rotation[, 1]
sort(PC1, decreasing = T)

# Extract loadings from PC2
PC2 <- pca$rotation[, 2]
sort(PC2, decreasing = T)


# .. Biplots ####
g <- ggbiplot(pca,
              groups = sample_pca$cp_support,
              ellipse = TRUE,
              alpha = 0.3,
              varname.size = 8) +
  scale_color_manual(name = "Carbon pricing",
                     labels = c("Oppose",
                                "Support"),
                     values = c("#00B0F6", "#FFD84D")) +
  labs(title = "Principal components of support for carbon pricing") +
  theme(legend.text = element_text(size = 20))
g
ggsave(g,
       file = here("output", "figures", "biplot_support_12.png"),
       width = 6, height = 4, units = "in")

g <- ggbiplot(pca,
              groups = sample_pca$cp_oppose,
              ellipse = TRUE,
              alpha = 0.3,
              varname.size = 8) +
  scale_color_manual(name = "Survey respondent who:",
                     labels = c("(strongly) supports carbon pricing",
                                "(strongly) opposes carbon pricing"),
                     values = c("#FFD84D", "#00B0F6")) +
  labs(title = "Predictors of opposition to carbon pricing") +
  xlab("PC #1 (explains 15.1% of variation)") +
  ylab("PC #2 (explains 9.2% of variation)") +
  theme(legend.text = element_text(size = 20),
        legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1, 
                              override.aes = list(linetype = 0)))
g
ggsave(g,
       file = here("output", "figures", "biplot_oppose_12.png"),
       width = 7, height = 5, units = "in")

ggbiplot(pca,
         groups = sample_pca$cp_strongsupport,
         ellipse = TRUE,
         alpha = 0.3)
ggbiplot(pca,
         groups = sample_pca$cp_strongoppose,
         ellipse = TRUE,
         alpha = 0.3)
ggbiplot(pca,
         groups = sample_pca$party_9,
         ellipse = TRUE,
         alpha = 0.3)

# Plot other PCs
ggbiplot(pca,
         choices = c(1, 3),
         groups = sample_pca$cp_oppose,
         ellipse = TRUE,
         alpha = 0.3)
ggbiplot(pca,
         choices = c(3, 4),
         groups = sample_pca$cp_oppose,
         ellipse = TRUE,
         alpha = 0.3)


# .. Hierarchical clustering ####
# Compute Euclidean distance matrix
dist2 <- dist(sample_pca[, !names(sample_pca) %in% select_labels], method = "euclidean")

# Perform hierarchical clustering with complete linkage
set.seed(1509)
hclust <- hclust(dist2, method = "complete")

# Plot dendogram colored by 2 clusters
dend1 <- as.dendrogram(hclust)
dend1 <- color_branches(dend1, k = 2)
dend1 <- color_labels(dend1, k = 2)
dend1 <- set(dend1, "labels_cex", 0.5)
dend1 <- set_labels(dend1, 
                    labels = sample_pca$cp_oppose[order.dendrogram(dend1)])
plot(dend1)
rm(dend1, hclust, dist2)




