# Cost Exposure paper
# Analysis
# Alice Lepissier
# alice_lepissier@brown.edu

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Codebook
# Functions
# Transformations
# .. Relevel factors
# Regressions
# .. Regress support for carbon pricing on perceived costs
# .. Regress support for carbon pricing on actual costs
# PCA



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(dendextend)
library(devtools)
library(here)
library(ggbiplot)
library(naniar)
library(regclass)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# CODEBOOK                  ####
## ## ## ## ## ## ## ## ## ## ##

load(here::here("Data", "Processed", "panel_vars.Rdata"))
panel <- panel_vars
rm(panel_vars)

codebook <- read.csv(here::here("Data", "codebook.csv"))

id.vars <- codebook %>%
  filter(group == "id.vars") %>%
  select(vars) %>%
  pull
treatment.vars <- codebook %>%
  filter(group == "treatment.vars") %>%
  select(vars) %>%
  pull
demographic.vars <- codebook %>%
  filter(group == "demographic.vars") %>%
  select(vars) %>%
  pull
household.vars <- codebook %>%
  filter(group == "household.vars") %>%
  select(vars) %>%
  pull
transport.vars <- codebook %>%
  filter(group == "transport.vars") %>%
  select(vars) %>%
  pull
partisanship.vars <- codebook %>%
  filter(group == "partisanship.vars") %>%
  select(vars) %>%
  pull
opinions.vars <- codebook %>%
  filter(group == "opinions.vars") %>%
  select(vars) %>%
  pull
perceptions.vars <- codebook %>%
  filter(group == "perceptions.vars") %>%
  select(vars) %>%
  pull
energy.vars <- codebook %>%
  filter(group == "energy.vars") %>%
  select(vars) %>%
  pull
bill.vars <- codebook %>%
  filter(group == "bill.vars") %>%
  select(vars) %>%
  pull



## ## ## ## ## ## ## ## ## ## ##
# FUNCTIONS                 ####
## ## ## ## ## ## ## ## ## ## ##

bin_to_num <- function(x) {
  return(as.numeric(as.character(x)))
}



## ## ## ## ## ## ## ## ## ## ##
# TRANSFORMATIONS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Relevel factors ####
panel <- panel %>%
  mutate(edu_5 = fct_relevel(edu_5,
                             "Less than high school",
                             "High school",
                             "Some college",
                             "College",
                             "Graduate or prof. degree"),
         income_6 = fct_relevel(income_6,
                                "Less than 20,000",
                                "20,000-40,000",
                                "40,000-60,000",
                                "60,000-80,000",
                                "80,000-100,000",
                                "100,000 and over"))



## ## ## ## ## ## ## ## ## ## ##
# REGRESSIONS               ####
## ## ## ## ## ## ## ## ## ## ##

sample <- panel %>%
  filter(wave == "wave7")
  

# .. Regress support for carbon pricing on perceived costs ####
fit1 <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural + left_right_num + 
             inc_heat_perceived_num + inc_gas_perceived_num + inc_overall_perceived_num + 
             gasprice_change_perceived_num, 
   data = sample)
summary(fit1)
nobs(fit1)
VIF(fit1)


# .. Regress support for carbon pricing on actual costs ####
fit2 <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural + left_right_num + 
             home_size_num + vehicle_num + drive + km_driven_num +
             bill_elec_num + bill_natgas_num + bill_diesel_num,
           data = sample)
summary(fit2)
nobs(fit2)
VIF(fit2)

fit2 <- lm(bin_to_num(cp_oppose) ~ edu_5 + income_6 + rural + left_right_num + 
             home_size_num + vehicle_num + drive + km_driven_num +
             bill_elec_num + bill_natgas_num + bill_diesel_num,
           data = sample)
summary(fit2)
nobs(fit2)

fit3 <- lm(bin_to_num(cp_oppose) ~ home_size_num + vehicle_num + drive + km_driven_num +
             bill_elec_num + bill_natgas_num + bill_diesel_num,
           data = sample)
summary(fit3)
nobs(fit3)
VIF(fit3)

fit4 <- lm(bin_to_num(cp_oppose) ~ home_size_num + vehicle_num + drive + km_driven_num +
             bill_elec_num + bill_natgas_num + bill_diesel_num +
             drive*vehicle_num,
           data = sample)
summary(fit4)
nobs(fit4)
VIF(fit4)

fit5 <- lm(bin_to_num(cp_oppose) ~ home_size_num + vehicle_num + drive + km_driven_num +
             bill_elec_num + bill_natgas_num + bill_diesel_num +
             drive*km_driven_num,
           data = sample)
summary(fit5)
nobs(fit5)
VIF(fit5)

fit6 <- lm(bin_to_num(cp_oppose) ~ home_size_num + vehicle_num + drive + km_driven_num +
             bill_elec_num + bill_natgas_num + bill_diesel_num +
             drive*km_driven_num + home_size_num*bill_elec_num,
           data = sample)
summary(fit6)
nobs(fit6)
VIF(fit6)

fit7 <- lm(bin_to_num(cp_oppose) ~ home_size_num + vehicle_num + drive + km_driven_num +
             bill_elec_num + bill_natgas_num + bill_diesel_num +
             drive*bill_diesel_num,
           data = sample)
summary(fit7)
nobs(fit7)
VIF(fit7)



## ## ## ## ## ## ## ## ## ## ##
# PCA                       ####
## ## ## ## ## ## ## ## ## ## ##

miss <- panel %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

panel <- panel %>%
  mutate(conservative = case_when(party_9 == "Conservative Party" ~ 1,
                                  party_9 != "Conservative Party" ~ 0),
         liberal = case_when(party_9 == "Liberal Party" ~ 1,
                            party_9 != "Liberal Party" ~ 0))

select_features <- c(
                     "conservative",
                     "liberal",
                     "female",
                     # "french",
                     "bachelors",
                     "income_num_mid",
                     "rural",
                     "owner",
                     # "home_size_num",
                     "vehicle_num",
                     "drive",
                     "km_driven_num",
                     # "left_right_num",
                     "inc_heat_perceived_num",
                     "inc_gas_perceived_num",
                     "inc_overall_perceived_num",
                     "gasprice_change_perceived_num",
                     "gasprice_change_jan_perceived_num",
                     "fossil_home"
                     # "renewable_home",
                     # "fossil_water",
                     # "renewable_water",
                     # "fossil_stove",
                     # "renewable_stove"
                     # "bill_elec_winter_num_mid",
                     # "bill_elec_summer_num_mid",
                     # "bill_elec_num",
                     # "bill_natgas_winter_num_mid",
                     # "bill_natgas_summer_num_mid",
                     # "bill_natgas_num",
                     # "bill_heatingoil_winter_num_mid",
                     # "bill_heatingoil_num",
                     # "bill_diesel_num_mid",
                     # "bill_diesel_num"
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

pca <- prcomp(sample_pca[, !names(sample_pca) %in% select_labels], 
              scale = TRUE, center = TRUE)

summary(pca)
ggbiplot(pca)

ggbiplot(pca,
         # labels = sample_pca$cp_oppose, 
         groups = sample_pca$cp_support,
         ellipse = TRUE)
ggbiplot(pca,
         # labels = sample_pca$cp_oppose, 
         groups = sample_pca$cp_strongoppose,
         ellipse = TRUE)
ggbiplot(pca,
         choices = c(1, 3),
         groups = sample_pca$cp_strongoppose,
         ellipse = TRUE)
ggbiplot(pca,
         choices = c(1, 2),
         groups = sample_pca$party_9,
         ellipse = TRUE)

# Extract loadings from PC1
PC1 <- pca$rotation[,1]
sort(abs(PC1), decreasing = T)

# Extract loadings from PC2
PC2 <- pca$rotation[,2]
sort(abs(PC2), decreasing = T)

# Compute Euclidean distance matrix
dist2 <- dist(sample_pca[, !names(sample_pca) %in% select_labels], method = "euclidean")
# Set seed for reproducibility
set.seed(1)
# Perform hierarchical clustering with complete linkage
hclust <- hclust(dist2, method = "complete")

# Plot dendogram colored by three clusters
dend1 <- as.dendrogram(hclust)
dend1 <- color_branches(dend1, k = 2)
dend1 <- color_labels(dend1, k = 2)
dend1 <- set(dend1, "labels_cex", 0.5)
dend1 <- set_labels(dend1, 
                    labels = sample_pca$cp_oppose[order.dendrogram(dend1)])
plot(dend1, main = "Dendrogram colored by three clusters")
