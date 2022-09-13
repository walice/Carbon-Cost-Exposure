# Cost Exposure paper
# Analysis
# Alice Lepissier
# alice_lepissier@brown.edu

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Graphical Parameters
# Codebook
# Functions
# Transformations
# .. Relevel factors
# .. Add to codebook
# Data Imputation
# .. Impute time-invariant variables from previous waves
# .. Impute missing data on perceptions from wave 6
# .. Impute missing data on bills from wave 6
# .. Check coverage of data imputation procedure
# PCA
# .. Select features
# .. Estimate principal components
# .. Biplots
# .. Hierarchical clustering
# Regressions
# .. Baseline model of support for carbon pricing
# .. Regress support for carbon pricing on perceived costs
# .. Regress support for carbon pricing on actual costs
# .. Add interaction terms for actual costs
# .. Regress carbon pricing support on a model of perceived and actual costs
# .. Compare nested models
# Classification Trees
# .. Prep the data
# .. Randomly split data into training and test sets
# .. Use the tree package
# .. Simulate 1,000 trees to obtain most important variable
# .. Random forest approach
# .. Create variable importance plot
# Cost Perceptions
# .. Model perceptions of carbon pricing on gasoline costs
# .. Model perceptions of carbon pricing on heating costs
# .. Model perceptions of carbon pricing on overall costs



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(caret)
library(dendextend)
library(devtools)
library(ggbiplot)
library(kableExtra)
library(maptree)
library(naniar)
library(regclass)
library(reshape2)
library(rpart)
library(rpart.plot)
library(showtext)
library(stargazer)
library(sysfonts)
library(tidyverse)
library(tree)
library(here) # Attach last to avoid conflicts
set.seed(1509)



## ## ## ## ## ## ## ## ## ## ##
# GRAPHICAL PARAMETERS      ####
## ## ## ## ## ## ## ## ## ## ##

theme_set(theme_minimal())
font_add_google("Montserrat", "montserrat")
font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 96)

theme_update(text = element_text(size = 20,
                                 family = "montserrat"),
             legend.spacing.x = unit("0.2", "cm"),
             title = element_text(size = 30))
my_theme <- theme(text = element_text(size = 20,
                                      family = "montserrat"),
                  legend.spacing.x = unit("0.2", "cm"),
                  legend.spacing.y = unit("0.2", "cm"),
                  title = element_text(size = 30))



## ## ## ## ## ## ## ## ## ## ##
# CODEBOOK                  ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "Processed", "panel_vars.Rdata"))
panel <- panel_vars
rm(panel_vars)

codebook <- read.csv(here("Data", "codebook.csv"))

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
                                           "I don't know"),
         gasprice_change_perceived_5 = fct_relevel(gasprice_change_perceived_5,
                                                   "Not sure",
                                                   "Gotten less expensive",
                                                   "I don't use gasoline",
                                                   "Stayed about the same",
                                                   "Gotten more expensive"))

panel <- panel %>%
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
panel <- panel %>%
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


# .. Add to codebook ####
add_to_codebook <- data.frame(vars = c("inc_heat_perceived_4", "inc_gas_perceived_4"),
                              class = c(class(panel$inc_heat_perceived_4), class(panel$inc_gas_perceived_4)),
                              missing_obs = c(sum(is.na(panel$inc_heat_perceived_4)), sum(is.na(panel$inc_gas_perceived_4))),
                              factor_levels = rep(paste(c("$0 per month",
                                                          "$1-$50 per month",
                                                          "$50-$99 per month",
                                                          "$100 or more per month"), collapse = ", "), 2),
                              group = rep("perceptions.vars", 2),
                              row.names = NULL)

codebook <- rbind(codebook, add_to_codebook)
codebook <- codebook %>%
  arrange(group, vars) %>%
  distinct(vars, .keep_all = TRUE)

write.csv(codebook, file = here("Data", "codebook.csv"), row.names = FALSE)

used <- c("cp_support",
          "cp_oppose",
          "female",
          "french",
          "edu_5",
          "bachelors",
          "income_6",
          "income_num_mid",
          "rural_7",
          "rural",
          "homeowner_3",
          "owner",
          "home_size_num",
          "vehicle_num",
          "commute_7",
          "drive",
          "km_driven_num",
          "left_right_num",
          "party_9",
          "conservative",
          "liberal",
          "inc_heat_perceived_4",
          "inc_heat_perceived_num",
          "inc_gas_perceived_4",
          "inc_gas_perceived_num",
          "inc_overall_perceived_num",
          "gasprice_change_perceived_num",
          "fossil_home",
          # "renewable_home",
          "fossil_water",
          # "renewable_water",
          "fossil_stove",
          # "renewable_stove",
          "familiar_bills_3",
          "bill_elec_num",
          # "bill_natgas_num",
          "bill_diesel_num")

kbl(codebook %>% 
      filter(vars %in% used) %>%
      rename(missing = missing_obs,
             variable = vars) %>%
      mutate(class = ifelse(class == "factor", "categorical", class)),
    format = "latex",
    longtable = TRUE) %>%
  kable_classic(latex_options = "striped") %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "15em", monospace = TRUE) %>%
  column_spec(3, width = "4em") %>%
  column_spec(4, width = "13em")

rm(used)



## ## ## ## ## ## ## ## ## ## ##
# DATA IMPUTATION           ####
## ## ## ## ## ## ## ## ## ## ##

miss <- panel %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

miss7 <- panel %>%
  filter(wave == "wave7") %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

miss6 <- panel %>%
  filter(wave == "wave6") %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

wave7IDs <- panel %>% filter(wave == "wave7") %>% pull(responseid)


# .. Impute time-invariant variables from previous waves ####
# Impute from most complete waves since not likely to change over time
# French
table(panel$french, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0     2667  1968  1432  1174   752   731     0
# 1      646   473   328   266   147   189     0
# <NA>     0     0     0     0     0     0  1008

french_imputed <- full_join(panel %>%
                              filter(is.na(french)) %>%
                              select(-french) %>%
                              filter(responseid %in% wave7IDs) %>%
                              filter(wave == "wave7"),
                            panel %>%
                              filter(responseid %in% wave7IDs) %>%
                              filter(wave == "wave1") %>%
                              select(responseid, french),
                            by = c("responseid"))
table(french_imputed$french, french_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$french)), "french"] <- french_imputed[which(french_imputed$responseid %in% wave7IDs & 
                                                                        french_imputed$wave == "wave7"), "french"] 
table(panel$french, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0     2667  1968  1432  1174   752   731   712
# 1      646   473   328   266   147   189   220
# <NA>     0     0     0     0     0     0    76

# Left/right spectrum
table(panel$left_right_num, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0       58    10     0     0     0     0     0
# 0.1    164     6     0     0     0    17    17
# 0.2    152     7     0     0     0    60    63
# 0.3    256    30     0     0     0    85    93
# 0.4    260    20     0     0     0    77    68
# 0.5   1090    90     0     0     0     0     0
# 0.6    234    17     0     0     0    81    86
# 0.7    270    22     0     0     0    90    89
# 0.8    197    19     0     0     0    55    91
# 0.9     93     7     0     0     0    18    19
# <NA>   539  2213  1760  1440   899   437   482

left_right_num_imputed <- full_join(panel %>%
                                      filter(is.na(left_right_num)) %>%
                                      select(-left_right_num) %>%
                                      filter(responseid %in% wave7IDs) %>%
                                      filter(wave == "wave7"),
                                    panel %>%
                                      filter(responseid %in% wave7IDs) %>%
                                      filter(wave == "wave1") %>%
                                      select(responseid, left_right_num),
                                    by = c("responseid"))
table(left_right_num_imputed$left_right_num, left_right_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$left_right_num)), "left_right_num"] <- left_right_num_imputed[which(left_right_num_imputed$responseid %in% wave7IDs & 
                                                                                                left_right_num_imputed$wave == "wave7"), "left_right_num"] 
table(panel$left_right_num, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0       58    10     0     0     0     0    13
# 0.1    164     6     0     0     0    17    33
# 0.2    152     7     0     0     0    60    82
# 0.3    256    30     0     0     0    85   131
# 0.4    260    20     0     0     0    77   105
# 0.5   1090    90     0     0     0     0   145
# 0.6    234    17     0     0     0    81   111
# 0.7    270    22     0     0     0    90   129
# 0.8    197    19     0     0     0    55   122
# 0.9     93     7     0     0     0    18    28
# <NA>   539  2213  1760  1440   899   437   109


# .. Impute missing data on perceptions from wave 6 ####
miss7 %>%
  filter(variable == "inc_heat_perceived_num" |
           variable == "inc_overall_perceived_num" |
           variable == "inc_gas_perceived_num")
# variable                  n_miss pct_miss
# <chr>                      <int>    <dbl>
# 1 inc_heat_perceived_num       596     59.1
# 2 inc_overall_perceived_num    469     46.5
# 3 inc_gas_perceived_num        323     32.0

# Perceived increase in heating costs as a result of carbon pricing
panel %>% 
  filter(is.na(inc_heat_perceived_num)) %>%
  group_by(wave) %>% 
  select(inc_heat_perceived_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6     5
# 7 wave7   596

inc_heat_perceived_num_imputed <- full_join(panel %>%
                                              filter(is.na(inc_heat_perceived_num)) %>%
                                              select(-inc_heat_perceived_num) %>%
                                              filter(responseid %in% wave7IDs) %>%
                                              filter(wave == "wave7"),
                                            panel %>%
                                              filter(responseid %in% wave7IDs) %>%
                                              filter(wave == "wave6") %>%
                                              select(responseid, inc_heat_perceived_num),
                                            by = c("responseid"))
table(inc_heat_perceived_num_imputed$inc_heat_perceived_num, inc_heat_perceived_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$inc_heat_perceived_num)), "inc_heat_perceived_num"] <- inc_heat_perceived_num_imputed[which(inc_heat_perceived_num_imputed$responseid %in% wave7IDs & 
                                                                                                                        inc_heat_perceived_num_imputed$wave == "wave7"), "inc_heat_perceived_num"] 
panel %>% 
  filter(is.na(inc_heat_perceived_num)) %>%
  group_by(wave) %>% 
  select(inc_heat_perceived_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6     5
# 7 wave7   432

# Perceived increase in overall costs as a result of carbon pricing
panel %>% 
  filter(is.na(inc_overall_perceived_num)) %>%
  group_by(wave) %>% 
  select(inc_overall_perceived_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave7   469

inc_overall_perceived_num_imputed <- full_join(panel %>%
                                                 filter(is.na(inc_overall_perceived_num)) %>%
                                                 select(-inc_overall_perceived_num) %>%
                                                 filter(responseid %in% wave7IDs) %>%
                                                 filter(wave == "wave7"),
                                               panel %>%
                                                 filter(responseid %in% wave7IDs) %>%
                                                 filter(wave == "wave6") %>%
                                                 select(responseid, inc_overall_perceived_num),
                                               by = c("responseid"))
table(inc_overall_perceived_num_imputed$inc_overall_perceived_num, inc_overall_perceived_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$inc_overall_perceived_num)), "inc_overall_perceived_num"] <- inc_overall_perceived_num_imputed[which(inc_overall_perceived_num_imputed$responseid %in% wave7IDs & 
                                                                                                                                 inc_overall_perceived_num_imputed$wave == "wave7"), "inc_overall_perceived_num"] 
panel %>% 
  filter(is.na(inc_overall_perceived_num)) %>%
  group_by(wave) %>% 
  select(inc_overall_perceived_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave7   323

# Perceived increase in gasoline costs as a result of carbon pricing
panel %>% 
  filter(is.na(inc_gas_perceived_num)) %>%
  group_by(wave) %>% 
  select(inc_gas_perceived_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6     5
# 7 wave7   323

inc_gas_perceived_num_imputed <- full_join(panel %>%
                                             filter(is.na(inc_gas_perceived_num)) %>%
                                             select(-inc_gas_perceived_num) %>%
                                             filter(responseid %in% wave7IDs) %>%
                                             filter(wave == "wave7"),
                                           panel %>%
                                             filter(responseid %in% wave7IDs) %>%
                                             filter(wave == "wave6") %>%
                                             select(responseid, inc_gas_perceived_num),
                                           by = c("responseid"))
table(inc_gas_perceived_num_imputed$inc_gas_perceived_num, inc_gas_perceived_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$inc_gas_perceived_num)), "inc_gas_perceived_num"] <- inc_gas_perceived_num_imputed[which(inc_gas_perceived_num_imputed$responseid %in% wave7IDs & 
                                                                                                                     inc_gas_perceived_num_imputed$wave == "wave7"), "inc_gas_perceived_num"] 
panel %>% 
  filter(is.na(inc_gas_perceived_num)) %>%
  group_by(wave) %>% 
  select(inc_gas_perceived_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6     5
# 7 wave7   228

# Check that the number of NAs in wave 7 decreased
miss7 %>%
  filter(variable == "inc_heat_perceived_num" |
           variable == "inc_overall_perceived_num" |
           variable == "inc_gas_perceived_num")
panel %>%
  filter(wave == "wave7") %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss)) %>%
  filter(variable == "inc_heat_perceived_num" |
           variable == "inc_overall_perceived_num" |
           variable == "inc_gas_perceived_num")
# variable                  n_miss pct_miss
# <chr>                      <int>    <dbl>
# 1 inc_heat_perceived_num       432     42.9
# 2 inc_overall_perceived_num    323     32.0
# 3 inc_gas_perceived_num        228     22.6


# .. Impute missing data on bills from wave 6 ####
miss7 %>%
  filter(variable == "bill_heatingoil_num" |
           variable == "bill_natgas_num" |
           variable == "bill_elec_num" |
           variable == "bill_diesel_num")
# variable            n_miss pct_miss
# <chr>                <int>    <dbl>
# 1 bill_heatingoil_num   1008    100  
# 2 bill_natgas_num        478     47.4
# 3 bill_elec_num          141     14.0
# 4 bill_diesel_num        111     11.0

# Monthly heating oil bills
panel %>% 
  filter(is.na(bill_heatingoil_num)) %>%
  group_by(wave) %>% 
  select(bill_heatingoil_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6   904
# 7 wave7  1008

bill_heatingoil_num_imputed <- full_join(panel %>%
                                           filter(is.na(bill_heatingoil_num)) %>%
                                           select(-bill_heatingoil_num) %>%
                                           filter(responseid %in% wave7IDs) %>%
                                           filter(wave == "wave7"),
                                         panel %>%
                                           filter(responseid %in% wave7IDs) %>%
                                           filter(wave == "wave6") %>%
                                           select(responseid, bill_heatingoil_num),
                                         by = c("responseid"))
table(bill_heatingoil_num_imputed$bill_heatingoil_num, bill_heatingoil_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$bill_heatingoil_num)), "bill_heatingoil_num"] <- bill_heatingoil_num_imputed[which(bill_heatingoil_num_imputed$responseid %in% wave7IDs & 
                                                                                                               bill_heatingoil_num_imputed$wave == "wave7"), "bill_heatingoil_num"] 
panel %>% 
  filter(is.na(bill_heatingoil_num)) %>%
  group_by(wave) %>% 
  select(bill_heatingoil_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6   904
# 7 wave7  1004

# Monthly natural gas bills
panel %>% 
  filter(is.na(bill_natgas_num)) %>%
  group_by(wave) %>% 
  select(bill_natgas_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6   439
# 7 wave7   478

bill_natgas_num_imputed <- full_join(panel %>%
                                       filter(is.na(bill_natgas_num)) %>%
                                       select(-bill_natgas_num) %>%
                                       filter(responseid %in% wave7IDs) %>%
                                       filter(wave == "wave7"),
                                     panel %>%
                                       filter(responseid %in% wave7IDs) %>%
                                       filter(wave == "wave6") %>%
                                       select(responseid, bill_natgas_num),
                                     by = c("responseid"))
table(bill_natgas_num_imputed$bill_natgas_num, bill_natgas_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$bill_natgas_num)), "bill_natgas_num"] <- bill_natgas_num_imputed[which(bill_natgas_num_imputed$responseid %in% wave7IDs & 
                                                                                                   bill_natgas_num_imputed$wave == "wave7"), "bill_natgas_num"] 
panel %>% 
  filter(is.na(bill_natgas_num)) %>%
  group_by(wave) %>% 
  select(bill_natgas_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6   439
# 7 wave7   404

# Monthly electricity bills
panel %>% 
  filter(is.na(bill_elec_num)) %>%
  group_by(wave) %>% 
  select(bill_elec_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6   183
# 7 wave7   141

bill_elec_num_imputed <- full_join(panel %>%
                                     filter(is.na(bill_elec_num)) %>%
                                     select(-bill_elec_num) %>%
                                     filter(responseid %in% wave7IDs) %>%
                                     filter(wave == "wave7"),
                                   panel %>%
                                     filter(responseid %in% wave7IDs) %>%
                                     filter(wave == "wave6") %>%
                                     select(responseid, bill_elec_num),
                                   by = c("responseid"))
table(bill_elec_num_imputed$bill_elec_num, bill_elec_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$bill_elec_num)), "bill_elec_num"] <- bill_elec_num_imputed[which(bill_elec_num_imputed$responseid %in% wave7IDs & 
                                                                                             bill_elec_num_imputed$wave == "wave7"), "bill_elec_num"] 
panel %>% 
  filter(is.na(bill_elec_num)) %>%
  group_by(wave) %>% 
  select(bill_elec_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6   183
# 7 wave7   110

# Monthly diesel/gasoline bills
panel %>% 
  filter(is.na(bill_diesel_num)) %>%
  group_by(wave) %>% 
  select(bill_diesel_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6     5
# 7 wave7   111

bill_diesel_num_imputed <- full_join(panel %>%
                                       filter(is.na(bill_diesel_num)) %>%
                                       select(-bill_diesel_num) %>%
                                       filter(responseid %in% wave7IDs) %>%
                                       filter(wave == "wave7"),
                                     panel %>%
                                       filter(responseid %in% wave7IDs) %>%
                                       filter(wave == "wave6") %>%
                                       select(responseid, bill_diesel_num),
                                     by = c("responseid"))
table(bill_diesel_num_imputed$bill_diesel_num, bill_diesel_num_imputed$wave, useNA = "ifany")
panel[which(panel$responseid %in% wave7IDs & panel$wave == "wave7" &
              is.na(panel$bill_diesel_num)), "bill_diesel_num"] <- bill_diesel_num_imputed[which(bill_diesel_num_imputed$responseid %in% wave7IDs & 
                                                                                                   bill_diesel_num_imputed$wave == "wave7"), "bill_diesel_num"] 
panel %>% 
  filter(is.na(bill_diesel_num)) %>%
  group_by(wave) %>% 
  select(bill_diesel_num) %>% 
  tally()
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6     5
# 7 wave7    78

# Check that the number of NAs in wave 7 decreased
miss7 %>%
  filter(variable == "bill_heatingoil_num" |
           variable == "bill_natgas_num" |
           variable == "bill_elec_num" |
           variable == "bill_diesel_num")
panel %>%
  filter(wave == "wave7") %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss)) %>%
  filter(variable == "bill_heatingoil_num" |
           variable == "bill_natgas_num" |
           variable == "bill_elec_num" |
           variable == "bill_diesel_num")
# variable            n_miss pct_miss
# <chr>                <int>    <dbl>
# 1 bill_heatingoil_num   1004    99.6 
# 2 bill_natgas_num        404    40.1 
# 3 bill_elec_num          110    10.9 
# 4 bill_diesel_num         78     7.74


# .. Check coverage of data imputation procedure ####
miss7_postimputation <- panel %>%
  filter(wave == "wave7") %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

imput <- left_join(miss7 %>%
                     rename(n_miss_before = n_miss,
                            pct_miss_before = pct_miss),
                   miss7_postimputation %>%
                     rename(n_miss_after = n_miss,
                            pct_miss_after = pct_miss),
                   by = c("variable")) %>%
  mutate(n_miss_change = n_miss_before - n_miss_after,
         pct_miss_change = pct_miss_before - pct_miss_after) %>%
  arrange(desc(pct_miss_change)) %>%
  mutate(variable = as.factor(variable)) %>%
  mutate(variable = fct_reorder(variable,
                                pct_miss_change,
                                .desc = TRUE))

ggplot(data = imput %>%
         filter(pct_miss_change > 0) %>%
         select(var = variable, 
                pct_miss_before, pct_miss_after) %>%
         melt(id.vars = "var"),
       aes(x = var,
           y = value,
           fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(list = ls(pattern = "imput"))
rm(list = ls(pattern = "miss"))

save(panel, file = here("Data", "Processed", "panel_imputed.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# PCA                       ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "Processed", "panel_imputed.Rdata"))


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

# Extract loadings from PC1
PC1 <- pca$rotation[, 1]
sort(abs(PC1), decreasing = T)

# Extract loadings from PC2
PC2 <- pca$rotation[, 2]
sort(abs(PC2), decreasing = T)


# .. Biplots ####
g <- ggbiplot(pca,
              groups = sample_pca$cp_support,
              ellipse = TRUE,
              alpha = 0.5,
              varname.size = 8) +
  scale_color_manual(name = "Carbon pricing",
                     labels = c("Oppose",
                                "Support"),
                     values = c("#00B0F6", "#FFD84D")) +
  labs(title = "Principal components of support for carbon pricing") +
  theme(legend.text = element_text(size = 20))
g
ggsave(g,
       file = here("Figures", "biplot_support_12.png"),
       width = 6, height = 4, units = "in")

g <- ggbiplot(pca,
              groups = sample_pca$cp_oppose,
              ellipse = TRUE,
              alpha = 0.5,
              varname.size = 8) +
  scale_color_manual(name = "Carbon pricing",
                     labels = c("Support",
                                "Oppose"),
                     values = c("#FFD84D", "#00B0F6")) +
  labs(title = "Principal components of opposition to carbon pricing") +
  theme(legend.text = element_text(size = 20))
g
ggsave(g,
       file = here("Figures", "biplot_oppose_12.png"),
       width = 6, height = 4, units = "in")

ggbiplot(pca,
         groups = sample_pca$cp_strongsupport,
         ellipse = TRUE,
         alpha = 0.5)
ggbiplot(pca,
         groups = sample_pca$cp_strongoppose,
         ellipse = TRUE,
         alpha = 0.5)
ggbiplot(pca,
         groups = sample_pca$party_9,
         ellipse = TRUE,
         alpha = 0.5)

# Plot other PCs
ggbiplot(pca,
         choices = c(1, 3),
         groups = sample_pca$cp_oppose,
         ellipse = TRUE,
         alpha = 0.5)
ggbiplot(pca,
         choices = c(3, 4),
         groups = sample_pca$cp_oppose,
         ellipse = TRUE,
         alpha = 0.5)


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



## ## ## ## ## ## ## ## ## ## ##
# REGRESSIONS               ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "Processed", "panel_imputed.Rdata"))

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
          out = here("Results", "pricing_base.txt"))

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
          keep.stat = c("n", "adj.rsq", "f", "AIC"))
# Choose fit0a - work with cp_oppose


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
          out = here("Results", "pricing_perceived.txt"))
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
          out = here("Results", "pricing_actual.txt"))
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
          out = here("Results", "pricing_actual_interactions.txt"))
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
          out = here("Results", "pricing_full_model.txt"))
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
          keep.stat = c("n", "adj.rsq"))


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



## ## ## ## ## ## ## ## ## ## ##
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
          out = here("Results", "perceived_gas_costs.txt"))
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
          out = here("Results", "perceived_heating_costs.txt"))


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
          out = here("Results", "perceived_overall_costs.txt"))

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
          keep.stat = c("n", "adj.rsq"))



## ## ## ## ## ## ## ## ## ## ##
# CLASSIFICATION TREES      ####
## ## ## ## ## ## ## ## ## ## ##

# .. Prep the data ####
vars <- c(
  "cp_oppose",
  "bachelors",
  "income_num_mid",
  "rural",
  # "left_right_num",
  "conservative",
  # "inc_heat_perceived_num",
  "inc_gas_perceived_num",
  "inc_overall_perceived_num",
  "gasprice_change_perceived_num",
  "owner",
  # "home_size_num",
  "fossil_home",
  "fossil_water",
  "fossil_stove",
  "drive",
  "vehicle_num",
  # "km_driven_num",
  "bill_diesel_num",
  "bill_elec_num"
)

# Subset data to obtain complete cases for the variables in the model
sample_complete <- sample %>%
  select(all_of(vars)) %>%
  drop_na

# Data manip
sample_complete <- sample_complete %>%
  mutate_at(vars(bachelors, rural, conservative,
                 owner, fossil_home, fossil_water, fossil_stove,
                 drive),
            ~bin_to_num(.))

levels(sample_complete$cp_oppose) <- c("Support", "Oppose")


# .. Randomly split data into training and test sets ####
set.seed(1509)
test.indices <- sample(1:nrow(sample_complete), round(nrow(sample_complete)*0.2))
sample.train <- sample_complete[-test.indices, ]
sample.test <- sample_complete[test.indices, ]


# .. Use the tree package ####
# Set tree controls
tree.opts <- tree.control(nrow(sample.train), 
                          minsize = 5, 
                          mindev = 1e-5)

# Grow the tree
set.seed(1509)
cp_oppose.tree <- tree(cp_oppose ~ . -cp_oppose,
                       data = sample.train, 
                       control = tree.opts)

# Perform cost-complexity pruning
cp_oppose.tree.10 <- prune.tree(cp_oppose.tree, 
                                best = 10)

# Draw tree with tree package
# dev.off()
draw.tree(cp_oppose.tree.10, 
          nodeinfo = TRUE,
          print.levels = TRUE,
          cex = 0.7)
jpeg(here("Figures", "classification_tree.jpg"))
dev.off()


# .. Simulate 1,000 trees to obtain most important variable ####
# Set up records to collect top variable
nsims <- 1000
records <- matrix(NA, ncol = 6, nrow = nsims)
colnames(records) <- c("simulation", "variable", 
                       "2nd var", "misclass", "used", "size")
indices <- matrix(NA, ncol = nsims, nrow = nrow(sample.test))

set.seed(1509)
i <- 1
for (i in 1:nsims){
  test.indices <- sample(1:nrow(sample_complete), round(nrow(sample_complete)*0.2))
  sample.train <- sample_complete[-test.indices, ]
  sample.test <- sample_complete[test.indices, ]
  indices[, i] <- test.indices
  
  tree <- NULL
  tree <- tree(cp_oppose ~ . -cp_oppose,
               data = sample.train, 
               control = tree.opts)
  tree.10 <- prune.tree(tree, 
                        best = 10)
  records[i, 1] <- i
  records[i, 2] <- as.character(tree.10$frame[1, "var"])
  records[i, 3] <- as.character(tree.10$frame[4, "var"])
  records[i, 4] <- summary(tree.10)$misclass[1] / summary(tree.10)$misclass[2]
  records[i, 5] <- length(summary(tree.10)$used)
  records[i, 6] <- summary(tree.10)$size
}

# Simulation with the lowest misclassification rate
sim <- which(records[, "misclass"] == min(records[, "misclass"]))
ind <- indices[, sim]
sample.train <- sample_complete[-ind, ]
sample.test <- sample_complete[ind, ]
tree.opts <- tree.control(nrow(sample.train), 
                          minsize = 5, 
                          mindev = 1e-5)
cp_oppose.tree <- tree(cp_oppose ~ . -cp_oppose,
                       data = sample.train, 
                       control = tree.opts)
cp_oppose.tree.10 <- prune.tree(cp_oppose.tree, 
                                best = 10)
draw.tree(cp_oppose.tree.10, 
          nodeinfo = TRUE,
          print.levels = TRUE,
          cex = 0.5)

# .. Random forest approach ####
# Opposition to carbon pricing
set.seed(1509)
cp_oppose.rf <- randomForest(cp_oppose ~ . -cp_oppose,
                             data = sample_complete, 
                             importance = TRUE,
                             ntree = 1000)
cp_oppose.rf
cp_oppose.varimp <- varImp(cp_oppose.rf)
cp_oppose.varimp$var <- rownames(cp_oppose.varimp)
varImpPlot(cp_oppose.rf, cex = 0.7)

# Support for carbon pricing
vars <- vars[-which(vars == "cp_oppose" | vars == "conservative")]
vars <- c(vars, c("cp_support", "liberal"))

sample_complete <- sample %>%
  select(all_of(vars)) %>%
  drop_na

set.seed(1509)
cp_support.rf <- randomForest(cp_support ~ . -cp_support,
                              data = sample_complete, 
                              importance = TRUE,
                              ntree = 1000)
cp_support.rf
cp_support.varimp <- varImp(cp_support.rf)
cp_support.varimp$var <- rownames(cp_support.varimp)
varImpPlot(cp_support.rf, cex = 0.7)


# .. Create variable importance plot ####
# Oppose
g <- ggplot(cp_oppose.varimp %>%
              select(var, Oppose),
            aes(x = fct_reorder(var, Oppose), y = Oppose)) +
  geom_segment(aes(xend = var, y = 0, yend = Oppose), color = "#00B0F6") +
  geom_point(size = 4, color = "#00B0F6") +
  theme(axis.text = element_text(size = 20)) +
  coord_flip() +
  labs(title = "Variable importance",
       subtitle = "Predicting opposition to carbon pricing",
       x = "", y = "Mean decrease in accuracy")
ggsave(g,
       file = here("Figures", "varimp_oppose.png"),
       width = 6, height = 5, units = "in")

# Support
g <- ggplot(cp_support.varimp %>%
              rename(Oppose = 1) %>%
              select(var, Oppose),
            aes(x = fct_reorder(var, Oppose), y = Oppose)) +
  geom_segment(aes(xend = var, y = 0, yend = Oppose), color = "#FFD84D") +
  geom_point(size = 4, color = "#FFD84D") +
  theme(axis.text = element_text(size = 20)) +
  coord_flip() +
  labs(title = "Variable importance",
       subtitle = "Predicting support for carbon pricing",
       x = "", y = "Mean decrease in accuracy")
ggsave(g,
       file = here("Figures", "varimp_support.png"),
       width = 6, height = 5, units = "in")

