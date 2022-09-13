# Cost Exposure paper
# Exploratory Data Analysis
# Alice Lepissier
# alice_lepissier@brown.edu

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Codebook
# Functions
# Response Patterns
# .. Explore missing data
# .. Explore respondents who answered all waves
# .. Explore distribution of outcomes
# Exploratory Cross-Tabs
# .. Demographic variables
# .. Household variables
# .. Transportation variables
# .. Partisanship variables
# .. Perceptions
# .. Energy sources
# .. Energy bills



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(corrplot)
library(here)
library(GGally)
library(naniar)
library(reshape2)
library(tidyverse)



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
# RESPONSE PATTERNS         ####
## ## ## ## ## ## ## ## ## ## ##

# .. Explore missing data ####
panel %>%
  filter(is.na(responseid)) %>%
  nrow
# 0

panel %>%
  group_by(wave) %>%
  tally
# wave      n
# <fct> <int>
# 1 wave1  3313
# 2 wave2  2441
# 3 wave3  1760
# 4 wave4  1440
# 5 wave5   899
# 6 wave6   920
# 7 wave7  1008

miss <- panel %>%
  group_by(wave) %>%
  miss_var_summary() %>%
  arrange(desc(wave), desc(pct_miss))

bigmiss <- miss %>%
  filter(pct_miss > 50)

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% id.vars)
# 0

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% treatment.vars)
# 0 (since postperiod indicators are irrelevant here)

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% demographic.vars)
# wave  variable   n_miss pct_miss
# <fct> <chr>       <int>    <dbl>
# 1 wave7 language_3   1008      100
# 2 wave7 french       1008      100
# 3 wave7 female          0        0
# 4 wave7 age_3           0        0
# 5 wave7 edu_8           0        0
# 6 wave7 edu_5           0        0
# 7 wave7 bachelors       0        0
table(panel$language_3, panel$wave, useNA = "ifany")
# Data in all other waves

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% household.vars)
# wave  variable       n_miss pct_miss
# <fct> <chr>           <int>    <dbl>
# 1 wave7 home_size_num     361  35.8   
# 2 wave7 income_6           88   8.73  
# 3 wave7 income_num_mid     88   8.73  
# 4 wave7 rural               1   0.0992
# 5 wave7 numchildren_8       0   0     
# 6 wave7 income_10           0   0     
# 7 wave7 rural_7             0   0     
# 8 wave7 owner               0   0  
table(panel$home_size_num, panel$wave, useNA = "ifany")
# home_size_num was only asked in wave 7

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% transport.vars)
# The km_driven_* variables have ~ 55% missing
table(panel$km_driven_11, panel$wave, useNA = "ifany")
# But other waves don't have this data either

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% partisanship.vars)
# wave  variable       n_miss pct_miss
# <fct> <chr>           <int>    <dbl>
# 1 wave7 votertype6.7      853     84.6
# 2 wave7 votertype1.4      735     72.9
# 3 wave7 votertype3.4      733     72.7
# 4 wave7 votertype1.7      533     52.9
# 5 wave7 left_right_num    482     47.8
# 6 wave7 party_9             0      0
table(panel$left_right_num, panel$wave, useNA = "ifany")
# Has data for wave 6

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% opinions.vars)
# 0 missing

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% perceptions.vars)
# wave  variable                          n_miss pct_miss
# <fct> <chr>                              <int>    <dbl>
# 1 wave7 div_perceived_12                    1008    100  
# 2 wave7 gasprice_dec_perceived_7             913     90.6
# 3 wave7 inc_heat_perceived_num               596     59.1
# 4 wave7 inc_overall_perceived_num            469     46.5
# 5 wave7 inc_gas_perceived_num                323     32.0
# 6 wave7 gasprice_inc_perceived_7             238     23.6
# 7 wave7 inc_heat_perceived_6                   0      0  
# 8 wave7 inc_gas_perceived_6                    0      0  
# 9 wave7 gasprice_change_perceived_5            0      0  
# 10 wave7 gasprice_change_perceived_num          0      0  
# 11 wave7 gasprice_change_jan_perceived_num      0      0  
table(panel$gasprice_inc_perceived_7, panel$wave, useNA = "ifany")
# Data in waves 2, 3, and 7
table(panel$inc_heat_perceived_num, panel$wave, useNA = "ifany")
# Data in waves 6 and 7
table(panel$inc_overall_perceived_num, panel$wave, useNA = "ifany")
# Data in waves 6 and 7
table(panel$inc_gas_perceived_num, panel$wave, useNA = "ifany")
# Data in waves 6 and 7

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% energy.vars)
# 0 missing except for heating_water_9 which is ~ 80%
table(panel$heating_water_9, panel$wave, useNA = "ifany")
# Data in waves 6 and 7

miss %>%
  filter(wave == "wave7") %>%
  filter(variable %in% bill.vars)
# wave  variable                       n_miss pct_miss
# <fct> <chr>                           <int>    <dbl>
# 1 wave7 bill_heatingoil_num              1008    100  
# 2 wave7 bill_diesel_13                   1008    100  
# 3 wave7 bill_diesel_num_mid              1008    100  
# 4 wave7 bill_heatingoil_winter_num_mid    999     99.1
# 5 wave7 bill_heatingoil_winter_7          998     99.0
# 6 wave7 bill_natgas_num                   478     47.4
# 7 wave7 bill_natgas_summer_num_mid        477     47.3
# 8 wave7 bill_natgas_winter_num_mid        474     47.0
# 9 wave7 bill_natgas_winter_7              404     40.1
# 10 wave7 bill_natgas_summer_7              404     40.1
# 11 wave7 bill_elec_winter_8                141     14.0
# 12 wave7 bill_elec_summer_8                141     14.0
# 13 wave7 bill_elec_winter_num_mid          141     14.0
# 14 wave7 bill_elec_summer_num_mid          141     14.0
# 15 wave7 bill_elec_num                     141     14.0
# 16 wave7 bill_diesel_num                   111     11.0
# 17 wave7 familiar_bills_3                    0      0  
table(panel$bill_heatingoil_num, panel$wave, useNA = "ifany")
# Data in wave 6
table(panel$bill_natgas_num, panel$wave, useNA = "ifany")
# Data in wave 6
table(panel$bill_elec_num, panel$wave, useNA = "ifany")
# Data in waves 6 and 7
table(panel$bill_diesel_num, panel$wave, useNA = "ifany")
# Data in waves 6 and 7


# .. Explore respondents who answered all waves  ####
wave1IDs <- panel %>% filter(wave == "wave1") %>% pull(responseid)
wave2IDs <- panel %>% filter(wave == "wave2") %>% pull(responseid)
wave3IDs <- panel %>% filter(wave == "wave3") %>% pull(responseid)
wave4IDs <- panel %>% filter(wave == "wave4") %>% pull(responseid)
wave5IDs <- panel %>% filter(wave == "wave5") %>% pull(responseid)
wave6IDs <- panel %>% filter(wave == "wave6") %>% pull(responseid)
wave7IDs <- panel %>% filter(wave == "wave7") %>% pull(responseid)

wave7_complete <- panel %>% filter(responseid %in% wave7IDs)
table(wave7_complete$wave)
# wave1 wave2 wave3 wave4 wave5 wave6 wave7 
# 932   673   488   405   321   288  1008
# These are the people who answered wave 7 (including new ones)

wave1_complete <- panel %>% filter(responseid %in% wave1IDs)
table(wave1_complete$wave)
# wave1 wave2 wave3 wave4 wave5 wave6 wave7 
# 3313  2189  1509  1190   899   847   932
# These are the people who have answered wave 1

no_attrition <- panel %>%
  filter(responseid %in% wave1IDs) %>%
  filter(responseid %in% wave2IDs) %>%
  filter(responseid %in% wave3IDs) %>%
  filter(responseid %in% wave4IDs) %>%
  filter(responseid %in% wave5IDs) %>%
  filter(responseid %in% wave6IDs) %>%
  filter(responseid %in% wave7IDs)
table(no_attrition$wave)
# wave1 wave2 wave3 wave4 wave5 wave6 wave7 
# 208   208   208   208   208   208   208 
# These are the same people who have answered all waves since the beginning


# .. Explore distribution of outcomes ####
summary(panel$cp_support)
summary(panel$cp_oppose)
summary(panel$cp_strongsupport)
summary(panel$cp_strongoppose)
summary(panel$cp_opinion_5)
hist(panel$cp_fair_num)



## ## ## ## ## ## ## ## ## ## ##
# EXPLORATORY CROSS-TABS    ####
## ## ## ## ## ## ## ## ## ## ##

table(panel$edu_5, panel$cp_support)
table(panel$edu_5, panel$cp_oppose)

table(panel$female, panel$cp_support)
table(panel$female, panel$cp_oppose)

table(panel$age_3, panel$cp_support)
table(panel$age_3, panel$cp_oppose)

table(panel$income_6, panel$cp_support)
table(panel$rural, panel$cp_support)
table(panel$owner, panel$cp_support)

table(panel$drive, panel$cp_support)
table(panel$vehicle_num, panel$cp_support)
table(panel$km_driven_11, panel$cp_support)

table(panel$left_right_num, panel$cp_support)


# .. Demographic variables #####
# Education (5 levels)
viz <- panel %>%
  filter(!is.na(edu_5)) %>%
  group_by(edu_5) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(edu_5 = fct_relevel(edu_5,
                             "Less than high school",
                             "High school",
                             "Some college",
                             "College",
                             "Graduate or prof. degree")) %>%
  melt(id.vars = "edu_5")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = edu_5)) +
  geom_bar(position = "dodge", stat = "identity")

# Female
viz <- panel %>%
  filter(!is.na(female)) %>%
  group_by(female) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(female = ifelse(female == 1, "Female", "Male")) %>%
  melt(id.vars = "female")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = female)) +
  geom_bar(position = "dodge", stat = "identity")


# .. Household variables #####
# Income (6 levels)
viz <- panel %>%
  filter(!is.na(income_6)) %>%
  group_by(income_6) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(income_6 = fct_relevel(income_6,
                                "Less than $20,000",
                                "$20,000-$40,000",
                                "$40,000-$60,000",
                                "$60,000-$80,000",
                                "$80,000-$100,000",
                                "$100,000 and over")) %>%
  melt(id.vars = "income_6")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = income_6)) +
  geom_bar(position = "dodge", stat = "identity")

# Rural
viz <- panel %>%
  filter(!is.na(rural)) %>%
  group_by(rural) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(rural = ifelse(rural == 1, "Rural", "Urban")) %>%
  melt(id.vars = "rural")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = rural)) +
  geom_bar(position = "dodge", stat = "identity")

# Owner
viz <- panel %>%
  filter(!is.na(owner)) %>%
  group_by(owner) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(owner = ifelse(owner == 1, "Owns home", "Does not own")) %>%
  melt(id.vars = "owner")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = owner)) +
  geom_bar(position = "dodge", stat = "identity")

# Number of children
viz <- panel %>%
  filter(!is.na(numchildren_8)) %>%
  group_by(numchildren_8) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "numchildren_8")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = numchildren_8)) +
  geom_bar(position = "dodge", stat = "identity")

# Size of home
quart <- quantile(panel$home_size_num, na.rm = TRUE)
# 0%     25%     50%     75%    100% 
# 4     950    1300    1950 4004000 

viz <- panel %>%
  filter(!is.na(home_size_num)) %>%
  mutate(home_size_num = cut(home_size_num,
                             breaks = c(0, quart[2], quart[3], quart[4], quart[5]))) %>%
  group_by(home_size_num) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "home_size_num")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = home_size_num)) +
  geom_bar(position = "dodge", stat = "identity")


# .. Transportation variables #####
# Drive to work
viz <- panel %>%
  filter(!is.na(drive)) %>%
  group_by(drive) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(drive = ifelse(drive == 1, "Drives to work", "Does not drive to work")) %>%
  melt(id.vars = "drive")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = drive)) +
  geom_bar(position = "dodge", stat = "identity")

# Number of vehicles owned
viz <- panel %>%
  filter(!is.na(vehicle_num)) %>%
  mutate(vehicle_num = as.factor(vehicle_num)) %>%
  group_by(vehicle_num) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "vehicle_num")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = vehicle_num)) +
  geom_bar(position = "dodge", stat = "identity")

# Number of kilometres driven
quart <- quantile(panel$km_driven_num, na.rm = TRUE)
# 0%    25%    50%    75%   100% 
# 0   5000  10000  20000 500000

viz <- panel %>%
  filter(!is.na(km_driven_num)) %>%
  mutate(km_driven_num = cut(km_driven_num,
                             breaks = c(-1, quart[2], quart[3], quart[4], quart[5]))) %>%
  group_by(km_driven_num) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "km_driven_num")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = km_driven_num)) +
  geom_bar(position = "dodge", stat = "identity")

# Type of commute to work
viz <- panel %>%
  filter(!is.na(commute_7)) %>%
  group_by(commute_7) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "commute_7")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = commute_7)) +
  geom_bar(position = "dodge", stat = "identity")


# .. Partisanship variables ####
# Left/right spectrum
viz <- panel %>%
  filter(!is.na(left_right_num)) %>%
  group_by(left_right_num) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "left_right_num")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = left_right_num)) +
  scale_fill_continuous(breaks = c(0, 0.9),
                        labels = c("Left", "Right")) +
  geom_bar(position = "dodge", stat = "identity")

# Party preference
viz <- panel %>%
  filter(!is.na(party_9)) %>%
  group_by(party_9) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "party_9")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = party_9)) +
  geom_bar(position = "dodge", stat = "identity")


# .. Perceptions ####
# Perceived increase in heating costs as a result of carbon pricing
viz <- panel %>%
  filter(!is.na(inc_heat_perceived_6)) %>%
  group_by(inc_heat_perceived_6) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(inc_heat_perceived_6 = fct_relevel(inc_heat_perceived_6,
                                            "I don't know",
                                            "$0 per month",
                                            "$1-$24 per month",
                                            "$25-$49 per month",
                                            "$50-$99 per month",
                                            "$100 or more per month")) %>%
  melt(id.vars = "inc_heat_perceived_6")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = inc_heat_perceived_6)) +
  geom_bar(position = "dodge", stat = "identity")

# Perceived increase in gasoline costs as a result of carbon pricing
viz <- panel %>%
  filter(!is.na(inc_gas_perceived_6)) %>%
  group_by(inc_gas_perceived_6) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(inc_gas_perceived_6 = fct_relevel(inc_gas_perceived_6,
                                            "I don't know",
                                            "$0 per month",
                                            "$1-$24 per month",
                                            "$25-$49 per month",
                                            "$50-$99 per month",
                                            "$100 or more per month")) %>%
  melt(id.vars = "inc_gas_perceived_6")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = inc_gas_perceived_6)) +
  geom_bar(position = "dodge", stat = "identity")


# .. Perceived change in gas prices
viz <- panel %>%
  filter(!is.na(gasprice_change_perceived_5)) %>%
  group_by(gasprice_change_perceived_5) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(gasprice_change_perceived_5 = fct_relevel(gasprice_change_perceived_5,
                                                   "Not sure",
                                                   "Gotten less expensive",
                                                   "I don't use gasoline",
                                                   "Stayed about the same",
                                                   "Gotten more expensive")) %>%
  melt(id.vars = "gasprice_change_perceived_5")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = gasprice_change_perceived_5)) +
  geom_bar(position = "dodge", stat = "identity")

# Net perceived changes in gas prices over the last month
viz <- panel %>%
  filter(!is.na(gasprice_change_perceived_num)) %>%
  group_by(gasprice_change_perceived_num) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "gasprice_change_perceived_num")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = gasprice_change_perceived_num)) +
  geom_bar(position = "dodge", stat = "identity")


# .. Energy sources ####
# Energy source vs. type of heating
viz <- panel %>%
  filter(wave == "wave7") %>%
  select(fossil_home, fossil_water, fossil_stove,
         renewable_home, renewable_water, renewable_stove,
         cp_support, cp_oppose) %>%
  mutate(home = case_when(fossil_home == "1" ~ "Fossil",
                          renewable_home == "1" ~ "Renewable"),
         water = case_when(fossil_water == "1" ~ "Fossil",
                           renewable_water == "1" ~ "Renewable"),
         stove = case_when(fossil_stove == "1" ~ "Fossil",
                           renewable_stove == "1" ~ "Renewable"),
         pricing = case_when(cp_support == "1" ~ "Support",
                             cp_oppose == "1" ~ "Oppose")) %>%
  select(home, water, stove,
         pricing) %>%
  drop_na %>%
  pivot_longer(c(home, water, stove),
               names_to = "type",
               values_to = "source")

ggplot(data = viz) +
  geom_count(mapping = aes(x = type, y = source))

# Types of heating
ggplot(data = panel %>%
         filter(wave == "wave7") %>%
         select(heating_stove_5, heating_home_10)) +
  geom_count(mapping = aes(x = heating_stove_5, y = heating_home_10))

ggplot(data = panel %>%
         filter(wave == "wave7") %>%
         select(heating_stove_5, heating_water_9)) +
  geom_count(mapping = aes(x = heating_stove_5, y = heating_water_9))

# Stove heating vs. carbon pricing support
viz <- panel %>%
  filter(wave == "wave7") %>%
  mutate(pricing = case_when(cp_support == "1" ~ "Support",
                             cp_oppose == "1" ~ "Oppose")) %>%
  select(heating_stove_5, pricing) %>%
  drop_na

ggplot(data = viz) +
  geom_count(mapping = aes(x = pricing, y = heating_stove_5))
# This is wrong - needs to be proportions
# This just shows that most people have electric stoves


# .. Energy bills ####
# Familiarity with bills
viz <- panel %>%
  filter(!is.na(familiar_bills_3)) %>%
  group_by(familiar_bills_3) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "familiar_bills_3")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = familiar_bills_3)) +
  geom_bar(position = "dodge", stat = "identity")

# Monthly gasoline or diesel bills (categorical)
viz <- panel %>%
  filter(!is.na(bill_diesel_13)) %>%
  group_by(bill_diesel_13) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  mutate(bill_diesel_13 = fct_relevel(bill_diesel_13,
                                      "I don't know",
                                      "$0",
                                      "$0-$49",
                                      "$50-$99",
                                      "$100-$149",
                                      "$150-$199",
                                      "$200-$249",
                                      "$250-$299",
                                      "$300-$349",
                                      "$350-$399",
                                      "$400-$449",
                                      "$450-$499",
                                      "$500 or more")) %>%
  melt(id.vars = "bill_diesel_13")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = bill_diesel_13)) +
  geom_bar(position = "dodge", stat = "identity")

# Monthly gasoline or diesel bills (continuous, binned)
summary(panel$bill_diesel_num)
summary(panel$bill_diesel_num_mid)

quart <- quantile(panel$bill_diesel_num, na.rm = TRUE)
# 0%  25%  50%  75% 100% 
# 0   60  150  250 4000

viz <- panel %>%
  filter(!is.na(bill_diesel_num)) %>%
  mutate(bill_diesel_num = cut(bill_diesel_num,
                               breaks = c(-1, quart[2], quart[3], quart[4], quart[5]))) %>%
  group_by(bill_diesel_num) %>%
  summarize(cp_support = mean(bin_to_num(cp_support), 
                              na.rm = TRUE),
            cp_oppose = mean(bin_to_num(cp_oppose),
                             na.rm = TRUE),
            cp_strongsupport = mean(bin_to_num(cp_strongsupport), 
                                    na.rm = TRUE),
            cp_strongoppose = mean(bin_to_num(cp_strongoppose),
                                   na.rm = TRUE)) %>%
  melt(id.vars = "bill_diesel_num")

ggplot(data = viz,
       aes(x = variable,
           y = value,
           fill = bill_diesel_num)) +
  geom_bar(position = "dodge", stat = "identity")
