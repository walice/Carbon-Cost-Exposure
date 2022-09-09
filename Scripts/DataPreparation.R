# Cost Exposure paper
# Data Preparation
# Alice Lepissier
# alice_lepissier@brown.edu

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import Data
# .. Import data from survey instruments
# ID & Treatment Variables
# .. Province names
# .. Month index
# .. Federal carbon tax
# .. Define post-treatment periods
# Demographic Variables
# .. Gender
# .. Age
# .. Language
# .. Education
# Household Variables
# .. Number of children
# .. Annual household income
# .. Number of vehicles
# .. Urban or rural residence
# Partisanship Variables
# .. Left/right spectrum
# .. Party preference
# .. Consistency in party preference



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(foreign)
library(here)
library(readstata13)
library(reshape2)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT DATA               ####
## ## ## ## ## ## ## ## ## ## ##

# .. Import data from survey instruments ####
wave1 <- read.dta13(here("Data", "Raw", "Geo-coded/T1.dta")) %>%
  mutate(wave = "wave1") %>%
  melt(id.vars = c("responseid", "wave"))
wave2 <- read.dta13(here("Data", "Raw", "Geo-coded/T2.dta")) %>%
  mutate(wave = "wave2") %>%
  melt(id.vars = c("responseid", "wave"))
wave3 <- read.dta13(here("Data", "Raw", "Geo-coded/T3.dta")) %>%
  mutate(wave = "wave3") %>%
  melt(id.vars = c("responseid", "wave"))
wave4 <- read.dta13(here("Data", "Raw", "523-018D-T4 (complets cleaned).dta")) %>%
  mutate(wave = "wave4") %>%
  melt(id.vars = c("responseid", "wave"))
wave5 <- read.dta13(here("Data", "Raw", "T5.dta")) %>%
  mutate(wave = "wave5") %>%
  melt(id.vars = c("responseid", "wave"))
wave6 <- read.spss(here("Data", "Raw", "MW14523_032A_T6 (V2).sav"),
                   to.data.frame = TRUE, use.value.labels = TRUE) %>%
  mutate(wave = "wave6") %>%
  melt(id.vars = c("responseid", "wave"))
wave7 <- read.spss(here("Data", "Raw", "MW14523_032B_CLIENT.sav"),
                   to.data.frame = TRUE, use.value.labels = TRUE) %>%
  mutate(wave = "wave7") %>%
  rename(responseid = record) %>%
  melt(id.vars = c("responseid", "wave"))

panel <- full_join(wave1, wave2,
                   by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave3,
            by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave4,
            by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave5,
            by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave6,
            by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave7,
            by = c("responseid", "wave", "variable", "value"))

panel <- dcast(panel, responseid + wave ~ variable)
nrow(panel)
# 11782

# save(panel, file = here("Data", "Processed", "panel_raw.Rdata"))
rm(wave1, wave2, wave3, wave4, wave5, wave6, wave7)

load(here("Data", "Processed", "panel_raw.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# ID & TREATMENT VARIABLES  ####
## ## ## ## ## ## ## ## ## ## ##

# .. Province names ####
panel <- panel %>%
  mutate(prov = as.factor(prov))

summary(panel$prov)
# 1    2    3    4    5     AB   BC   ON   QC   SK  NA's 
# 1503 1496 1501 1501 1513  279  293  290  294  284 2828 

panel$prov <- recode_factor(panel$prov,
                            "1" = "AB",
                            "2" = "BC",
                            "3" = "ON",
                            "4" = "QC",
                            "5" = "SK")
summary(panel$prov)
# AB   BC   ON   QC   SK   NA's 
# 1782 1789 1791 1795 1797 2828 

# Look for variables with province names
names(panel)[str_detect(colnames(panel), fixed("prov", ignore_case = TRUE))]
# "prov"   "PROV_5" "PROV_6" "PROV"

# Fill out missing provinces from wave 5
panel$prov[which(is.na(panel$prov))] <- panel$PROV_5[which(is.na(panel$prov))]
summary(panel$prov)
# AB   BC   ON   QC   SK   NA's 
# 1958 1989 1984 1964 1958 1929

# Fill out missing provinces from wave 6
panel %>%
  select(PROV_6) %>%
  mutate_all(as.factor) %>%
  summary
# PROV_6     
# Alberta         :  196  
# British Columbia:  207  
# Nova Scotia     :    1  
# Ontario         :  202  
# Quebec          :  208  
# Saskatchewan    :  107  
# NA's            :10861  

panel$PROV_6 <- recode_factor(panel$PROV_6,
                              "Alberta" = "AB",
                              "British Columbia" = "BC",
                              "Ontario" = "ON",
                              "Quebec" = "QC",
                              "Saskatchewan" = "SK")

panel$PROV_6[which(panel$PROV_6 == "Nova Scotia")] <- NA
panel$PROV_6 <- droplevels(panel$PROV_6)
summary(panel$PROV_6)
# AB    BC    ON    QC    SK  NA's 
# 196   207   202   208   107 10862 

panel$prov[which(is.na(panel$prov))] <- panel$PROV_6[which(is.na(panel$prov))]
summary(panel$prov)
# AB   BC   ON   QC   SK NA's 
# 2154 2196 2186 2172 2065 1009 

# Fill out missing provinces from wave 7
panel %>%
  filter(is.na(PROV)) %>%
  select(wave) %>%
  mutate(wave = as.factor(wave)) %>%
  summary
# wave     
# wave1:3313  
# wave2:2441  
# wave3:1760  
# wave4:1440  
# wave5: 899  
# wave6: 921  
# All the missing data in PROV is for other waves --> PROV corresponds to wave 7

panel %>%
  select(PROV) %>%
  mutate_all(as.factor) %>%
  summary
# PROV      
# Alberta         :  205  
# British Columbia:  201  
# Ontario         :  202  
# Quebec          :  200  
# Saskatchewan    :  200  
# NA's            :10774 

panel$PROV <- recode_factor(panel$PROV,
                            "Alberta" = "AB",
                            "British Columbia" = "BC",
                            "Ontario" = "ON",
                            "Quebec" = "QC",
                            "Saskatchewan" = "SK")
summary(panel$PROV)
# AB    BC    ON    QC    SK  NA's 
# 205   201   202   200   200 10774 

panel$prov[which(is.na(panel$prov))] <- panel$PROV[which(is.na(panel$prov))]
summary(panel$prov)
# AB   BC   ON   QC   SK      NA's 
# 2359 2397 2388 2372 2265    1

# Drop the Nova Scotia obs
panel <- panel %>%
  filter(!is.na(prov))
summary(panel$prov)
# AB   BC   ON   QC   SK 
# 2359 2397 2388 2372 2265

panel <- panel %>%
  select(-c("PROV_5", "PROV_6", "PROV"))


# .. Month index ####
panel %>%
  filter(is.na(wave)) %>%
  nrow
# 0

panel <- panel %>%
  mutate(monthindex = case_when(wave == "wave1" ~ 1, # February 2019
                                wave == "wave2" ~ 3, # April 2019
                                wave == "wave3" ~ 6, # July 2019
                                wave == "wave4" ~ 10, # November 2019
                                wave == "wave5" ~ 16, # May 2020 ### CHECK
                                wave == "wave6" ~ 33, # October 2021 ### CHECK
                                wave == "wave7" ~ 43)) # August 2022 ### CHECK

panel %>% 
  select(wave, monthindex) %>%
  mutate(wave = as.factor(wave),
         monthindex = as.factor(monthindex)) %>%
  summary
# wave      monthindex
# wave1:3313   1 :3313   
# wave2:2441   3 :2441   
# wave3:1760   6 :1760   
# wave4:1440   10:1440   
# wave5: 899   16: 899   
# wave6: 920   33: 920   
# wave7:1008   43:1008   


# .. Federal carbon tax ####
panel <- panel %>%
  mutate(fedprice = case_when(!prov %in% c("ON", "SK") ~ 0,
                              prov %in% c("ON", "SK") ~ 1))
table(panel$prov, panel$fedprice)
#       0    1
# AB 2359    0
# BC 2397    0
# ON    0 2388
# QC 2372    0
# SK    0 2265
  

# .. Define post-treatment periods ####
panel <- panel %>%
  mutate(postperiod_1_2 = case_when(wave == "wave1" ~ 0,
                                    wave == "wave2" ~ 1),
         postperiod_2_3 = case_when(wave == "wave2" ~ 0,
                                    wave == "wave3" ~ 1),
         postperiod_2_4 = case_when(wave == "wave2" ~ 0,
                                    wave == "wave4" ~ 1),
         postperiod_3_4 = case_when(wave == "wave3" ~ 0,
                                    wave == "wave4" ~ 1),
         postperiod_1_5 = case_when(wave == "wave1" ~ 0,
                                    wave == "wave5" ~ 1))

table(panel$wave, panel$postperiod_1_2, useNA = "ifany")
#          0    1 <NA>
# wave1 3313    0    0
# wave2    0 2441    0
# wave3    0    0 1760
# wave4    0    0 1440
# wave5    0    0  899
# wave6    0    0  920
# wave7    0    0 1008



## ## ## ## ## ## ## ## ## ## ##
# DEMOGRAPHIC VARIABLES     ####
## ## ## ## ## ## ## ## ## ## ##

# .. Gender ####
# Look for variables with gender responses
names(panel)[str_detect(colnames(panel), fixed("d1", ignore_case = TRUE))]
# d1, D1_5, D1_6, D1_7

panel %>%
  select(wave, d1) %>%
  table
# d1
# wave    Female Male Other
# wave1   1690 1615     8
# wave2   1258 1177     6
# wave3    875  883     2
# wave4    715  724     1
# wave5      0    0     0
# wave6      0    0     0
# wave7      0    0     0

panel$female_bin <- recode_factor(panel$d1,
                                  "Male" = "0",
                                  "Female" = "1")
summary(panel$female_bin)
# 0     1    Other  NA's 
# 4399  4538    17  2827 

# Fill out missing gender from wave 5
panel$D1_5 <- recode_factor(panel$D1_5,
                            "Male" = "0",
                            "Female" = "1")
panel$female_bin[which(is.na(panel$female_bin))] <- panel$D1_5[which(is.na(panel$female_bin))]
summary(panel$female_bin)
# 0     1    Other  NA's 
# 4827  5007    19  1928

# Fill out missing gender from wave 6
panel$D1_6 <- recode_factor(panel$D1_6,
                            "Male" = "0",
                            "Female" = "1")
panel$female_bin[which(is.na(panel$female_bin))] <- panel$D1_6[which(is.na(panel$female_bin))]
summary(panel$female_bin)
# 0     1    Other  NA's 
# 5275  5478    20  1008 

# Fill out missing gender from wave 7
panel$D1_7 <- recode_factor(panel$D1_7,
                            "Male" = "0",
                            "Female" = "1")
panel$female_bin[which(is.na(panel$female_bin))] <- panel$D1_7[which(is.na(panel$female_bin))]
summary(panel$female_bin)
# 0     1       Other 
# 5757  6001    23


# .. Age ####
# Look for variables with age responses
names(panel)[str_detect(colnames(panel), fixed("d3", ignore_case = TRUE))]
# d3 D3_5 D3_6 D3_7

sort(unique(panel$d3))
# [1] "100 or older" "18"           "19"           "20"           "21"           "22"          
# [7] "23"           "24"           "25"           "26"           "27"           "28"          
# [13] "29"           "30"           "31"           "32"           "33"           "34"          
# [19] "35"           "36"           "37"           "38"           "39"           "40"          
# [25] "41"           "42"           "43"           "44"           "45"           "46"          
# [31] "47"           "48"           "49"           "50"           "51"           "52"          
# [37] "53"           "54"           "55"           "56"           "57"           "58"          
# [43] "59"           "60"           "61"           "62"           "63"           "64"          
# [49] "65"           "66"           "67"           "68"           "69"           "70"          
# [55] "71"           "72"           "73"           "74"           "75"           "76"          
# [61] "77"           "78"           "79"           "80"           "81"           "82"          
# [67] "83"           "84"           "85"           "86"           "87"           "88"          
# [73] "89"           "90"           "93"           "96"  

panel %>% 
  select(wave, d3) %>% 
  filter(is.na(d3)) %>%
  mutate(wave = as.factor(wave),
         d3 = as.factor(d3)) %>%
  summary
# wave        age      
# wave5: 899  NA's:2827  
# wave6: 920              
# wave7:1008

panel$age <- panel$d3

# Fill out missing age from wave 5
panel$age[which(is.na(panel$age))] <- panel$D3_5[which(is.na(panel$age))]
panel %>% 
  select(wave, age) %>% 
  filter(is.na(age)) %>%
  mutate(wave = as.factor(wave),
         age = as.factor(age)) %>%
  summary
# wave        age      
# wave6: 920  NA's:1928  
# wave7:1008    

# Fill out missing age from wave 6
panel$age[which(is.na(panel$age))] <- panel$D3_6[which(is.na(panel$age))]
panel %>% 
  select(wave, age) %>% 
  filter(is.na(age)) %>%
  mutate(wave = as.factor(wave),
         age = as.factor(age)) %>%
  summary
# wave        age      
# wave7:1008  NA's:1008    

# Fill out missing age from wave 7
panel$age[which(is.na(panel$age))] <- panel$D3_7[which(is.na(panel$age))]
panel %>% 
  filter(is.na(age)) %>%
  nrow
# 0

# Create trichotomous age variable
panel <- panel %>%
  mutate(age_tri = case_when(age > 17 & age < 35 ~ "18-34",
                             age > 34 & age < 55 ~ "35-54",
                             age > 54 ~ "55 and older")) %>%
  mutate(age_tri = as.factor(age_tri))
summary(panel$age_tri)
# 18-34        35-54 55 and older         NA's 
#  2621         4334         4824            2 


# .. Language ####
# Look for variables with language responses
names(panel)[str_detect(colnames(panel), fixed("d2", ignore_case = TRUE))]
# [1] "d2_96_other" "d2_1"        "d2_2"        "d2_96"       "Id2"         "d2"          "D2_6"       
# [8] "D2_6r96oe"
names(panel)[str_detect(colnames(panel), fixed("lang", ignore_case = TRUE))]
# [1] "lang"   "LANG_5" "QLANG"

table(panel$wave, panel$lang, useNA = "ifany")
#          1    2   EN   FR <NA>
# wave1 2755  558    0    0    0
# wave2 2030  411    0    0    0
# wave3 1458  302    0    0    0
# wave4    0    0 1185  255    0
# wave5    0    0    0    0  899
# wave6    0    0    0    0  920
# wave7    0    0    0    0 1008

table(panel$wave, panel$d2, useNA = "ifany")
#       English French Other (please specify) <NA>
# wave1       0      0                      0 3313
# wave2    1891    473                     77    0
# wave3    1372    328                     60    0
# wave4    1137    266                     37    0
# wave5       0      0                      0  899
# wave6       0      0                      0  920
# wave7       0      0                      0 1008

table(panel$wave, panel$LANG_5, useNA = "ifany")
#         EN   FR <NA>
# wave1    0    0 3313
# wave2    0    0 2441
# wave3    0    0 1760
# wave4    0    0 1440
# wave5  752  147    0
# wave6    0    0  920
# wave7    0    0 1008

table(panel$wave, panel$D2_6, useNA = "ifany")
#       English French Other (please specify) <NA>
# wave1       0      0                      0 3313
# wave2       0      0                      0 2441
# wave3       0      0                      0 1760
# wave4       0      0                      0 1440
# wave5       0      0                      0  899
# wave6     706    189                     25    0
# wave7       0      0                      0 1008

panel <- panel %>%
  mutate(language = as.factor(d2))

# Fill out missing language from wave 1
table(panel$wave, panel$d2_1, useNA = "ifany")
#          0    1 <NA>
# wave1  104  649 2560
# wave2    0    0 2441
# wave3    0    0 1760
# wave4    0    0 1440
# wave5    0    0  899
# wave6    0    0  920
# wave7    0    0 1008

table(panel$wave, panel$d2_2, useNA = "ifany")
#          0    1 <NA>
# wave1  102 2565  646
# wave2    0    0 2441
# wave3    0    0 1760
# wave4    0    0 1440
# wave5    0    0  899
# wave6    0    0  920
# wave7    0    0 1008

panel$language[!is.na(panel$d2_1)] <- "French"
panel$language[!is.na(panel$d2_2)] <- "English"
panel$language[!is.na(panel$d2_96)] <- "Other (please specify)"

table(panel$wave, panel$language, useNA = "ifany")
#       English French Other (please specify) <NA>
# wave1    2560    646                    107    0
# wave2    1891    473                     77    0
# wave3    1372    328                     60    0
# wave4    1137    266                     37    0
# wave5     752    147                      0    0
# wave6       0      0                      0  920
# wave7       0      0                      0 1008

# Fill out missing language from wave 5
panel$LANG_5 <- recode_factor(panel$LANG_5,
                              "EN" = "English",
                              "FR" = "French")
panel$language[which(is.na(panel$language))] <- panel$LANG_5[which(is.na(panel$language))]
summary(panel$language)

table(panel$wave, panel$language, useNA = "ifany")
#       English French Other (please specify) <NA>
# wave1    2560    646                    107    0
# wave2    1891    473                     77    0
# wave3    1372    328                     60    0
# wave4    1137    266                     37    0
# wave5     752    147                      0    0
# wave6       0      0                      0  920
# wave7       0      0                      0 1008

# Fill out missing language from wave 6
panel$language[which(is.na(panel$language))] <- panel$D2_6[which(is.na(panel$language))]

table(panel$wave, panel$language, useNA = "ifany")
#       English French Other (please specify) <NA>
# wave1    2560    646                    107    0
# wave2    1891    473                     77    0
# wave3    1372    328                     60    0
# wave4    1137    266                     37    0
# wave5     752    147                      0    0
# wave6     706    189                     25    0
# wave7       0      0                      0 1008

# Create dichotomous language variable
panel <- panel %>%
  mutate(french_bin = case_when(language == "French" ~ 1,
                                language != "French" ~ 0)) %>%
  mutate(french_bin = as.factor(french_bin))
table(panel$wave, panel$french_bin, useNA = "ifany")
#          0    1 <NA>
# wave1 2667  646    0
# wave2 1968  473    0
# wave3 1432  328    0
# wave4 1174  266    0
# wave5  752  147    0
# wave6  731  189    0
# wave7    0    0 1008


# .. Education ####
# Look for variables with education responses
names(panel)[str_detect(colnames(panel), fixed("d12", ignore_case = TRUE))]
# [1] "d12"   "D12_6" "D12_7"

panel %>%
  filter(!complete.cases(d12)) %>%
  select(wave) %>%
  mutate(wave = as.factor(wave)) %>%
  summary
# wave     
# wave2:2189  
# wave5: 899  
# wave6: 920  
# wave7:1008 

# Where is education data for wave 2?
panel %>% 
  filter(wave == "wave2") %>% 
  select(d12) %>% 
  mutate(d12 = as.factor(d12)) %>% 
  summary
# d12      
# Undergraduate university degree:  67  
# College / CEGEP graduate       :  45  
# High school graduate           :  43  
# Post-graduate university degree:  31  
# Some university                :  28  
# (Other)                        :  38  
# NA's                           :2189  

# Where is education data for wave 5?
panel %>% 
  filter(wave == "wave5") %>% 
  select(d12) %>% 
  mutate(d12 = as.factor(d12)) %>% 
  summary
# d12     
# NA's:899

panel %>% 
  mutate(d12 = as.factor(d12)) %>% 
  select(d12) %>% 
  summary
# d12      
# Undergraduate university degree:1715  
# College / CEGEP graduate       :1511  
# High school graduate           :1219  
# Post-graduate university degree: 686  
# Some university                : 676  
# (Other)                        : 958  
# NA's                           :5016 

panel$education <- panel$d12

# Fill out missing education from wave 6
panel %>%
  filter(complete.cases(D12_6)) %>%
  select(wave) %>%
  table
# wave6 
# 920 
panel$education[which(is.na(panel$education))] <- panel$D12_6[which(is.na(panel$education))]

# Fill out missing education from wave 7
panel %>%
  filter(complete.cases(D12_7)) %>%
  select(wave) %>%
  table
# wave7 
# 1008 
panel$education[which(is.na(panel$education))] <- panel$D12_7[which(is.na(panel$education))]

table(panel$wave, panel$education, useNA = "ifany")

# Create dichotomous variable indicating whether respondent has a Bachelors
bachelors_yes <- c("Undergraduate university degree",
                   "Post-graduate university degree")

bachelors_no <- c("Less than high school",
                  "High school graduate",
                  "Less than college/some CEGEP",
                  "College / CEGEP graduate",
                  "Apprenticeship",
                  "Some university")

panel <- panel %>%
  mutate(bachelors_bin = case_when(education %in% bachelors_yes ~ 1,
                                   education %in% bachelors_no ~ 0)) %>%
  mutate(bachelors_bin = as.factor(bachelors_bin))
summary(panel$bachelors_bin)
# 0    1 NA's 
# 5527 3166 3088 
rm(bachelors_yes, bachelors_no)

# Create education variable with 5 levels
panel <- panel %>%
  mutate(edu_5 = case_when(education == "Less than high school" ~ "Less than high school",
                           education == "High school graduate" ~ "High school",
                           education == "Less than college/some CEGEP" ~ "Some college",
                           education == "College / CEGEP graduate" ~ "Some college",
                           education == "Apprenticeship" ~ "Some college",
                           education == "Some university" ~ "Some college",
                           education == "Undergraduate university degree" ~ "College",
                           education == "Post-graduate university degree" ~ "Graduate or prof. degree")) %>%
  mutate(edu_5 = as.factor(edu_5))
summary(panel$edu_5)
# College  Graduate or prof. degree  High school  Less than high school 
#    2288                       878         1533                    261 
# Some college  NA's 
#         3733  3088 



## ## ## ## ## ## ## ## ## ## ##
# HOUSEHOLD VARIABLES       ####
## ## ## ## ## ## ## ## ## ## ##

# .. Number of children ####
# Look for variables with gender responses
names(panel)[str_detect(colnames(panel), fixed("d7", ignore_case = TRUE))]
# [1] "d7"   "D7_6" "D7"   "D7_7"

names(panel)[str_detect(colnames(panel), fixed("child", ignore_case = TRUE))]
# [1] "children"   "Children_6" "Children_7"

table(panel$wave, panel$d7, useNA = "ifany")
#          0    1    2    3    4    5    6 7 or more <NA>
# wave1 2365  412  386  104   32   10    2         2    0
# wave2  181   35   24    8    3    0    0         1 2189
# wave3    0    0    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0    0    0         0  899
# wave6    0    0    0    0    0    0    0         0  920
# wave7    0    0    0    0    0    0    0         0 1008

table(panel$wave, panel$children, useNA = "ifany")
#          0    1    2    3    4    5    6 <NA>
# wave1    0    0    0    0    0    0    0 3313
# wave2    0    0    0    0    0    0    0 2441
# wave3 1309  206  166   57   16    5    1    0
# wave4 1068  155  154   46   13    3    1    0
# wave5    0    0    0    0    0    0    0  899
# wave6    0    0    0    0    0    0    0  920
# wave7    0    0    0    0    0    0    0 1008

table(panel$wave, panel$D7_6, useNA = "ifany")
#          0    1    2    3    4    5 7 or more <NA>
# wave1    0    0    0    0    0    0         0 3313
# wave2    0    0    0    0    0    0         0 2441
# wave3    0    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0    0         0  899
# wave6  729   82   72   25    8    3         1    0
# wave7    0    0    0    0    0    0         0 1008

table(panel$wave, panel$D7_7, useNA = "ifany")
#          0    1    2    3    4    5 7 or more <NA>
# wave1    0    0    0    0    0    0         0 3313
# wave2    0    0    0    0    0    0         0 2441
# wave3    0    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0    0         0  899
# wave6    0    0    0    0    0    0         0  920
# wave7  756  124   80   33   10    4         1    0

table(panel$wave, panel$Children_6, useNA = "ifany")
#          0    1    2    3    4    5 <NA>
# wave1    0    0    0    0    0    0 3313
# wave2    0    0    0    0    0    0 2441
# wave3    0    0    0    0    0    0 1760
# wave4    0    0    0    0    0    0 1440
# wave5    0    0    0    0    0    0  899
# wave6  702   76   79   28    8    4   23
# wave7    0    0    0    0    0    0 1008

table(panel$wave, panel$Children_7, useNA = "ifany")
#          0    1    2    3    4    5 7 or more <NA>
# wave1    0    0    0    0    0    0         0 3313
# wave2    0    0    0    0    0    0         0 2441
# wave3    0    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0    0         0  899
# wave6    0    0    0    0    0    0         0  920
# wave7  716  127   81   29   13    3         1   38

# Fill out number of children from waves 1 and 2
panel$numchildren <- as.numeric(panel$d7)

# Fill out number of children from waves 3 and 4
panel$numchildren[which(is.na(panel$numchildren))] <- panel$children[which(is.na(panel$numchildren))]

# Where is the data for number of children in wave 5?

# Fill out number of children from wave 6
sum(panel$D7_6 == panel$Children_6, na.rm = TRUE)
# [1] 847
length(panel$D7_6[!is.na(panel$D7_6)])
# [1] 920
length(panel$Children_6[!is.na(panel$Children_6)])
# [1] 897
# Not sure what is happening here

# Use the more complete variable
panel$numchildren[which(is.na(panel$numchildren))] <- panel$D7_6[which(is.na(panel$numchildren))]
table(panel$wave, panel$numchildren, useNA = "ifany")

# Fill out number of children from wave 7
sum(panel$D7_7 == panel$Children_7, na.rm = TRUE)
# [1] 853
length(panel$D7_7[!is.na(panel$D7_7)])
# [1] 1008
length(panel$Children_7[!is.na(panel$Children_7)])
# [1] 970
# Not sure what is happening here

# Use the more complete variable
panel$numchildren[which(is.na(panel$numchildren))] <- panel$D7_7[which(is.na(panel$numchildren))]
table(panel$wave, panel$numchildren, useNA = "ifany")
#          0    1    2    3    4    5    6 7 or more <NA>
# wave1 2365  412  386  104   32   10    2         0    2
# wave2  181   35   24    8    3    0    0         0 2190
# wave3 1309  206  166   57   16    5    1         0    0
# wave4 1068  155  154   46   13    3    1         0    0
# wave5    0    0    0    0    0    0    0         0  899
# wave6  729   82   72   25    8    3    0         1    0
# wave7  756  124   80   33   10    4    0         1    0


# .. Annual household income ####
# Look for variables with gender responses
names(panel)[str_detect(colnames(panel), fixed("d14", ignore_case = TRUE))]
# [1] "d14"   "D14_6" "D14_7"

panel %>%
  filter(is.na(d14)) %>%
  select(wave) %>%
  mutate(wave = as.factor(wave)) %>%
  summary
# wave     
# wave2:2189  
# wave5: 899  
# wave6: 920  
# wave7:1008  

# Where is income data for wave 2?
panel %>% 
  filter(wave == "wave2") %>% 
  select(d14) %>% 
  mutate(d14 = as.factor(d14)) %>% 
  summary
# d14      
# $20,000-$39,999     :  40  
# $40,000-$59,999     :  38  
# $60,000-$79,999     :  34  
# $80,000-$99,999     :  33  
# Prefer not to answer:  33  
# (Other)             :  74  
# NA's                :2189

# Where is income data for wave 5?
panel %>% 
  filter(wave == "wave5") %>% 
  select(d14) %>% 
  mutate(d14 = as.factor(d14)) %>% 
  summary
# d14     
# NA's:899

panel %>% 
  mutate(d14 = as.factor(d14)) %>% 
  select(d14) %>% 
  summary
#                       d14      
# $40,000-$59,999     :1086  
# $20,000-$39,999     : 930  
# $80,000-$99,999     : 925  
# $60,000-$79,999     : 915  
# Prefer not to answer: 829  
# (Other)             :2080  
# NA's                :5016  

panel$income <- panel$d14

# Fill out missing income from wave 6
panel %>%
  filter(complete.cases(D14_6)) %>%
  select(wave) %>%
  table
# wave6 
# 920 
panel$income[which(is.na(panel$income))] <- panel$D14_6[which(is.na(panel$income))]

# Fill out missing income from wave 7
panel %>%
  filter(complete.cases(D14_7)) %>%
  select(wave) %>%
  table
# wave7 
# 1008 
panel$income[which(is.na(panel$income))] <- panel$D14_7[which(is.na(panel$income))]

table(panel$wave, panel$income, useNA = "ifany")

# Create numeric income variable which takes the mid-point of each income bracket
panel <- panel %>%
  mutate(income_num = case_when(income == "Under $20,000" ~ 15000,
                                income == "$20,000-$39,999" ~ 30000,
                                income == "$40,000-$59,999" ~ 50000,
                                income == "$60,000-$79,999" ~ 70000,
                                income == "$80,000-$99,999" ~ 90000,
                                income == "$100,000-$119,999" ~ 110000,
                                income == "$120,000-$159, 999" ~ 140000,
                                income == "$160,000-$199,999" ~ 180000,
                                income == "$200,000 or more" ~ 250000))

table(panel$wave, panel$income_num, useNA = "ifany")
#       15000 30000 50000 70000 90000 110000 140000 180000 250000 <NA>
# wave1   273   442   516   439   464    296    272    103    107  401
# wave2    14    40    38    34    33     20     23      5     12 2222
# wave3   123   241   290   241   231    158    146     53     52  225
# wave4    94   207   242   201   197    118    129     44     38  170
# wave5     0     0     0     0     0      0      0      0      0  899
# wave6    44   146   138   144   110     75     92     38     31  102
# wave7    64   123   129   144   148    106    107     53     46   88

# Create household income variable with 6 levels
panel <- panel %>%
  mutate(income_6 = case_when(income == "Under $20,000" ~ "Less than 20,000",
                              income == "$20,000-$39,999" ~ "20,000-40,000",
                              income == "$40,000-$59,999" ~ "40,000-60,000",
                              income == "$60,000-$79,999" ~ "60,000-80,000",
                              income == "$80,000-$99,999" ~ "80,000-100,000",
                              income == "$100,000-$119,999" ~ "100,000 and over",
                              income == "$120,000-$159, 999" ~ "100,000 and over",
                              income == "$160,000-$199,999" ~ "100,000 and over",
                              income == "$200,000 or more" ~ "100,000 and over")) %>%
  mutate(income_6 = as.factor(income_6))
summary(panel$income_6)
# 100,000 and over    20,000-40,000    40,000-60,000    60,000-80,000   80,000-100,000 Less than 20,000 
#             2124             1199             1353             1203             1183              612 
# NA's 
# 4107


# .. Number of vehicles ####
# Look for variables with vehicle responses
names(panel)[str_detect(colnames(panel), fixed("d8", ignore_case = TRUE))]
# [1] "d8"   "D8_6" "D8_7"

table(panel$wave, panel$d8, useNA = "ifany")
#          0    1    2    3    4 5 or more <NA>
# wave1  343 1360 1197  296   72        45    0
# wave2   24  109   93   17    5         4 2189
# wave3    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0         0  899
# wave6    0    0    0    0    0         0  920
# wave7    0    0    0    0    0         0 1008

table(panel$wave, panel$D8_6, useNA = "ifany")
#          0    1    2    3    4 5 or more <NA>
# wave1    0    0    0    0    0         0 3313
# wave2    0    0    0    0    0         0 2441
# wave3    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0         0  899
# wave6  113  426  305   54   14         8    0
# wave7    0    0    0    0    0         0 1008

table(panel$wave, panel$D8_7, useNA = "ifany")
#          0    1    2    3    4 5 or more <NA>
# wave1    0    0    0    0    0         0 3313
# wave2    0    0    0    0    0         0 2441
# wave3    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0         0  899
# wave6    0    0    0    0    0         0  920
# wave7  109  430  327   93   36        13    0

panel$numvehicle <- panel$d8

# Fill out missing vehicles from wave 6
panel$numvehicle[which(is.na(panel$numvehicle))] <- panel$D8_6[which(is.na(panel$numvehicle))]

# Fill out missing vehicles from wave 7
panel$numvehicle[which(is.na(panel$numvehicle))] <- panel$D8_7[which(is.na(panel$numvehicle))]

table(panel$wave, panel$numvehicle, useNA = "ifany")
#          0    1    2    3    4 5 or more <NA>
# wave1  343 1360 1197  296   72        45    0
# wave2   24  109   93   17    5         4 2189
# wave3    0    0    0    0    0         0 1760
# wave4    0    0    0    0    0         0 1440
# wave5    0    0    0    0    0         0  899
# wave6  113  426  305   54   14         8    0
# wave7  109  430  327   93   36        13    0


# .. Urban or rural residence ####
# Look for variables with urban/rural responses
names(panel)[str_detect(colnames(panel), fixed("d10", ignore_case = TRUE))]
# [1] "d10"   "D10_6" "D10_7"

names(panel)[str_detect(colnames(panel), fixed("urban", ignore_case = TRUE))]
# [1] "urbanrural"

table(panel$urbanrural, panel$wave, useNA = "ifany")
#                              wave1 wave2 wave3 wave4 wave5 wave6 wave7
# large urban (100,000+)        1916   164   153     0     0     0     0
# medium urban (30,000-99,999)   308    18    15     0     0     0     0
# rural                          236    17    19     0     0     0     0
# small urban (1000-29,999)      348    19    23     0     0     0     0
# unknown                        288    20    28     0     0     0     0
# <NA>                           217  2203  1522  1440   899   920  1008

table(panel$d10, panel$wave, useNA = "ifany")
#                                           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# In a remote area                             21    17     0     0     0     0     0
# In a rural area                             301   221     0     0     0     0     0
# In a small town                             362   241     0     0     0     0     0
# In a smaller, regional city                 643   444     0     0     0     0     0
# Not sure                                     17     5     0     0     0     0     0
# Within a large city                        1288   987     0     0     0     0     0
# Within a suburb, adjacent to a large city   681   526     0     0     0     0     0
# <NA>                                          0     0  1760  1440   899   920  1008

table(panel$D10_6, panel$wave, useNA = "ifany")
#                                           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# In a remote area                              0     0     0     0     0     8     0
# In a rural area                               0     0     0     0     0    83     0
# In a small town                               0     0     0     0     0    71     0
# In a smaller, regional city                   0     0     0     0     0   162     0
# Not sure                                      0     0     0     0     0     2     0
# Within a large city                           0     0     0     0     0   388     0
# Within a suburb, adjacent to a large city     0     0     0     0     0   206     0
# <NA>                                       3313  2441  1760  1440   899     0  1008

table(panel$D10_7, panel$wave, useNA = "ifany")
#                                           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# In a remote area                              0     0     0     0     0     0     5
# In a rural area                               0     0     0     0     0     0   111
# In a small town                               0     0     0     0     0     0   117
# In a smaller, regional city                   0     0     0     0     0     0   155
# Not sure                                      0     0     0     0     0     0     1
# Within a large city                           0     0     0     0     0     0   408
# Within a suburb, adjacent to a large city     0     0     0     0     0     0   211
# <NA>                                       3313  2441  1760  1440   899   920     0

panel$urban <- panel$d10

# Fill out missing urban/rural from wave 6
panel$urban[which(is.na(panel$urban))] <- panel$D10_6[which(is.na(panel$urban))]

# Fill out missing urban/rural from wave 7
panel$urban[which(is.na(panel$urban))] <- panel$D10_7[which(is.na(panel$urban))]

table(panel$urban, panel$wave, useNA = "ifany")
#                                           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# In a remote area                             21    17     0     0     0     8     5
# In a rural area                             301   221     0     0     0    83   111
# In a small town                             362   241     0     0     0    71   117
# In a smaller, regional city                 643   444     0     0     0   162   155
# Not sure                                     17     5     0     0     0     2     1
# Within a large city                        1288   987     0     0     0   388   408
# Within a suburb, adjacent to a large city   681   526     0     0     0   206   211
# <NA>                                          0     0  1760  1440   899     0     0

# Create dichotomous rural variable
rural_yes <- c("In a remote area",
               "In a rural area",
               "In a small town")

rural_no <- c("In a smaller, regional city",
              "Within a large city",
              "Within a suburb, adjacent to a large city")

panel <- panel %>%
  mutate(rural_bin = case_when(urban %in% rural_yes ~ 1,
                               urban %in% rural_no ~ 0)) %>%
  mutate(rural_bin = as.factor(rural_bin))
summary(panel$rural_bin)
# 0       1 NA's 
# 6099 1558 4124
rm(rural_yes, rural_no)



## ## ## ## ## ## ## ## ## ## ##
# PARTISANSHIP VARIABLES    ####
## ## ## ## ## ## ## ## ## ## ##

# .. Left/right spectrum ####
# Look for variables with left/right responses
names(panel)[str_detect(colnames(panel), fixed("d11", ignore_case = TRUE))]
# [1] "d11"   "D11_6" "D11_7"

table(panel$d11, panel$wave, useNA = "ifany")
#                    wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 1                     73     2     0     0     0     0     0
# 2                    152     7     0     0     0     0     0
# 3                    256    30     0     0     0     0     0
# 4                    260    20     0     0     0     0     0
# 6                    234    17     0     0     0     0     0
# 7                    270    22     0     0     0     0     0
# 8                    197    19     0     0     0     0     0
# 9                     93     7     0     0     0     0     0
# Centre5             1090    90     0     0     0     0     0
# Far to the left0      58    10     0     0     0     0     0
# Far to the right10    91     4     0     0     0     0     0
# Not sure             539     0     0     0     0     0     0
# <NA>                   0  2213  1760  1440   899   920  1008

table(panel$D11_6, panel$wave, useNA = "ifany")
table(panel$D11_7, panel$wave, useNA = "ifany")

panel$left_right <- panel$d11

# Fill out missing left/right from wave 6
panel$left_right[which(is.na(panel$left_right))] <- panel$D11_6[which(is.na(panel$left_right))]

# Fill out missing left/right from wave 7
panel$left_right[which(is.na(panel$left_right))] <- panel$D11_7[which(is.na(panel$left_right))]

table(panel$left_right, panel$wave, useNA = "ifany")
#                    wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0                      0     0     0     0     0    12    23
# 1                     73     2     0     0     0    17    17
# 10                     0     0     0     0     0    12    37
# 2                    152     7     0     0     0    60    63
# 3                    256    30     0     0     0    85    93
# 4                    260    20     0     0     0    77    68
# 5                      0     0     0     0     0   263   279
# 6                    234    17     0     0     0    81    86
# 7                    270    22     0     0     0    90    89
# 8                    197    19     0     0     0    55    91
# 9                     93     7     0     0     0    18    19
# 99                     0     0     0     0     0   150   143
# Centre5             1090    90     0     0     0     0     0
# Far to the left0      58    10     0     0     0     0     0
# Far to the right10    91     4     0     0     0     0     0
# Not sure             539     0     0     0     0     0     0
# <NA>                   0  2213  1760  1440   899     0     0

panel <- panel %>%
  mutate(left_right_num = case_when(left_right == "Far to the left0" ~ 0,
                                    left_right == "1" ~ 0.1,
                                    left_right == "2" ~ 0.2,
                                    left_right == "3" ~ 0.3,
                                    left_right == "4" ~ 0.4,
                                    left_right == "Centre5" ~ 0.5,
                                    left_right == "6" ~ 0.6,
                                    left_right == "7" ~ 0.7,
                                    left_right == "8" ~ 0.8,
                                    left_right == "9" ~ 0.9,
                                    left_right == "Far to the right10" ~ 0.1,)) %>%
  mutate(left_right_num = as.numeric(left_right_num))
summary(panel$left_right_num)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.300   0.500   0.491   0.600   0.900    7770 


# .. Party preference ####
# Look for variables with party preference responses
names(panel)[str_detect(colnames(panel), fixed("v3", ignore_case = TRUE))]
# [1] "v3"        "dv3"       "V3_6"      "V3_6r96oe" "V3_7"      "V3_7r96oe" "V3_7R0"    "V3_7R1"

names(panel)[str_detect(colnames(panel), fixed("vote", ignore_case = TRUE))]
# [1] "votexab"   "vote1"     "vote2"     "vote3"     "vote4_1"   "vote4_2"   "vote5_1"   "vote5_2"  
# [9] "vote6_1"   "vote6_2"   "vote6_3"   "vote6_4"   "vote6_5"   "vote6_6"   "vote6ref"  "VOTE_6r1" 
# [17] "VOTE_6r2"  "VOTE_6r3"  "VOTE_6r4"  "VOTE_6r5"  "VOTE_6r6"  "VOTE_7r1"  "VOTE_7r3"  "VOTE_7r4" 
# [25] "VOTE_7r5"  "VOTE_7r6"  "VOTE_7r8"  "VOTE_7r9"  "VOTE_7r10" "VOTE_ON_7" "VOTE_QC_7"

# Relevant variables are v3, V3_6, V3_7, vote2, vote4_1, vote4_2, vote5_1, vote5_2

table(panel$v3, panel$wave, useNA = "ifany")
#                    wave1 wave2 wave3 wave4 wave5 wave6 wave7
# Bloc Québécois       115    91    63     0     0     0     0
# Conservative Party  1130   879   640     0     0     0     0
# Green Party          290   238   174     0     0     0     0
# I would not vote     525   328   241     0     0     0     0
# Liberal Party        667   485   386     0     0     0     0
# ndp                  460   359   216     0     0     0     0
# People's Party       126    61    40     0     0     0     0
# <NA>                   0     0     0  1440   899   920  1008

panel$party <- panel$v3

# Fill out missing party preference from wave 4
table(panel$vote2, panel$wave, useNA = "ifany")
panel$party[which(is.na(panel$party))] <- panel$vote2[which(is.na(panel$party))]

# Fill out missing party preference from wave 6
panel$party[which(is.na(panel$party))] <- panel$V3_6[which(is.na(panel$party))]

# Fill out missing party preference from wave 7
panel$party[which(is.na(panel$party))] <- panel$V3_7[which(is.na(panel$party))]

# Implement consistent naming for parties
panel$party[which(panel$party == "Conservatives")] <- "Conservative Party"
panel$party[which(panel$party == "Liberals")] <- "Liberal Party"
panel$party[which(panel$party == "ndp")] <- "NDP"
panel$party[which(panel$party == "Other (please specify)")] <- "Other party"
panel$party[which(panel$party == "People's Party of Canada")] <- "People's Party"
panel$party[which(panel$party == "I don't know / undecided")] <- "Don't know"

table(panel$party, panel$wave, useNA = "ifany")
#                    wave1 wave2 wave3 wave4 wave5 wave6 wave7
# Bloc Québécois       115    91    63    92     0    61    55
# Conservative Party  1130   879   640   459     0   251   278
# Did not vote           0     0     0     3     0     0     0
# Don't know             0     0     0    47     0    41     0
# Green Party          290   238   174    61     0    28    40
# I would not vote     525   328   241     0     0    82   131
# Liberal Party        667   485   386   307     0   255   211
# NDP                  460   359   216   214     0   168   196
# None                   0     0     0     9     0     0     0
# Other party            0     0     0    17     0    14    59
# People's Party       126    61    40    25     0    20    38
# <NA>                   0     0     0   206   899     0     0


# .. Consistency in party preference ####

panel_wide <- panel %>%
  select(responseid, wave, party) %>%
  spread(wave, party)

# Create indicator for party preferences between waves 1 and 4
panel_wide <- panel_wide %>%
  mutate(votertype_1_4 = case_when(wave1 == "Conservative Party" & wave4 == "Conservative Party" ~ "Consistent Convervatives",
                                   wave1 == "Liberal Party" & wave4 == "Liberal Party" ~ "Consistent Liberals",
                                   wave1 == "NDP" & wave4 == "NDP" ~ "Consistent NDP",
                                   wave1 == "Green Party" & wave4 == "Green Party" ~ "Consistent Greens",
                                   wave1 == "Bloc Québécois" & wave4 == "Bloc Québécois" ~ "Consistent Bloc",
                                   wave1 == "Liberal Party" & wave4 != "Liberal Party" ~ "Liberals to Other",
                                   wave1 != "Liberal Party" & wave4 == "Liberal Party" ~ "Other to Liberals"))

# Create indicator for party preferences between waves 3 and 4
panel_wide <- panel_wide %>%
  mutate(votertype_3_4 = case_when(wave3 == "Conservative Party" & wave4 == "Conservative Party" ~ "Consistent Convervatives",
                                   wave3 == "Liberal Party" & wave4 == "Liberal Party" ~ "Consistent Liberals",
                                   wave3 == "NDP" & wave4 == "NDP" ~ "Consistent NDP",
                                   wave3 == "Green Party" & wave4 == "Green Party" ~ "Consistent Greens",
                                   wave3 == "Bloc Québécois" & wave4 == "Bloc Québécois" ~ "Consistent Bloc",
                                   wave3 == "Liberal Party" & wave4 != "Liberal Party" ~ "Liberals to Other",
                                   wave3 != "Liberal Party" & wave4 == "Liberal Party" ~ "Other to Liberals"))

# Create indicator for party preferences between waves 6 and 7
panel_wide <- panel_wide %>%
  mutate(votertype_6_7 = case_when(wave6 == "Conservative Party" & wave7 == "Conservative Party" ~ "Consistent Convervatives",
                                   wave6 == "Liberal Party" & wave7 == "Liberal Party" ~ "Consistent Liberals",
                                   wave6 == "NDP" & wave7 == "NDP" ~ "Consistent NDP",
                                   wave6 == "Green Party" & wave7 == "Green Party" ~ "Consistent Greens",
                                   wave6 == "Bloc Québécois" & wave7 == "Bloc Québécois" ~ "Consistent Bloc",
                                   wave6 == "Liberal Party" & wave7 != "Liberal Party" ~ "Liberals to Other",
                                   wave6 != "Liberal Party" & wave7 == "Liberal Party" ~ "Other to Liberals"))

# Create indicator for party preferences between waves 1 and 7
panel_wide <- panel_wide %>%
  mutate(votertype_1_7 = case_when(wave1 == "Conservative Party" & wave7 == "Conservative Party" ~ "Consistent Convervatives",
                                   wave1 == "Liberal Party" & wave7 == "Liberal Party" ~ "Consistent Liberals",
                                   wave1 == "NDP" & wave7 == "NDP" ~ "Consistent NDP",
                                   wave1 == "Green Party" & wave7 == "Green Party" ~ "Consistent Greens",
                                   wave1 == "Bloc Québécois" & wave7 == "Bloc Québécois" ~ "Consistent Bloc",
                                   wave1 == "Liberal Party" & wave7 != "Liberal Party" ~ "Liberals to Other",
                                   wave1 != "Liberal Party" & wave7 == "Liberal Party" ~ "Other to Liberals"))

# Put it all together
panel <- panel %>%
  left_join(panel_wide %>%
              select(-c(starts_with("wave"))),
            by = c("responseid"))
rm(panel_wide)
