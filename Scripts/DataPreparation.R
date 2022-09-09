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
# Carbon Pricing Opinions
# .. Opinions on carbon pricing
# .. Support/opposition to carbon pricing
# .. Beliefs about fairness of carbon pricing
# Perceptions
# .. Perceived rebate amount estimate
# .. Perceived increase in heating costs as a result of carbon pricing
# .. Perceived increase in gasoline costs as a result of carbon pricing
# Codebook
# Export Panel



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(EnvStats)
library(foreign)
library(here)
library(Hmisc)
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

panel <- panel %>%
  mutate(wave = as.factor(wave))

save(panel, file = here("Data", "Processed", "panel_raw.Rdata"))
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
  summary
# wave     
# wave1:3313  
# wave2:2441  
# wave3:1760  
# wave4:1440  
# wave5: 899  
# wave6: 921  
# wave7:   0
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
  mutate(monthindex = as.factor(monthindex)) %>%
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
                              prov %in% c("ON", "SK") ~ 1)) %>%
  mutate(fedprice = as.factor(fedprice))
table(panel$prov, panel$fedprice)
#       0    1
# AB 2359    0
# BC 2397    0
# ON    0 2388
# QC 2372    0
# SK    0 2265
  

# .. Define post-treatment periods ####
panel <- panel %>%
  mutate(postperiod1.2 = case_when(wave == "wave1" ~ 0,
                                   wave == "wave2" ~ 1),
         postperiod2.3 = case_when(wave == "wave2" ~ 0,
                                   wave == "wave3" ~ 1),
         postperiod2.4 = case_when(wave == "wave2" ~ 0,
                                   wave == "wave4" ~ 1),
         postperiod3.4 = case_when(wave == "wave3" ~ 0,
                                   wave == "wave4" ~ 1),
         postperiod1.5 = case_when(wave == "wave1" ~ 0,
                                   wave == "wave5" ~ 1)) %>%
  mutate_at(vars(starts_with("postperiod")),
            list(~as.factor(.)))

table(panel$postperiod1.2, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0     3313     0     0     0     0     0     0
# 1        0  2441     0     0     0     0     0
# <NA>     0     0  1760  1440   899   920  1008



## ## ## ## ## ## ## ## ## ## ##
# DEMOGRAPHIC VARIABLES     ####
## ## ## ## ## ## ## ## ## ## ##

# .. Gender ####
# Look for variables with gender responses
names(panel)[str_detect(colnames(panel), fixed("d1", ignore_case = TRUE))]
# Relevant variables are d1, D1_5, D1_6, D1_7

panel %>%
  select(d1, wave) %>%
  table(useNA = "ifany")
#        wave
# d1     wave1 wave2 wave3 wave4 wave5 wave6 wave7
# Female  1690  1258   875   715     0     0     0
# Male    1615  1177   883   724     0     0     0
# Other      8     6     2     1     0     0     0
# <NA>       0     0     0     0   899   920  1008

panel$gender <- panel$d1

# Fill out missing gender from wave 5
panel$gender[which(is.na(panel$gender))] <- panel$D1_5[which(is.na(panel$gender))]

# Fill out missing gender from wave 6
panel$gender[which(is.na(panel$gender))] <- panel$D1_6[which(is.na(panel$gender))]

# Fill out missing gender from wave 7
panel$gender[which(is.na(panel$gender))] <- panel$D1_7[which(is.na(panel$gender))]

panel <- panel %>%
  mutate(female = case_when(gender == "Female" ~ 1,
                            gender == "Male" | gender == "Other" ~ 0)) %>%
  mutate(female = as.factor(female),
         gender = as.factor(gender))

summary(panel$female)
# 0    1 
# 5780 6001 


# .. Age ####
# Look for variables with age responses
names(panel)[str_detect(colnames(panel), fixed("d3", ignore_case = TRUE))]
# Relevant variables are d3 D3_5 D3_6 D3_7

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
  filter(is.na(d3)) %>%
  select(wave) %>%
  summary
# wave     
# wave1:   0  
# wave2:   0  
# wave3:   0  
# wave4:   0  
# wave5: 899  
# wave6: 920  
# wave7:1008 

panel$age <- panel$d3

# Fill out missing age from wave 5
panel$age[which(is.na(panel$age))] <- panel$D3_5[which(is.na(panel$age))]
panel %>% 
  filter(is.na(age)) %>%
  select(wave) %>% 
  summary
# wave     
# wave1:   0  
# wave2:   0  
# wave3:   0  
# wave4:   0  
# wave5:   0  
# wave6: 920  
# wave7:1008    

# Fill out missing age from wave 6
panel$age[which(is.na(panel$age))] <- panel$D3_6[which(is.na(panel$age))]
panel %>% 
  filter(is.na(age)) %>%
  select(wave) %>% 
  summary
# wave     
# wave1:   0  
# wave2:   0  
# wave3:   0  
# wave4:   0  
# wave5:   0  
# wave6:   0  
# wave7:1008     

# Fill out missing age from wave 7
panel$age[which(is.na(panel$age))] <- panel$D3_7[which(is.na(panel$age))]
panel %>% 
  filter(is.na(age)) %>%
  nrow
# 0

# Create trichotomous age variable
panel <- panel %>%
  mutate(age_3 = case_when(age > 17 & age < 35 ~ "18-34",
                           age > 34 & age < 55 ~ "35-54",
                           age > 54 ~ "55 and older")) %>%
  mutate(age_3 = as.factor(age_3))
summary(panel$age_3)
# 18-34        35-54 55 and older         NA's 
#  2621         4334         4824            2 


# .. Language ####
# Look for variables with language responses
names(panel)[str_detect(colnames(panel), fixed("d2", ignore_case = TRUE))]
# [1] "d2_96_other" "d2_1"        "d2_2"        "d2_96"       "Id2"         "d2"          "D2_6"       
# [8] "D2_6r96oe"
names(panel)[str_detect(colnames(panel), fixed("lang", ignore_case = TRUE))]
# [1] "lang"   "LANG_5" "QLANG"

table(panel$lang, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 1     2755  2030  1458     0     0     0     0
# 2      558   411   302     0     0     0     0
# EN       0     0     0  1185     0     0     0
# FR       0     0     0   255     0     0     0
# <NA>     0     0     0     0   899   920  1008

table(panel$d2, panel$wave, useNA = "ifany")
#                        wave1 wave2 wave3 wave4 wave5 wave6 wave7
# English                    0  1891  1372  1137     0     0     0
# French                     0   473   328   266     0     0     0
# Other (please specify)     0    77    60    37     0     0     0
# <NA>                    3313     0     0     0   899   920  1008

table(panel$LANG_5, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# EN       0     0     0     0   752     0     0
# FR       0     0     0     0   147     0     0
# <NA>  3313  2441  1760  1440     0   920  1008

table(panel$D2_6, panel$wave, useNA = "ifany")
#                        wave1 wave2 wave3 wave4 wave5 wave6 wave7
# English                    0     0     0     0     0   706     0
# French                     0     0     0     0     0   189     0
# Other (please specify)     0     0     0     0     0    25     0
# <NA>                    3313  2441  1760  1440   899     0  1008

panel <- panel %>%
  mutate(language_3 = as.factor(d2))

# Fill out missing language from wave 1
table(panel$d2_1, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0      104     0     0     0     0     0     0
# 1      649     0     0     0     0     0     0
# <NA>  2560  2441  1760  1440   899   920  1008

table(panel$d2_2, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0      102     0     0     0     0     0     0
# 1     2565     0     0     0     0     0     0
# <NA>   646  2441  1760  1440   899   920  1008

panel$language_3[!is.na(panel$d2_1)] <- "French"
panel$language_3[!is.na(panel$d2_2)] <- "English"
panel$language_3[!is.na(panel$d2_96)] <- "Other (please specify)"

table(panel$language_3, panel$wave, useNA = "ifany")
#                        wave1 wave2 wave3 wave4 wave5 wave6 wave7
# English                 2560  1891  1372  1137     0     0     0
# French                   646   473   328   266     0     0     0
# Other (please specify)   107    77    60    37     0     0     0
# <NA>                       0     0     0     0   899   920  1008

# Fill out missing language from wave 5
panel$LANG_5 <- recode_factor(panel$LANG_5,
                              "EN" = "English",
                              "FR" = "French")
panel$language_3[which(is.na(panel$language_3))] <- panel$LANG_5[which(is.na(panel$language_3))]

table(panel$language_3, panel$wave, useNA = "ifany")
#                        wave1 wave2 wave3 wave4 wave5 wave6 wave7
# English                 2560  1891  1372  1137   752     0     0
# French                   646   473   328   266   147     0     0
# Other (please specify)   107    77    60    37     0     0     0
# <NA>                       0     0     0     0     0   920  1008

# Fill out missing language from wave 6
panel$language_3[which(is.na(panel$language_3))] <- panel$D2_6[which(is.na(panel$language_3))]

table(panel$language_3, panel$wave, useNA = "ifany")
#                        wave1 wave2 wave3 wave4 wave5 wave6 wave7
# English                 2560  1891  1372  1137   752   706     0
# French                   646   473   328   266   147   189     0
# Other (please specify)   107    77    60    37     0    25     0
# <NA>                       0     0     0     0     0     0  1008

# Create dichotomous language variable
panel <- panel %>%
  mutate(french = case_when(language_3 == "French" ~ 1,
                            language_3 != "French" ~ 0)) %>%
  mutate(french = as.factor(french))
table(panel$french, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0     2667  1968  1432  1174   752   731     0
# 1      646   473   328   266   147   189     0
# <NA>     0     0     0     0     0     0  1008


# .. Education ####
# Look for variables with education responses
names(panel)[str_detect(colnames(panel), fixed("d12", ignore_case = TRUE))]
# [1] "d12"   "D12_6" "D12_7"

panel %>%
  filter(!complete.cases(d12)) %>%
  select(wave) %>%
  summary
# wave     
# wave1:   0  
# wave2:2189  
# wave3:   0  
# wave4:   0  
# wave5: 899  
# wave6: 920  
# wave7:1008 

table(panel$d12, panel$wave, useNA = "ifany")
#                                 wave1 wave2 wave3 wave4 wave5 wave6 wave7
# Apprenticeship                    155     9    79    58     0     0     0
# College / CEGEP graduate          735    45   396   335     0     0     0
# High school graduate              588    43   319   269     0     0     0
# Less than college/some CEGEP      221    26   111    89     0     0     0
# Less than high school             118     3    52    37     0     0     0
# Post-graduate university degree   337    31   181   137     0     0     0
# Some university                   342    28   171   135     0     0     0
# Undergraduate university degree   817    67   451   380     0     0     0
# <NA>                                0  2189     0     0   899   920  1008

# Where is education data for wave 2?
# Where is education data for wave 5?

panel$education <- panel$d12

# Fill out missing education from wave 6
panel %>%
  filter(complete.cases(D12_6)) %>%
  select(wave) %>%
  table
# wave1 wave2 wave3 wave4 wave5 wave6 wave7 
#     0     0     0     0     0   920     0 
panel$education[which(is.na(panel$education))] <- panel$D12_6[which(is.na(panel$education))]

# Fill out missing education from wave 7
panel %>%
  filter(complete.cases(D12_7)) %>%
  select(wave) %>%
  table
# wave1 wave2 wave3 wave4 wave5 wave6 wave7 
#     0     0     0     0     0     0  1008
panel$education[which(is.na(panel$education))] <- panel$D12_7[which(is.na(panel$education))]

table(panel$education, panel$wave, useNA = "ifany")
panel <- panel %>%
  rename(edu_8 = education) %>%
  mutate(edu_8 = as.factor(edu_8))

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
  mutate(bachelors = case_when(edu_8 %in% bachelors_yes ~ 1,
                               edu_8 %in% bachelors_no ~ 0)) %>%
  mutate(bachelors = as.factor(bachelors))
summary(panel$bachelors)
# 0    1 NA's 
# 5527 3166 3088 
rm(bachelors_yes, bachelors_no)

# Create education variable with 5 levels
panel <- panel %>%
  mutate(edu_5 = case_when(edu_8 == "Less than high school" ~ "Less than high school",
                           edu_8 == "High school graduate" ~ "High school",
                           edu_8 == "Less than college/some CEGEP" ~ "Some college",
                           edu_8 == "College / CEGEP graduate" ~ "Some college",
                           edu_8 == "Apprenticeship" ~ "Some college",
                           edu_8 == "Some university" ~ "Some college",
                           edu_8 == "Undergraduate university degree" ~ "College",
                           edu_8 == "Post-graduate university degree" ~ "Graduate or prof. degree")) %>%
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

table(panel$d7, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0          2365   181     0     0     0     0     0
# 1           412    35     0     0     0     0     0
# 2           386    24     0     0     0     0     0
# 3           104     8     0     0     0     0     0
# 4            32     3     0     0     0     0     0
# 5            10     0     0     0     0     0     0
# 6             2     0     0     0     0     0     0
# 7 or more     2     1     0     0     0     0     0
# <NA>          0  2189  1760  1440   899   920  1008

table(panel$children, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0        0     0  1309  1068     0     0     0
# 1        0     0   206   155     0     0     0
# 2        0     0   166   154     0     0     0
# 3        0     0    57    46     0     0     0
# 4        0     0    16    13     0     0     0
# 5        0     0     5     3     0     0     0
# 6        0     0     1     1     0     0     0
# <NA>  3313  2441     0     0   899   920  1008

table(panel$D7_6, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0             0     0     0     0     0   729     0
# 1             0     0     0     0     0    82     0
# 2             0     0     0     0     0    72     0
# 3             0     0     0     0     0    25     0
# 4             0     0     0     0     0     8     0
# 5             0     0     0     0     0     3     0
# 7 or more     0     0     0     0     0     1     0
# <NA>       3313  2441  1760  1440   899     0  1008

table(panel$D7_7, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0             0     0     0     0     0     0   756
# 1             0     0     0     0     0     0   124
# 2             0     0     0     0     0     0    80
# 3             0     0     0     0     0     0    33
# 4             0     0     0     0     0     0    10
# 5             0     0     0     0     0     0     4
# 7 or more     0     0     0     0     0     0     1
# <NA>       3313  2441  1760  1440   899   920     0

table(panel$Children_6, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0        0     0     0     0     0   702     0
# 1        0     0     0     0     0    76     0
# 2        0     0     0     0     0    79     0
# 3        0     0     0     0     0    28     0
# 4        0     0     0     0     0     8     0
# 5        0     0     0     0     0     4     0
# <NA>  3313  2441  1760  1440   899    23  1008

table(panel$Children_7, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0             0     0     0     0     0     0   716
# 1             0     0     0     0     0     0   127
# 2             0     0     0     0     0     0    81
# 3             0     0     0     0     0     0    29
# 4             0     0     0     0     0     0    13
# 5             0     0     0     0     0     0     3
# 7 or more     0     0     0     0     0     0     1
# <NA>       3313  2441  1760  1440   899   920    38

# Fill out number of children from waves 1 and 2
panel$numchildren_8 <- panel$d7

# Fill out number of children from waves 3 and 4
panel$numchildren_8[which(is.na(panel$numchildren_8))] <- panel$children[which(is.na(panel$numchildren_8))]

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
panel$numchildren_8[which(is.na(panel$numchildren_8))] <- panel$D7_6[which(is.na(panel$numchildren_8))]
table(panel$numchildren_8, panel$wave, useNA = "ifany")

# Fill out number of children from wave 7
sum(panel$D7_7 == panel$Children_7, na.rm = TRUE)
# [1] 853
length(panel$D7_7[!is.na(panel$D7_7)])
# [1] 1008
length(panel$Children_7[!is.na(panel$Children_7)])
# [1] 970
# Not sure what is happening here

# Use the more complete variable
panel$numchildren_8[which(is.na(panel$numchildren_8))] <- panel$D7_7[which(is.na(panel$numchildren_8))]
table(panel$numchildren_8, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0          2365   181  1309  1068     0   729   756
# 1           412    35   206   155     0    82   124
# 2           386    24   166   154     0    72    80
# 3           104     8    57    46     0    25    33
# 4            32     3    16    13     0     8    10
# 5            10     0     5     3     0     3     4
# 6             2     0     1     1     0     0     0
# 7 or more     0     0     0     0     0     1     1
# <NA>          2  2190     0     0   899     0     0

panel <- panel %>%
  mutate(numchildren_8 = as.factor(numchildren_8))


# .. Annual household income ####
# Look for variables with gender responses
names(panel)[str_detect(colnames(panel), fixed("d14", ignore_case = TRUE))]
# [1] "d14"   "D14_6" "D14_7"

panel %>%
  filter(is.na(d14)) %>%
  select(wave) %>%
  summary
# wave     
# wave1:   0  
# wave2:2189  
# wave3:   0  
# wave4:   0  
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
# wave1 wave2 wave3 wave4 wave5 wave6 wave7 
#     0     0     0     0     0   920     0
panel$income[which(is.na(panel$income))] <- panel$D14_6[which(is.na(panel$income))]

# Fill out missing income from wave 7
panel %>%
  filter(complete.cases(D14_7)) %>%
  select(wave) %>%
  table
# wave1 wave2 wave3 wave4 wave5 wave6 wave7 
#     0     0     0     0     0     0  1008 
panel$income[which(is.na(panel$income))] <- panel$D14_7[which(is.na(panel$income))]

table(panel$income, panel$wave, useNA = "ifany")
#                      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# $100,000-$119,999      296    20   158   118     0    75   106
# $120,000-$159, 999     272    23   146   129     0    92   107
# $160,000-$199,999      103     5    53    44     0    38    53
# $20,000-$39,999        442    40   241   207     0   146   123
# $200,000 or more       107    12    52    38     0    31    46
# $40,000-$59,999        516    38   290   242     0   138   129
# $60,000-$79,999        439    34   241   201     0   144   144
# $80,000-$99,999        464    33   231   197     0   110   148
# Prefer not to answer   401    33   225   170     0   102    88
# Under $20,000          273    14   123    94     0    44    64
# <NA>                     0  2189     0     0   899     0     0

panel <- panel %>%
  rename(income_10 = income) %>%
  mutate(income_10 = as.factor(income_10))

# Create numeric income variable which takes the mid-point of each income bracket
panel <- panel %>%
  mutate(income_num_mid = case_when(income_10 == "Under $20,000" ~ 15000,
                                    income_10 == "$20,000-$39,999" ~ 30000,
                                    income_10 == "$40,000-$59,999" ~ 50000,
                                    income_10 == "$60,000-$79,999" ~ 70000,
                                    income_10 == "$80,000-$99,999" ~ 90000,
                                    income_10 == "$100,000-$119,999" ~ 110000,
                                    income_10 == "$120,000-$159, 999" ~ 140000,
                                    income_10 == "$160,000-$199,999" ~ 180000,
                                    income_10 == "$200,000 or more" ~ 250000))

table(panel$wave, panel$income_num_mid, useNA = "ifany")
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
  mutate(income_6 = case_when(income_10 == "Under $20,000" ~ "Less than 20,000",
                              income_10 == "$20,000-$39,999" ~ "20,000-40,000",
                              income_10 == "$40,000-$59,999" ~ "40,000-60,000",
                              income_10 == "$60,000-$79,999" ~ "60,000-80,000",
                              income_10 == "$80,000-$99,999" ~ "80,000-100,000",
                              income_10 == "$100,000-$119,999" ~ "100,000 and over",
                              income_10 == "$120,000-$159, 999" ~ "100,000 and over",
                              income_10 == "$160,000-$199,999" ~ "100,000 and over",
                              income_10 == "$200,000 or more" ~ "100,000 and over")) %>%
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

table(panel$d8, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0           343    24     0     0     0     0     0
# 1          1360   109     0     0     0     0     0
# 2          1197    93     0     0     0     0     0
# 3           296    17     0     0     0     0     0
# 4            72     5     0     0     0     0     0
# 5 or more    45     4     0     0     0     0     0
# <NA>          0  2189  1760  1440   899   920  1008

table(panel$D8_6, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0             0     0     0     0     0   113     0
# 1             0     0     0     0     0   426     0
# 2             0     0     0     0     0   305     0
# 3             0     0     0     0     0    54     0
# 4             0     0     0     0     0    14     0
# 5 or more     0     0     0     0     0     8     0
# <NA>       3313  2441  1760  1440   899     0  1008

table(panel$D8_7, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0             0     0     0     0     0     0   109
# 1             0     0     0     0     0     0   430
# 2             0     0     0     0     0     0   327
# 3             0     0     0     0     0     0    93
# 4             0     0     0     0     0     0    36
# 5 or more     0     0     0     0     0     0    13
# <NA>       3313  2441  1760  1440   899   920     0

panel$numvehicle <- panel$d8

# Fill out missing vehicles from wave 6
panel$numvehicle[which(is.na(panel$numvehicle))] <- panel$D8_6[which(is.na(panel$numvehicle))]

# Fill out missing vehicles from wave 7
panel$numvehicle[which(is.na(panel$numvehicle))] <- panel$D8_7[which(is.na(panel$numvehicle))]

table(panel$numvehicle, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 0           343    24     0     0     0   113   109
# 1          1360   109     0     0     0   426   430
# 2          1197    93     0     0     0   305   327
# 3           296    17     0     0     0    54    93
# 4            72     5     0     0     0    14    36
# 5 or more    45     4     0     0     0     8    13
# <NA>          0  2189  1760  1440   899     0     0

panel <- panel %>%
  rename(numvehicle_6 = numvehicle) %>%
  mutate(numvehicle_6 = as.factor(numvehicle_6))

# Create numeric variable for number of vehicles
panel <- panel %>%
  mutate(numvehicle = recode_factor(numvehicle_6,
                                    "0" = "0",
                                    "1" = "1",
                                    "2" = "2",
                                    "3" = "3",
                                    "4" = "4",
                                    "5 or more" = "5")) %>%
  mutate(numvehicle = as.numeric(as.character(numvehicle)))
summary(panel$numvehicle)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    1.00    1.00    1.53    2.00    5.00    6288 
   

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

panel$rural_7 <- panel$d10

# Fill out missing urban/rural from wave 6
panel$rural_7[which(is.na(panel$rural_7))] <- panel$D10_6[which(is.na(panel$rural_7))]

# Fill out missing urban/rural from wave 7
panel$rural_7[which(is.na(panel$rural_7))] <- panel$D10_7[which(is.na(panel$rural_7))]

table(panel$rural_7, panel$wave, useNA = "ifany")
#                                           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# In a remote area                             21    17     0     0     0     8     5
# In a rural area                             301   221     0     0     0    83   111
# In a small town                             362   241     0     0     0    71   117
# In a smaller, regional city                 643   444     0     0     0   162   155
# Not sure                                     17     5     0     0     0     2     1
# Within a large city                        1288   987     0     0     0   388   408
# Within a suburb, adjacent to a large city   681   526     0     0     0   206   211
# <NA>                                          0     0  1760  1440   899     0     0

panel <- panel %>%
  mutate(rural_7 = as.factor(rural_7))

# Create dichotomous rural variable
rural_yes <- c("In a remote area",
               "In a rural area",
               "In a small town")

rural_no <- c("In a smaller, regional city",
              "Within a large city",
              "Within a suburb, adjacent to a large city")

panel <- panel %>%
  mutate(rural = case_when(rural_7 %in% rural_yes ~ 1,
                           rural_7 %in% rural_no ~ 0)) %>%
  mutate(rural = as.factor(rural))
summary(panel$rural)
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
panel$party[which(panel$party == "Did not vote" | 
                    panel$party == "I would not vote" |
                    panel$party == "None")] <- "Don't vote"

table(panel$party, panel$wave, useNA = "ifany")
#                    wave1 wave2 wave3 wave4 wave5 wave6 wave7
# Bloc Québécois       115    91    63    92     0    61    55
# Conservative Party  1130   879   640   459     0   251   278
# Don't know             0     0     0    47     0    41     0
#   Don't vote           525   328   241    12     0    82   131
# Green Party          290   238   174    61     0    28    40
# Liberal Party        667   485   386   307     0   255   211
# NDP                  460   359   216   214     0   168   196
# Other party            0     0     0    17     0    14    59
# People's Party       126    61    40    25     0    20    38
# <NA>                   0     0     0   206   899     0     0

panel <- panel %>%
  rename(party_9 = party) %>%
  mutate(party_9 = as.factor(party_9))
summary(panel$party_9)
# Bloc Québécois Conservative Party         Don't know         Don't vote        Green Party 
# 477               3637                 88               1319                831 
# Liberal Party                NDP        Other party     People's Party               NA's 
# 2311               1613                 90                310               1105 


# .. Consistency in party preference ####
panel_wide <- panel %>%
  select(responseid, wave, party_9) %>%
  spread(wave, party_9)

# Create indicator for party preferences between waves 1 and 4
panel_wide <- panel_wide %>%
  mutate(votertype1.4 = case_when(wave1 == "Conservative Party" & wave4 == "Conservative Party" ~ "Consistent Convervatives",
                                  wave1 == "Liberal Party" & wave4 == "Liberal Party" ~ "Consistent Liberals",
                                  wave1 == "NDP" & wave4 == "NDP" ~ "Consistent NDP",
                                  wave1 == "Green Party" & wave4 == "Green Party" ~ "Consistent Greens",
                                  wave1 == "Bloc Québécois" & wave4 == "Bloc Québécois" ~ "Consistent Bloc",
                                  wave1 == "Liberal Party" & wave4 != "Liberal Party" ~ "Liberals to Other",
                                  wave1 != "Liberal Party" & wave4 == "Liberal Party" ~ "Other to Liberals"))

# Create indicator for party preferences between waves 3 and 4
panel_wide <- panel_wide %>%
  mutate(votertype3.4 = case_when(wave3 == "Conservative Party" & wave4 == "Conservative Party" ~ "Consistent Convervatives",
                                  wave3 == "Liberal Party" & wave4 == "Liberal Party" ~ "Consistent Liberals",
                                  wave3 == "NDP" & wave4 == "NDP" ~ "Consistent NDP",
                                  wave3 == "Green Party" & wave4 == "Green Party" ~ "Consistent Greens",
                                  wave3 == "Bloc Québécois" & wave4 == "Bloc Québécois" ~ "Consistent Bloc",
                                  wave3 == "Liberal Party" & wave4 != "Liberal Party" ~ "Liberals to Other",
                                  wave3 != "Liberal Party" & wave4 == "Liberal Party" ~ "Other to Liberals"))

# Create indicator for party preferences between waves 6 and 7
panel_wide <- panel_wide %>%
  mutate(votertype6.7 = case_when(wave6 == "Conservative Party" & wave7 == "Conservative Party" ~ "Consistent Convervatives",
                                  wave6 == "Liberal Party" & wave7 == "Liberal Party" ~ "Consistent Liberals",
                                  wave6 == "NDP" & wave7 == "NDP" ~ "Consistent NDP",
                                  wave6 == "Green Party" & wave7 == "Green Party" ~ "Consistent Greens",
                                  wave6 == "Bloc Québécois" & wave7 == "Bloc Québécois" ~ "Consistent Bloc",
                                  wave6 == "Liberal Party" & wave7 != "Liberal Party" ~ "Liberals to Other",
                                  wave6 != "Liberal Party" & wave7 == "Liberal Party" ~ "Other to Liberals"))

# Create indicator for party preferences between waves 1 and 7
panel_wide <- panel_wide %>%
  mutate(votertype1.7 = case_when(wave1 == "Conservative Party" & wave7 == "Conservative Party" ~ "Consistent Convervatives",
                                  wave1 == "Liberal Party" & wave7 == "Liberal Party" ~ "Consistent Liberals",
                                  wave1 == "NDP" & wave7 == "NDP" ~ "Consistent NDP",
                                  wave1 == "Green Party" & wave7 == "Green Party" ~ "Consistent Greens",
                                  wave1 == "Bloc Québécois" & wave7 == "Bloc Québécois" ~ "Consistent Bloc",
                                  wave1 == "Liberal Party" & wave7 != "Liberal Party" ~ "Liberals to Other",
                                  wave1 != "Liberal Party" & wave7 == "Liberal Party" ~ "Other to Liberals"))

# Put it all together
panel <- panel %>%
  left_join(panel_wide %>%
              select(-c(starts_with("wave"))) %>%
              mutate_at(vars(starts_with("votertype")),
                        list(~as.factor(.))),
            by = c("responseid"))
rm(panel_wide)



## ## ## ## ## ## ## ## ## ## ##
# CARBON PRICING OPINIONS   ####
## ## ## ## ## ## ## ## ## ## ##

# .. Opinions on carbon pricing ####
# Look for variables with carbon pricing responses
names(panel)[str_detect(colnames(panel), fixed("po2", ignore_case = TRUE))]
# [1] "po2"           "po22_96_other" "po22_1"        "po22_2"        "po22_3"        "po22_4"       
# [7] "po22_5"        "po22_96"       "po22_98"       "po23_96_other" "po23_1"        "po23_2"       
# [13] "po23_3"        "po23_4"        "po23_5"        "po23_6"        "po23_96"       "po23_97"      
# [19] "po23_98"       "PO2_5"         "PO2_5_2"       "PO2_6"         "PO2_7" 

names(panel)[str_detect(colnames(panel), fixed("dv", ignore_case = TRUE))]
# [1] "dv0"   "dv1"   "dv2_1" "dv2_2" "dv2_3" "dv2_4" "dv3"   "adv"   "DVr1"  "DVr2"  "DVr3"  "DVr4" 

# Relevant variables are po2, PO2_5, PO2_5_2, PO2_6, PO2_7, dv1

table(panel$po2, panel$wave, useNA = "ifany")
table(panel$PO2_5, panel$wave, useNA = "ifany")
table(panel$PO2_6, panel$wave, useNA = "ifany")
table(panel$PO2_7, panel$wave, useNA = "ifany")

panel$cp_opinion <- panel$po2

# Fill out missing carbon pricing opinions from wave 5
panel$cp_opinion[which(is.na(panel$cp_opinion))] <- panel$PO2_5[which(is.na(panel$cp_opinion))]

# Fill out missing carbon pricing opinions from wave 6
panel$cp_opinion[which(is.na(panel$cp_opinion))] <- panel$PO2_6[which(is.na(panel$cp_opinion))]

# Fill out missing carbon pricing opinions from wave 7
panel$cp_opinion[which(is.na(panel$cp_opinion))] <- panel$PO2_7[which(is.na(panel$cp_opinion))]

table(panel$cp_opinion, panel$wave, useNA = "ifany")
#                  wave1 wave2 wave3 wave4 wave5 wave6 wave7
# Not sure           377   179   128    86   100    92   103
# Somewhat oppose    626   474   327   158   177   163   177
# Somewhat support   880   683   478   249   247   255   269
# Strongly oppose    970   731   535   237   249   253   317
# Strongly support   460   374   292   136   126   157   142
# <NA>                 0     0     0   574     0     0     0

panel <- panel %>%
  rename(cp_opinion_5 = cp_opinion) %>%
  mutate(cp_opinion_5 = as.factor(cp_opinion_5))

summary(panel$cp_opinion_5)
# Not sure  Somewhat oppose Somewhat support  Strongly oppose Strongly support             NA's 
#     1065             2102             3061             3292             1687              574 


# .. Support/opposition to carbon pricing ####
support <- c("Somewhat support", "Strongly support")
oppose <- c("Somewhat oppose", "Strongly oppose")

strongsupport <- c("Strongly support")
strongoppose <- c("Strongly oppose")

# Create dichotomous variables for support/opposition
panel <- panel %>%
  mutate(cp_support = case_when(cp_opinion_5 %in% support ~ 1,
                                !cp_opinion_5 %in% support ~ 0)) %>%
  mutate(cp_support = as.factor(cp_support))
panel <- panel %>%
  mutate(cp_oppose = case_when(cp_opinion_5 %in% oppose ~ 1,
                               !cp_opinion_5 %in% oppose ~ 0)) %>%
  mutate(cp_oppose = as.factor(cp_oppose))

# Create dichotomous variables for strong support/opposition
panel <- panel %>%
  mutate(cp_strongsupport = case_when(cp_opinion_5 %in% strongsupport ~ 1,
                                      !cp_opinion_5 %in% strongsupport ~ 0)) %>%
  mutate(cp_strongsupport = as.factor(cp_strongsupport))
panel <- panel %>%
  mutate(cp_strongoppose = case_when(cp_opinion_5 %in% strongoppose ~ 1,
                                     !cp_opinion_5 %in% strongoppose ~ 0)) %>%
  mutate(cp_strongoppose = as.factor(cp_strongoppose))

summary(panel$cp_opinion)
summary(panel$cp_support)
# 0    1 
# 7033 4748 
summary(panel$cp_strongsupport)
# 0     1 
# 10094  1687 
summary(panel$cp_oppose)
# 0    1 
# 6387 5394 
summary(panel$cp_strongoppose)
# 0    1 
# 8489 3292 

rm(support, oppose, strongsupport, strongoppose)


# .. Beliefs about fairness of carbon pricing ####
# Look for variables with carbon beliefs responses
names(panel)[str_detect(colnames(panel), fixed("po10", ignore_case = TRUE))]
# [1] "po10_1"   "po10_2"   "po10_3"   "po10_4"   "po10_5"   "po10_6"   "po10_7"   "po10_8"   "PO10_5r1"
# [10] "PO10_5r2" "PO10_5r3" "PO10_5r4" "PO10_5r5" "PO10_5r6" "PO10_7r1" "PO10_7r2" "PO10_7r3" "PO10_7r4"
# [19] "PO10_7r5" "PO10_7r6"

# Relevant variables are po10_1, PO10_5r1, PO10_7r1

panel <- panel %>%
  mutate(cp_fair_num = as.numeric(str_sub(panel$po10_1, start = -1)))
table(panel$cp_fair_num, useNA = "ifany")
# 1    2    3    4    5 <NA> 
# 2520 1658 2415 1645  716 2827 

# Fill out missing opinions on fairness from wave 5
panel$cp_fair_num[which(is.na(panel$cp_fair_num))] <- panel$PO10_5r1[which(is.na(panel$cp_fair_num))]

# Fill out missing opinions on fairness from wave 7
panel <- panel %>%
  mutate(PO10_7r1 = as.numeric(str_sub(panel$PO10_7r1, start = 1, end = 1)))
panel$cp_fair_num[which(is.na(panel$cp_fair_num))] <- panel$PO10_7r1[which(is.na(panel$cp_fair_num))]

table(panel$cp_fair_num, panel$wave, useNA = "ifany")
#      wave1 wave2 wave3 wave4 wave5 wave6 wave7
# 1      860   749   500   411   218     0   330
# 2      570   478   341   269   169     0   156
# 3      968   582   447   418   287     0   293
# 4      651   446   320   228   164     0   170
# 5      264   186   152   114    61     0    59
# <NA>     0     0     0     0     0   920     0

panel <- panel %>%
  mutate(cp_fair_num = as.numeric(cp_fair_num))
summary(panel$cp_fair_num)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   3.000   2.589   4.000   5.000     920 



## ## ## ## ## ## ## ## ## ## ##
# PERCEPTIONS               ####
## ## ## ## ## ## ## ## ## ## ##

# .. Perceived rebate amount estimate ####
# Look for variables with responses about received rebates
names(panel)[str_detect(colnames(panel), fixed("claim", ignore_case = TRUE))]
# [1] "claima"     "claimb"     "claimc"     "claimd"     "Claim_3A_6" "Claim_3B_6" "Claim_3C_6"
# [8] "Claim_3D_6"

table(panel$claima, panel$wave, useNA = "ifany")
table(panel$claimb, panel$wave, useNA = "ifany")
table(panel$claimc, panel$wave, useNA = "ifany")
table(panel$claimd, panel$wave, useNA = "ifany")
# The claim* variables are all for wave 3
# claimd is for waves 3 and 4

# Relevant variables are claimd and Claim_3D_6

panel$div_perceived <- panel$claimd

# Fill out missing perceptions of dividends from wave 6
panel$div_perceived[which(is.na(panel$div_perceived))] <- panel$Claim_3D_6[which(is.na(panel$div_perceived))]

table(panel$div_perceived, panel$wave, useNA = "ifany")
#           wave1 wave2 wave3 wave4 wave5 wave6 wave7
# $0            0     0   887   600     0   425     0
# $1-100        0     0   244   149     0   120     0
# $100-200      0     0   184    83     0    98     0
# $1000+        0     0    15    13     0     9     0
# $200-300      0     0   168    47     0    75     0
# $300-400      0     0   116    23     0    61     0
# $400-500      0     0    65    26     0    48     0
# $500-600      0     0    47    13     0    27     0
# $600-700      0     0    19     6     0    13     0
# $700-800      0     0     7     2     0    12     0
# $800-900      0     0     3     1     0     5     0
# $900-1000     0     0     5     3     0     0     0
# <NA>       3313  2441     0   474   899    27  1008

# Create categorical variable indicating tranche of perceived dividend
panel <- panel %>%
  mutate(div_perceived_12 = as.factor(div_perceived))
panel$div_perceived_12 <- recode_factor(panel$div_perceived_12,
                                         "$0" = 0,
                                         "$1-100" = 1,
                                         "$100-200" = 2,
                                         "$200-300" = 3,
                                         "$300-400" = 4,
                                         "$400-500" = 5,
                                         "$500-600" = 6,
                                         "$600-700" = 7,
                                         "$700-800" = 8,
                                         "$800-900" = 9,
                                         "$900-1000" = 10,
                                         "$1000+" = 11)
summary(panel$div_perceived_12)
#    0    1    2    3    4    5    6    7    8    9   10   11 NA's 
# 1912  513  365  290  200  139   87   38   21    9    8   37 8162 


# .. Perceived increase in heating costs as a result of carbon pricing ####
# Six months from now, how many dollars more per month do you think your household 
# will pay for electricity, natural gas and home heating because of the carbon pricing policy? (1)
table(panel$po13_1, panel$wave, useNA = "ifany")
# Data for waves 1 and 4

# To the best of your knowledge, how much more per month in dollars, on average, 
# did your household pay for home heating, hot water, and cooking as a result 
# of carbon pricing in your province?
table(panel$C8_6, panel$wave, useNA = "ifany")
# Data for wave 6
# Data is numeric

# To the best of your knowledge, how much more per month THIS WINTER, in dollars, on average, 
# did your household pay for home heating, hot water, and cooking as a result 
# of carbon pricing in your province? You can treat Winter as the average 
# from October 1 through March 31st of this year.
table(panel$C8_7b, panel$wave, useNA = "ifany")
# Data for wave 7
# Data is numeric

# Convert numeric estimates of increased heating costs from waves 6 and 7 to categorical
panel <- panel %>%
  mutate(C8_6_cat = case_when(C8_6 == 0 ~ "$0 per month",
                              C8_6 >= 1 & C8_6 <= 24 ~ "$1-$24 per month",
                              C8_6 >= 25 & C8_6 <= 49 ~ "$25-$49 per month",
                              C8_6 >= 50 & C8_6 <= 99 ~ "$50-$99 per month",
                              C8_6 >= 100 ~ "$100 or more per month"))

panel <- panel %>%
  mutate(C8_7b_cat = case_when(C8_7b == 0 ~ "$0 per month",
                               C8_7b >= 1 & C8_7b <= 24 ~ "$1-$24 per month",
                               C8_7b >= 25 & C8_7b <= 49 ~ "$25-$49 per month",
                               C8_7b >= 50 & C8_7b <= 99 ~ "$50-$99 per month",
                               C8_7b >= 100 ~ "$100 or more per month"))

# Fill in data from waves 1, 4, 6, and 7 for categorical variable
panel$inc_heat_perceived_6 <- panel$po13_1
panel$inc_heat_perceived_6[which(is.na(panel$inc_heat_perceived_6))] <- panel$C8_6_cat[which(is.na(panel$inc_heat_perceived_6))]
panel$inc_heat_perceived_6[which(is.na(panel$inc_heat_perceived_6))] <- panel$C8_7b_cat[which(is.na(panel$inc_heat_perceived_6))]

panel$inc_heat_perceived_6[which(panel$inc_heat_perceived_6 == "I don’t know")] <- "I don't know"
panel$inc_heat_perceived_6[which(panel$inc_heat_perceived_6 == "$50-99$ per month")] <- "$50-$99 per month"

panel <- panel %>%
  mutate(inc_heat_perceived_6 = as.factor(inc_heat_perceived_6))
table(panel$inc_heat_perceived_6, panel$wave, useNA = "ifany")
#                        wave1 wave2 wave3 wave4 wave5 wave6 wave7
# $0 per month             157     0     0   112     0   260    25
# $1-$24 per month         833     0     0   417     0   372   172
# $100 or more per month   385     0     0   140     0    38   596
# $25-$49 per month        593     0     0   240     0   133   104
# $50-$99 per month        440     0     0   141     0   117   111
# I don't know             905     0     0   390     0     0     0
# <NA>                       0  2441  1760     0   899     0     0

# Fill in data from waves 6 and 7 for numerical variable
panel$inc_heat_perceived_num <- panel$C8_6
panel$inc_heat_perceived_num[which(is.na(panel$inc_heat_perceived_num))] <- panel$C8_7b[which(is.na(panel$inc_heat_perceived_num))]
panel <- panel %>%
  mutate(inc_heat_perceived_num = as.numeric(inc_heat_perceived_num))

ggplot(data = panel %>%
         select(wave, inc_heat_perceived_num) %>%
         filter(wave == "wave6" | wave == "wave7"),
       aes(x = inc_heat_perceived_num,
           fill = wave)) +
  geom_density()
# Data is highly right-skewed due to outliers

# Identify and remove outliers with Rosner's test
outlier.test <- rosnerTest(panel$inc_heat_perceived_num, k = 10)
outlier.obs <- outlier.test$all.stats[which(outlier.test$all.stats$Outlier == TRUE), "Obs.Num"]
panel$inc_heat_perceived_num[outlier.obs] <- NA

ggplot(data = panel %>%
         select(wave, inc_heat_perceived_num) %>%
         filter(wave == "wave6" | wave == "wave7"),
       aes(x = inc_heat_perceived_num,
           fill = wave)) +
  geom_density()


# .. Perceived increase in gasoline costs as a result of carbon pricing ####
# Six months from now, how many dollars more per month do you think your household 
# will pay for gasoline and diesel because of the carbon pricing policy? (3)
table(panel$po13_3, panel$wave, useNA = "ifany")
# Data for wave 1

# To the best of your knowledge, how much more per month in dollars, on average, 
# did your household pay in the last year for gasoline and diesel as a result 
# of carbon pricing in your province?
table(panel$C11_6, panel$wave, useNA = "ifany")
# Data for wave 6
# Data is numeric

# To the best of your knowledge, how much more per month in dollars, on average, 
# did your household pay over the last year for gasoline and diesel as a result 
# of carbon pricing in your province?
table(panel$E11_7, panel$wave, useNA = "ifany")
# Data for wave 7
# Data is numeric

# Convert numeric estimates of increased gasoline costs from waves 6 and 7 to categorical
panel <- panel %>%
  mutate(C11_6_cat = case_when(C11_6 == 0 ~ "$0 per month",
                               C11_6 >= 1 & C11_6 <= 24 ~ "$1-$24 per month",
                               C11_6 >= 25 & C11_6 <= 49 ~ "$25-$49 per month",
                               C11_6 >= 50 & C11_6 <= 99 ~ "$50-$99 per month",
                               C11_6 >= 100 ~ "$100 or more per month"))

panel <- panel %>%
  mutate(E11_7_cat = case_when(E11_7 == 0 ~ "$0 per month",
                               E11_7 >= 1 & E11_7 <= 24 ~ "$1-$24 per month",
                               E11_7 >= 25 & E11_7 <= 49 ~ "$25-$49 per month",
                               E11_7 >= 50 & E11_7 <= 99 ~ "$50-$99 per month",
                               E11_7 >= 100 ~ "$100 or more per month"))

# Fill in data from waves 1, 6, and 7 for categorical variable
panel$inc_gas_perceived_6 <- panel$po13_3
panel$inc_gas_perceived_6[which(is.na(panel$inc_gas_perceived_6))] <- panel$C11_6_cat[which(is.na(panel$inc_gas_perceived_6))]
panel$inc_gas_perceived_6[which(is.na(panel$inc_gas_perceived_6))] <- panel$E11_7_cat[which(is.na(panel$inc_gas_perceived_6))]

panel$inc_gas_perceived_6[which(panel$inc_gas_perceived_6 == "I don’t know")] <- "I don't know"
panel$inc_gas_perceived_6[which(panel$inc_gas_perceived_6 == "$50-99$ per month")] <- "$50-$99 per month"

panel <- panel %>%
  mutate(inc_gas_perceived_6 = as.factor(inc_gas_perceived_6))
table(panel$inc_gas_perceived_6, panel$wave, useNA = "ifany")
#                        wave1 wave2 wave3 wave4 wave5 wave6 wave7
# $0 per month             177     0     0     0     0   258    80
# $1-$24 per month         790     0     0     0     0   308   295
# $100 or more per month   375     0     0     0     0    47   332
# $25-$49 per month        671     0     0     0     0   157   156
# $50-$9 per month           0     0     0     0     0   150   145
# $50-99$ per month        420     0     0     0     0     0     0
# I don't know             880     0     0     0     0     0     0
# <NA>                       0  2441  1760  1440   899     0     0

# Fill in data from waves 6 and 7 for numerical variable
panel$inc_gas_perceived_num <- panel$C11_6
panel$inc_gas_perceived_num[which(is.na(panel$inc_gas_perceived_num))] <- panel$E11_7[which(is.na(panel$inc_gas_perceived_num))]
panel <- panel %>%
  mutate(inc_gas_perceived_num = as.numeric(inc_gas_perceived_num))

ggplot(data = panel %>%
         select(wave, inc_gas_perceived_num) %>%
         filter(wave == "wave6" | wave == "wave7"),
       aes(x = inc_gas_perceived_num,
           fill = wave)) +
  geom_density()
# Data is highly right-skewed due to outliers

# Identify and remove outliers with Rosner's test
outlier.test <- rosnerTest(panel$inc_gas_perceived_num, k = 10)
outlier.obs <- outlier.test$all.stats[which(outlier.test$all.stats$Outlier == TRUE), "Obs.Num"]
panel$inc_gas_perceived_num[outlier.obs] <- NA

ggplot(data = panel %>%
         select(wave, inc_gas_perceived_num) %>%
         filter(wave == "wave6" | wave == "wave7"),
       aes(x = inc_gas_perceived_num,
           fill = wave)) +
  geom_density()

rm(outlier.test, outlier.obs)



## ## ## ## ## ## ## ## ## ## ##
# CODEBOOK                  ####
## ## ## ## ## ## ## ## ## ## ##

id.vars <- c("responseid",
             "wave",
             "monthindex",
             "prov")
treatment.vars <- c("fedprice",
                    "postperiod1.2",
                    "postperiod2.3",
                    "postperiod2.4",
                    "postperiod3.4",
                    "postperiod1.5")
demographic.vars <- c("female",
                      "age_3",
                      "language_3",
                      "french",
                      "edu_8",
                      "edu_5",
                      "bachelors")
household.vars <- c("numchildren_8",
                    "income_10",
                    "income_6",
                    "income_num_mid",
                    "numvehicle",
                    "rural_7",
                    "rural")
partisanship.vars <- c("left_right_num",
                       "party_9",
                       "votertype1.4",
                       "votertype3.4",
                       "votertype6.7",
                       "votertype1.7")
opinions.vars <- c("cp_opinion_5",
                   "cp_support",
                   "cp_oppose",
                   "cp_strongsupport",
                   "cp_strongoppose",
                   "cp_fair_num")
perceptions.vars <- c("div_perceived_12",
                      "inc_heat_perceived_6",
                      "inc_heat_perceived_num",
                      "inc_gas_perceived_6",
                      "inc_gas_perceived_num")

panel_vars <- panel %>% 
  select(all_of(c(id.vars,
                  treatment.vars,
                  demographic.vars,
                  household.vars,
                  partisanship.vars,
                  opinions.vars,
                  perceptions.vars)))

levs <- sapply(panel_vars, levels)
codebook <- data.frame(vars = colnames(panel_vars),
                       class = sapply(panel_vars, class),
                       missing_obs = sapply(panel_vars, function(x) sum(is.na(x))),
                       factor_levels = sapply(levs, paste, collapse = ", "),
                       row.names = NULL)
write.csv(codebook, file = here("Data", "codebook.csv"), row.names = FALSE)

rm(levs, codebook, panel)



## ## ## ## ## ## ## ## ## ## ##
# EXPORT PANEL              ####
## ## ## ## ## ## ## ## ## ## ##

save(panel_vars, file = here("Data", "Processed", "panel_vars.Rdata"))