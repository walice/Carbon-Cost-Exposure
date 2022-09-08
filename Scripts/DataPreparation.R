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
# Clean Variables
# .. Province names



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

## .. Import data from survey instruments ####
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

save(panel, file = here("Data", "Processed", "panel_raw.Rdata"))

rm(wave1, wave2, wave3, wave4, wave5, wave6, wave7)



## ## ## ## ## ## ## ## ## ## ##
# CLEAN VARIABLES           ####
## ## ## ## ## ## ## ## ## ## ##

load(here("Data", "Processed", "panel_raw.Rdata"))


# .. Province names ####
panel %>%
  select(prov) %>%
  mutate_all(as.factor) %>%
  summary
# 5      :1513  
# 1      :1503  
# 3      :1501  
# 4      :1501  
# 2      :1496  
# (Other):1440  
# NA's   :2828  

panel <- panel %>%
  mutate(prov = as.factor(prov))

summary(panel$prov)
# 1    2    3    4    5   AB   BC   ON   QC   SK NA's 
# 1503 1496 1501 1501 1513  279  293  290  294  284 2828 

panel$prov <- recode_factor(panel$prov,
                            "1" = "AB",
                            "2" = "BC",
                            "3" = "ON",
                            "4" = "QC",
                            "5" = "SK")
levels(panel$prov)
summary(panel$prov)
# AB   BC   ON   QC   SK NA's 
# 1782 1789 1791 1795 1797 2828 

# Look for variables with province names
varnames <- names(panel)
varnames[str_detect(colnames(panel), fixed("prov", ignore_case=TRUE))]
# "prov"   "PROV_5" "PROV_6" "PROV"

# Fill out missing provinces from wave 5
panel$prov[which(is.na(panel$prov))] <- panel$PROV_5[which(is.na(panel$prov))]
summary(panel$prov)
# AB   BC   ON   QC   SK NA's 
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
  select(wave, PROV) %>%
  filter(is.na(PROV)) %>%
  mutate(wave = as.factor(wave)) %>%
  summary
# wave          PROV          
# wave1:3313   Length:10774      
# wave2:2441   Class :character  
# wave3:1760   Mode  :character  
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
# AB   BC   ON   QC   SK NA's 
# 2359 2397 2388 2372 2265    1

# Drop the Nova Scotia obs
panel <- panel %>%
  filter(!is.na(prov))
summary(panel$prov)
# AB   BC   ON   QC   SK 
# 2359 2397 2388 2372 2265

panel <- panel %>%
  select(-c("PROV_5", "PROV_6", "PROV"))
