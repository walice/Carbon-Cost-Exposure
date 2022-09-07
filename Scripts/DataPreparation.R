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

# panel <- panel %>%
#   mutate(prov = as.factor(prov))
# 
# summary(panel$prov)
# 
# panel$prov <- recode_factor(panel$prov, 
#                             1 = "AB",
#                             2 = "BC",
#                             3 = "ON",
#                             4 = "QC",
#                             5 = "SK")
# 
# levels(panel$prov)

panel$prov[which(panel$prov=="1")] <- "AB"
panel$prov[which(panel$prov=="2")] <- "BC"
panel$prov[which(panel$prov=="3")] <- "ON"
panel$prov[which(panel$prov=="4")] <- "QC"
panel$prov[which(panel$prov=="5")] <- "SK"
panel$prov[which(is.na(panel$prov))] <- panel$PROV_5[which(is.na(panel$prov))]

panel %>%
  select(prov) %>%
  mutate_all(as.factor) %>%
  summary
# prov     
# AB  :1958  
# BC  :1989  
# ON  :1984  
# QC  :1964  
# SK  :1958  
# NA's:1929
