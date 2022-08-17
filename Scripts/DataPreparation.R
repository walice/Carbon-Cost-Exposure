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

panel <- full_join(wave1, wave2,
                   by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave3,
            by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave4,
            by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave5,
            by = c("responseid", "wave", "variable", "value")) %>%
  full_join(wave6,
            by = c("responseid", "wave", "variable", "value"))

panel <- dcast(panel, responseid + wave ~ variable)

rm(wave1, wave2, wave3, wave4, wave5, wave6)



## ## ## ## ## ## ## ## ## ## ##
# CLEAN VARIABLES           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Province names ####


