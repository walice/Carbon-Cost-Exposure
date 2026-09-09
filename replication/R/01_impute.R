# 01_impute.R
# Step 1: relevel factors, build 4-level perception categories, and impute
# missing Wave 7 covariates from earlier waves (time-invariant variables from
# the earliest wave; bills and perceptions from Wave 6 only).
# Input : data/panel_vars.rds (harmonised 7-wave panel, pre-imputation)
# Output: data/derived/panel_imputed.rds, output/tables/codebook_*.{csv,tex}
# Code is taken verbatim from Scripts/Analysis.R of the working repository.

source(here::here("R", "00_setup.R"))
panel <- readRDS(here("data", "panel_vars.rds"))
codebook <- read.csv(here("data", "codebook.csv"))

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

write.csv(codebook, file = here("output", "tables", "codebook_full.csv"), row.names = FALSE)

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

codebook_tex <- kbl(codebook %>% 
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
writeLines(codebook_tex, here("output", "tables", "codebook_SI.tex"))  # SI Appendix A codebook

rm(used)





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
                                as.numeric(pct_miss_change),
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

saveRDS(panel, file = here("data", "derived", "panel_imputed.rds"))



