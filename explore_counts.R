# Wrangle and plot SQA open data
# profiling % A grades by variables year and level
# Sunny Townsend, Feb 2023
library(tidyverse)
library(eeptools)

source("function_extract_percentages.R") # wrangle totals data - extract and flatten

# Load data ----
adv = read.csv("data/attainment-statistics-december-2022.xlsx - Advanced_Higher.csv",
               skip = 2) %>%
  as_tibble()
nat5 = read.csv("data/attainment-statistics-december-2022.xlsx - National_5.csv",
                skip = 2) %>%
  as_tibble()
high = read.csv("data/attainment-statistics-december-2022.xlsx - Higher.csv",
                skip = 2) %>%
  as_tibble()

# input prep ----
cols_x_A = which(names(adv) %in% c("Grade.A.Count.2019",  
                                   "Grade.A.Count.2020",  
                                   "Grade.A.Count.2021",  
                                   "Grade.A.Count.2022"))
cols_x_AC = which(names(adv) %in% c("Grade.A.C.Count.2019",  
                                    "Grade.A.C.Count.2020",  
                                    "Grade.A.C.Count.2021",  
                                    "Grade.A.C.Count.2022"))         

# national 5 ----
nat5_c_A = extract_counts(nat5,
                          cols_x = cols_x_A,
                          attainment_label = "A",
                          level_label = "National 5")
nat5_c_AC = extract_counts(nat5,
                           cols_x = cols_x_AC,
                           attainment_label = "A-C",
                           level_label = "National 5")


# highers ----
high_c_A = extract_counts(high,
                          cols_x = cols_x_A,
                          attainment_label = "A",
                          level_label = "Highers")

high_c_AC = extract_counts(high,
                           cols_x = cols_x_AC,
                           attainment_label = "A-C",
                           level_label = "Highers")


# advanced highers ----
adv_c_A = extract_counts(adv,
                         cols_x = cols_x_A,
                         attainment_label = "A",
                         level_label = "Advanced highers")

adv_c_AC = extract_counts(adv,
                          cols_x = cols_x_AC,
                          attainment_label = "A-C",
                          level_label = "Advanced highers")


# combine into dataset for analysis and visualisation ----
count_data = rbind(nat5_c_A, nat5_c_AC,
                   high_c_A, high_c_AC,
                   adv_c_A, adv_c_AC)

# tests ----
89.3 == count_data %>%
  filter(year == 2020, level == "Highers", attainment == "A-C") %>%
  select(perc) %>%
  round(1)

