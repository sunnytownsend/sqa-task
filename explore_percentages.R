# Wrangle and plot SQA open data
# profiling % A grades by variables year and level
# Sunny Townsend, Feb 2023
library(tidyverse)
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


# National 5 ----
nat5_A = extract_percentages(data = nat5,
                             cols = which(names(nat5) %in% c("Grade.A.Percentage.2019",  
                                                             "Grade.A.Percentage.2020",  
                                                             "Grade.A.Percentage.2021",  
                                                             "Grade.A.Percentage.2022")),
                             attainment_label = "A", 
                             level_label = "National 5")

nat5_AC = extract_percentages(data = nat5,
                              cols = which(names(nat5) %in% c("Grade.A.C.Percentage.2019",  
                                                              "Grade.A.C.Percentage.2020",  
                                                              "Grade.A.C.Percentage.2021",  
                                                              "Grade.A.C.Percentage.2022")),
                              attainment_label = "A-C", 
                              level_label = "National 5")
# Highers ----
high_A = extract_percentages(data = high,
                             cols = which(names(high) %in% c("Grade.A.Percentage.2019",  
                                                             "Grade.A.Percentage.2020",  
                                                             "Grade.A.Percentage.2021",  
                                                             "Grade.A.Percentage.2022")),
                             attainment_label = "A", 
                             level_label = "Highers")

high_AC = extract_percentages(data = high,
                              cols = which(names(high) %in% c("Grade.A.C.Percentage.2019",  
                                                              "Grade.A.C.Percentage.2020",  
                                                              "Grade.A.C.Percentage.2021",  
                                                              "Grade.A.C.Percentage.2022")),
                              attainment_label = "A-C", 
                              level_label = "Highers")

# advanced highers ----
adv_A = extract_percentages(data = adv,
                            cols = which(names(adv) %in% c("Grade.A.Percentage.2019",  
                                                           "Grade.A.Percentage.2020",  
                                                           "Grade.A.Percentage.2021",  
                                                           "Grade.A.Percentage.2022")),
                            attainment_label = "A", 
                            level_label = "Advanced Highers")

adv_AC = extract_percentages(data = adv,
                             cols = which(names(adv) %in% c("Grade.A.C.Percentage.2019",  
                                                            "Grade.A.C.Percentage.2020",  
                                                            "Grade.A.C.Percentage.2021",  
                                                            "Grade.A.C.Percentage.2022")),
                             attainment_label = "A-C", 
                             level_label = "Advanced Highers")


# combine into dataset for analysis and visualisation ----
perc_data = rbind(adv_A, nat5_A, high_A, adv_AC, nat5_AC, high_AC)

# add exam variable
perc_data$exams = NA
perc_data[which(perc_data$year %in% c(2019,2022)), "exams"] = "yes"
perc_data[which(perc_data$year %in% c(2020,2021)), "exams"] = "no"


# bar chart ----
png("outputs/A_profiles.png")
filter(perc_data, attainment == "A") %>%
  ggplot(aes(fill=as.factor(year), y=percentage, x=level)) + 
  geom_bar(position="dodge", stat="identity")
dev.off()

png("outputs/AC_profiles.png")
filter(perc_data, attainment == "A-C") %>%
  ggplot(aes(fill=as.factor(year), y=percentage, x=level)) + 
  geom_bar(position="dodge", stat="identity")
dev.off()

# line plot ----
filter(perc_data, attainment == "A") %>%
  ggplot(aes(x=year, y=percentage, group=level, color=level)) +
  geom_line()

# exam bar chart
png("outputs/A_exams.png")
filter(perc_data, attainment == "A") %>%
  ggplot(aes(fill=as.factor(exams), y=percentage, x=level)) + 
  geom_bar(position="dodge", stat="identity")
dev.off()

png("outputs/AC_exams.png")
filter(perc_data, attainment == "A-C") %>%
  ggplot(aes(fill=as.factor(exams), y=percentage, x=level)) + 
  geom_bar(position="dodge", stat="identity")
dev.off()


# tests ----
nat5_A$percentage[1] == 
  nat5[which(nat5$Subject == "Total"), "Grade.A.Percentage.2022"] %>%
  str_remove("%") %>%
  as.numeric() 
high_A$percentage[1] == 
  high[which(high$Subject == "Total"), "Grade.A.Percentage.2022"] %>%
  str_remove("%") %>%
  as.numeric() 
adv_A$percentage[1] == 
  adv[which(adv$Subject == "Total"), "Grade.A.Percentage.2022"] %>%
  str_remove("%") %>%
  as.numeric() 

