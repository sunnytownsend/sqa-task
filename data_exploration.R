# Wrangle and plot SQA open data
# profiling % A grades by variables year and level
# Sunny Townsend, Feb 2023


library(tidyverse)


# wrangle totals data - extract and flatten ----
extract_percentages <- function(data, cols, attainment_label, level_label){
  
  data %>%
    filter(Subject == "Total") %>%
    select(names(adv)[cols]) %>%
    str_remove("%") %>%
    as.numeric() %>%
    as.data.frame() %>%
    mutate(year = 2022:2019,
           attainment = attainment_label,
           level = level_label) %>%
    rename(percentage = ".") %>%
    as_tibble()
}


# Advanced highers data ----
adv = read.csv("data/attainment-statistics-december-2022.xlsx - Advanced_Higher.csv",
               skip = 2) %>%
  as_tibble()

adv_A = extract_percentages(data = adv,
                        cols = which(names(adv) %in% c("Grade.A.Percentage.2019",  
                                                       "Grade.A.Percentage.2020",  
                                                       "Grade.A.Percentage.2021",  
                                                       "Grade.A.Percentage.2022")),
                        attainment_label = "A", 
                        level_label = "Advanced Highers")

#test
adv_A$percentage[1] == 
  adv[which(adv$Subject == "Total"), "Grade.A.Percentage.2022"] %>%
  str_remove("%") %>%
  as.numeric() 


# National 5 ----
nat5 = read.csv("data/attainment-statistics-december-2022.xlsx - National_5.csv",
               skip = 2) %>%
  as_tibble()

nat5_A = extract_percentages(data = nat5,
                        cols = which(names(nat5) %in% c("Grade.A.Percentage.2019",  
                                                       "Grade.A.Percentage.2020",  
                                                       "Grade.A.Percentage.2021",  
                                                       "Grade.A.Percentage.2022")),
                        attainment_label = "A", 
                        level_label = "National 5")
with(nat5_A, barplot(percentage ~ year))

#test
nat5_A$percentage[1] == 
  nat5[which(nat5$Subject == "Total"), "Grade.A.Percentage.2022"] %>%
  str_remove("%") %>%
  as.numeric() 



# Highers ----
high = read.csv("data/attainment-statistics-december-2022.xlsx - Higher.csv",
                skip = 2) %>%
  as_tibble()

high_A = extract_percentages(data = high,
                             cols = which(names(high) %in% c("Grade.A.Percentage.2019",  
                                                            "Grade.A.Percentage.2020",  
                                                            "Grade.A.Percentage.2021",  
                                                            "Grade.A.Percentage.2022")),
                             attainment_label = "A", 
                             level_label = "Highers")
with(high_A, barplot(percentage ~ year))

#test
high_A$percentage[1] == 
  high[which(high$Subject == "Total"), "Grade.A.Percentage.2022"] %>%
  str_remove("%") %>%
  as.numeric() 
    

# combine into dataset for analysis and visualisation ----
data_A = rbind(adv_A, nat5_A, high_A)


# plot ----
png("outputs/A_profiles.png")
ggplot(data_A, aes(fill=as.factor(year), y=percentage, x=level)) + 
   geom_bar(position="dodge", stat="identity")
dev.off()

