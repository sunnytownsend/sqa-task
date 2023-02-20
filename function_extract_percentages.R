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


# ----
extract_counts <- function(data, cols_x, attainment_label, level_label){
  
  
  cols_y = which(names(data) %in% c( "Entries.2019",  
                                     "Entries.2020",  
                                     "Entries.2021",  
                                     "Entries.2022"))
  
  x = data %>%
    filter(Subject == "Total") %>%
    select(names(data)[cols_x]) %>%
    decomma() %>%
    as.data.frame() %>%
    mutate(year = 2022:2019,
           attainment = attainment_label,
           level = level_label) %>%
    rename(count = ".") %>%
    as_tibble()
  
  
  y =  data %>%
    filter(Subject == "Total") %>%
    select(names(data)[cols_y]) %>%
    decomma() %>%
    as.data.frame() %>%
    mutate(year = 2022:2019) %>%
    rename(entries = ".") %>%
    as_tibble()
  
  
  # join entries to count and calc percentage
  count_data = left_join(x, y) %>%
    mutate(perc = count/entries * 100)
  
  # add exam variable
  count_data$exams = NA
  count_data[which(count_data$year %in% c(2019,2022)), "exams"] = "yes"
  count_data[which(count_data$year %in% c(2020,2021)), "exams"] = "no"
  
  return(count_data)
}