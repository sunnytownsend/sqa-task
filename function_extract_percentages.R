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