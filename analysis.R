# analyse effect of exam/no exams on grades
# binomial GLM for proportion data

source("explore_counts.R")


# A ----
count_data_A = count_data %>%
  filter(attainment == "A") %>%
  mutate(entries_minus_count = entries-count)

#
count_by_exam_A = count_data_A %>%
  group_by(exams) %>%
  summarise(count = sum(count),
            entries_minus_count = sum(entries_minus_count))

bglm_A <- with(count_by_exam_A,
               glm(cbind(count, entries_minus_count) ~ exams,
                   family=binomial))
summary(bglm_A)


# probabilities ----
c = coefficients(bglm_A)[1]
delta = coefficients(bglm_A)[2]
p_A_no = exp(c)/(1+exp(c))
p_A_yes = exp(c + delta)/
  (1+exp(c + delta))


# A-C ----
count_data_AC = count_data %>%
  filter(attainment == "A-C") %>%
  mutate(entries_minus_count = entries-count)

#
count_by_exam_AC = count_data_AC %>%
  group_by(exams) %>%
  summarise(count = sum(count),
            entries_minus_count = sum(entries_minus_count))

bglm_AC <- with(count_by_exam_AC,
               glm(cbind(count, entries_minus_count) ~ exams,
                   family=binomial))
summary(bglm_AC)


# probabilities ----
c = coefficients(bglm_AC)[1]
delta = coefficients(bglm_AC)[2]
p_AC_no = exp(c)/(1+exp(c))
p_AC_yes = exp(c + delta)/
  (1+exp(c + delta))



               