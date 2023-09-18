library(tidyverse)


# Task 1: Import
school = read_csv(here::here("data/nys_schools.csv"))
acs = read_csv(here::here("data/nys_acs.csv"))


# Task 2: Explore
str(school)
str(acs)

summary(school)
summary(acs)


# Task 3: Missing value
school[school == -99] = NA

cut_points = c(0, 46347, 56448, Inf)
categories <- c("high", "medium", "low")
acs$poverty = cut(acs$median_household_income, breaks = cut_points, labels=categories)

scores_std <- school %>%
    select(year, contains("score")) %>%
    group_by(year) %>%
    summarize(ela_mean = mean(mean_ela_score, na.rm=TRUE),
              math_mean = mean(mean_math_score, na.rm=TRUE),
              ela_sd = sd(mean_ela_score, na.rm=TRUE),
              math_sd = sd(mean_math_score, na.rm=TRUE))

# Create z-score columns
schools_all = inner_join(school, scores_std, by="year")
schools_all = mutate(schools_all,
                     ela_z_score = (mean_ela_score-ela_mean)/ela_sd,
                     math_z_score = (mean_math_score-math_mean)/math_sd)

schools_all = inner_join(schools_all, acs, by = c("county_name", "year"))



schools_all %>% 
    group_by(poverty) %>% 
    summarise(mean_math_score = sum(mean_math_score))
    ggplot() +
    geom_col(aes(x=poverty, y=mean_math_score))
