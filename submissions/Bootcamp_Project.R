library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)
library(data.table)

nys_schools <- read.csv("data/nys_schools.csv")
nys_acs <- read.csv("data/nys_acs.csv")

View(nys_schools)
View(nys_acs)
summary(nys_schools)

# total_enrollment should not be below 0, there are -99 values
# per_free_lunch, per_reduced_lunch, per_lep are PERCENTS
# should not be over 1, should not be below 0, there are max values of 257
# mean_ela_score and mean_math_score are test scores, shouldn't be negative

summary(nys_acs)

# outliers in median_household_income
# The schools dataset is ranging between the years 2008-2017
# The county dataset is ranging between the years 2009-2016
# Be aware of that when merging datasets,
# county data might be missing for some schools in 2008 and 2017

nys_schools$total_enroll[nys_schools$total_enroll < 0] = 0

summary(nys_schools)

nys_schools$per_free_lunch[nys_schools$per_free_lunch <0] = 0
nys_schools$per_reduced_lunch[nys_schools$per_reduced_lunch < 0] = 0
nys_schools$per_lep[nys_schools$per_lep < 0] = 0
nys_schools$mean_ela_score[nys_schools$mean_ela_score < 0] =0
nys_schools$mean_math_score[nys_schools$mean_math_score < 0 ] = 0

nys_schools$per_free_lunch[nys_schools$per_free_lunch > 1] = 1
nys_schools$per_reduced_lunch[nys_schools$per_reduced_lunch >1] =1

#high, medium, low poverty groups

nys_acs$poverty_level <- cut(nys_acs$county_per_poverty, 
                              breaks=c(-Inf, 0.075, 0.2, Inf), 
                              labels=c("low","medium","high"))

table(nys_acs$poverty_level)

scores_std <- nys_schools %>%
  select(year, contains("score")) %>%
  group_by(year) %>%
  summarize(ela_mean = mean(mean_ela_score, na.rm=TRUE),
            math_mean = mean(mean_math_score, na.rm=TRUE),
            ela_sd = sd(mean_ela_score, na.rm=TRUE),
            math_sd = sd(mean_math_score, na.rm=TRUE))

scores_std

schools_all = inner_join(schools_cl, scores_std, by="year")
schools_all = mutate(schools_all,
                     ela_z_score = (mean_ela_score-ela_mean)/ela_sd,
                     math_z_score = (mean_math_score-math_mean)/math_sd)





