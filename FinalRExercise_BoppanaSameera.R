

nys_schools <- read.csv("data/nys_schools.csv", stringsAsFactors=FALSE)
View(nys_schools)

nys_acs <- read.csv("data/nys_acs.csv", stringsAsFactors=FALSE)
View(nys_acs)


summary(nys_acs)
summary(nys_schools)


nys_schools_clean <- nys_schools %>% filter(total_enroll != -99) %>% filter(per_free_lunch != -99) %>% 
filter(mean_ela_score != -99) %>% filter(mean_math_score != -99) 


summary(nys_schools_clean)

nys_acs$poverty_classification <- as.factor(ifelse(nys_acs$median_household_income < 46347, 'High',
                                                   ifelse(nys_acs$median_household_income < 50134, 'Medium', 'Low')))

summary(nys_acs)
view(nys_acs)

mean_math <- mean(nys_schools_clean$mean_math_score)
sd_math <- sd(nys_schools_clean$mean_math_score)
nys_schools_clean[math_zscore] = nys_schools_clean$mean_math_score - 
  
  
  
#1. For each county: total enrollment, percent of students qualifying for free or reduced price lunch, and percent of population in poverty.

  
  
view(nys_schools_clean)
nys_schools_clean$total_enroll_country <- 'NA'
total_enroll_country <- nys_schools_clean %>%  group_by(county_name) %>% total_enroll
view(total_enroll_country)
