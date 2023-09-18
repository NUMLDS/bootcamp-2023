# load data

nys_schools <- read.csv(here::here("data/nys_schools.csv"))
nys_acs <- read.csv(here::here("data/nys_acs.csv"))

# explore data
summary(nys_acs)
summary(nys_schools)

# get rid of missing data


nys_schools_clean <- replace(nys_schools, nys_schools==-99, NA)
summary(nys_schools_clean)
sum(is.na(nys_schools_clean))


# group into high, medium, and low poverty groups
##Low was <= 1st quantile for county_per_poverty
##Medium was > 1st quantile  and <= 3rd quantile for county_per_poverty
##High was > 3rd quantile for county_per_poverty
 nys_acs <- nys_acs %>%
   mutate(poverty_level = ifelse(county_per_poverty <= .10903, "low",
                           ifelse(county_per_poverty<=.14929, "medium", "high")))
 
#standardize test by year

nys_schools_clean <- nys_schools_clean %>%
  group_by(year) %>%
  mutate(stand_math = scale(mean_math_score), stand_ela = scale(mean_ela_score))

#merge datasets
nys_data <- inner_join(nys_schools_clean, nys_acs, by = c("year","county_name"))

#Example summary 1
ex_1 <- nys_data %>%
          group_by(county_name) %>%
          summarize(Enrollment = sum(total_enroll), 
                    per_lower_lunch = sum((per_free_lunch+per_reduced_lunch)*total_enroll)/sum(total_enroll),
                    per_poverty = sum(county_per_poverty*total_enroll)/sum(total_enroll))
                    

#Example visualization
nys_data %>%
 # group_by(poverty_level) %>%
  ggplot() +
  geom_point(aes(x = year, y = mean(stand_math) , col = poverty_level))



