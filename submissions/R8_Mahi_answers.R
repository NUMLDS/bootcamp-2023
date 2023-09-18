library(dplyr)
library(data.table)
library(ggplot2)

nys_schools_cleaned = nys_schools %>% 
  mutate(district_name_clean = as.factor(ifelse(district_name == "-99" |district_name == "" ,"MISC",as.character(district_name))),
         county_name_clean = as.factor(ifelse(county_name == "-99" |county_name == "","MISC",as.character(district_name))),
         region_clean = as.factor(ifelse(region == "-99" |region == "" ,"MISC",as.character(district_name)))) %>% 
  group_by(district_name_clean) %>% 
  mutate(mean_ela_score_clean = ifelse(is.na(mean_ela_score)|mean_ela_score == -99,mean(mean_ela_score),mean_ela_score))  


### 1. Import Data (5 min)
schools <- read.csv("~/Desktop/MLDS 2024/Bootcamp/R Programming/data/nys_schools.csv")
counties <- read.csv("data/nys_acs.csv", stringsAsFactors = FALSE)

### 2. Explore Data (5 minutes)

summary(schools) # schools has a lot of -99 minimums, issue with test scores
summary(counties)

### 3. Clean Data (40 minutes) [this is hard! if you can only get one of them, that's totally fine]

### Group brainstorm:
# How to deal with missing values?
# How to split into categories?
# What is a z-score?

# Googled: get characters columns in r
names(schools[, sapply(schools, class) == 'character'])
names(schools[, sapply(schools, class) == 'numeric'])

# Googled: replace values in dataframe in r
schools_char = schools[c("school_name", "district_name", "county_name", "region")]
schools_num = schools[c("total_enroll", "per_free_lunch", "per_reduced_lunch", "per_lep", "mean_ela_score", "mean_math_score")]

# Replace -99 values
schools_char[schools_char=="-99"] <- ""
unique(schools$region)
unique(schools_char$region)

# Check NAs
schools_num[schools_num==-99] <- NA
summary(schools)
summary(schools_num)
colSums(is.na(schools_num))

## 3.1 Create a new df without -99 values
schools_clean <- cbind(schools$school_cd, schools_char, schools$year, schools_num)
schools_clean <- rename(schools_clean, school_cd = "schools$school_cd")
schools_clean <- rename(schools_clean, year = "schools$year")
# View(schools_clean)

### 3.2 Create low mid high county groups

# Check histogram of poverty percentages
hist(counties$county_per_poverty)
hist(counties$county_per_poverty, breaks=20)
hist(counties$county_per_poverty, breaks=50)
hist(counties$county_per_poverty, breaks=100)

#counties$poverty_level = NA
#counties$poverty_level[counties$county_per_poverty>.18] = "high"
#counties$poverty_level[counties$county_per_poverty<.075] = "low"

# Create breaks
counties$poverty_level <- cut(counties$county_per_poverty, 
                              breaks=c(-Inf, 0.075, 0.2, Inf), 
                              labels=c("low","medium","high"))

# Check breaks
table(counties$poverty_level)

### 3.3 Standardized test scores

# Make calculation manually
scores_std <- schools_clean %>%
  select(year, contains("score")) %>%
  group_by(year) %>%
  summarize(ela_mean = mean(mean_ela_score, na.rm=TRUE),
            math_mean = mean(mean_math_score, na.rm=TRUE),
            ela_sd = sd(mean_ela_score, na.rm=TRUE),
            math_sd = sd(mean_math_score, na.rm=TRUE))

# Create z-score columns
schools_all = inner_join(schools_clean, scores_std, by="year")
schools_all = mutate(schools_all,
                     ela_z_score = (mean_ela_score-ela_mean)/ela_sd,
                     math_z_score = (mean_math_score-math_mean)/math_sd)

# Check results
# View(schools_all)
# View(filter(schools_all, year==2017))

### Regroup to review different solutions and issues along the way