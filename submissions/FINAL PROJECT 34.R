#Part 1 is referenced to Alice's code 1
#Step 1 Import data
library(dplyr)
library(data.table)
library(ggplot2)
install.packages("readr")
library(readr)
schools <- read_csv("nys_schools.csv")
counties <- read_csv("nys_acs.csv")
summary(schools)
summary(counties)

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
View(schools_clean)

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
View(schools_all)
View(filter(schools_all, year==2017))

### Regroup to review different solutions and issues along the way

#STEP 2: ANALYZE THE DATA
#Task 5
#1
county_summary <- schools %>%
  group_by(county_name) %>%
  summarize(
    total_enrollment = sum(total_enroll),
    percent_free_or_reduced_lunch = mean(per_free_lunch + per_reduced_lunch),
    percent_population_in_poverty = mean(county_per_poverty)
  )
#For the top 5 and bottom 5 poverty rate counties
top_bottom_poverty_counties <- counties %>%
  arrange(county_per_poverty) %>%
  slice(c(1:5, n() - 4:n()))

# Print the top and bottom counties
print(top_bottom_poverty_counties)


#Task 6: Data Visualization
# Bar plot of average test performance by poverty level
ggplot(counties, aes(x = poverty_level, y = math_z_score)) +
  geom_col(stat = "summary", fun = "mean", fill = "blue") +
  labs(
    x = "Poverty Level",
    y = "Average Math Score",
    title = "Average Math Score Across Counties by Poverty Level"
  )


# Scatter plot
ggplot(schools_all, aes(x = per_free_lunch, y = ela_z_score)) +
  geom_point(col="pink") +
  labs(
    x = "Percent of Students with Free/Reduced Lunch",
    y = "ELA Test Performance (Z-Score)",
    title = "Relationship Between Free/Reduced Lunch and ELA Test Performance"
  ) +
  scale_x_continuous(limits = c(0, 1)) 



