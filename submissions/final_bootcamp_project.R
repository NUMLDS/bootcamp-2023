# Task 1: Import your data
# Read the data files nys_schools.csv and nys_acs.csv into R. These data come from two different sources: one is data on schools in New York state from the New York State Department of Education, and the other is data on counties from the American Communities Survey from the US Census Bureau. Review the codebook file so that you know what each variable name means in each dataset.

# Prep data
nys_schools <- read_csv(here("data/nys_schools.csv"))
View(nys_schools)
nys_acs <- read_csv(here("data/nys_acs.csv"))
View(nys_acs)

# Task 2: Explore your data
# Getting to know your data is a critical part of data analysis. Take the time to explore the structure of the two dataframes you have imported. What types of variables are there? Is there any missing data? How can you tell? What else do you notice about the data?
summary(nys_schools)
summary(nys_acs)
    
# Task 3: Recoding and variable manipulation
# Deal with missing values, which are currently coded as -99.
nys_schools_has_missing <- apply(nys_schools, 1, function(row) any(row == -99))
nys_schools_clean <- nys_schools[-which(nys_schools_has_missing),]
nrow(nys_schools_clean)

nys_acs_has_missing <- apply(nys_acs, 1, function(row) any(row == -99))
which(nys_acs_has_missing)
nys_acs_clean <- nys_acs

# Create a categorical variable that groups counties into "high", "medium", and "low" poverty groups. Decide how you want to split up the groups and briefly explain your decision.
summary(nys_schools_clean$median_household_income)
quantiles <- quantile(nys_schools_clean$median_household_income, probs = c(0, 1/3, 2/3, 1))
nys_schools_clean$poverty_group <- cut(nys_schools_clean$median_household_income, breaks = quantiles, labels = c("low", "medium", "high"))

# The tests that the NYS Department of Education administers changes from time to time, so scale scores are not directly comparable year-to-year. 
# Create a new variable that is the standardized z-score for math and English Language Arts (ELA) for each year (hint: group by year and use the scale() function)
scores_std <- nys_schools_clean %>% 
  select(year, contains("score")) %>% 
  group_by(year) %>% 
  summarise(ela_mean=mean(mean_ela_score, na.rm=TRUE),
            ela_sd=sd(mean_ela_score, na.rm=TRUE),
            math_score_mean=mean(mean_math_score, na.rm=TRUE),
            math_score_sd=sd(mean_math_score, na.rm=TRUE))


# Task 4: Merge datasets
# Create a dataset that merges variables from the schools dataset and the ACS dataset. Remember that you have learned multiple approaches on how to do this, and that you will have to decide how to combine the two data sets.
colnames(nys_schools_clean)
colnames(scores_std)
schools_all <- inner_join(nys_schools_clean, scores_std, by="year")
colnames(schools_all)
schools_all <- mutate(schools_all,
                     ela_z_score = (mean_ela_score-ela_mean)/ela_sd,
                     math_z_score = (mean_math_score-math_score_mean)/math_score_sd)
