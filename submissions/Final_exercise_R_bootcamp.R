#Load packages 
library(dplyr)
library(ggplot2)

#Set your working dirextory 
setwd("~/Documents/Northwestern/R_bootcamp/bootcamp-2023-main")

#load the data 
nyc_schools <- read.csv("data/nys_schools.csv")
nyc_acs <- read.csv("data/nys_acs.csv")

# Explore the data frames 
str(nyc_schools)
nrow(nyc_schools)

str(nyc_acs)
nrow(nyc_acs)
 
sum(is.na(nyc_schools))
sum(is.na(nyc_acs))

summary(nyc_schools)
summary(nyc_acs)

# Replace missing data -99 with NA
nyc_schools_2 <- replace(nyc_schools, nyc_schools ==-99, NA)
sum(is.na(nyc_schools_2))
summary(nyc_schools_2)

# create new categorical variable for counties 
nyc_acs <- nyc_acs %>% 
    mutate(poverty_level = case_when(county_per_poverty <= 0.1090 ~ "low", 
                                     county_per_poverty > 0.1090 & county_per_poverty < 0.1493 ~ "median",
                                     county_per_poverty >= 0.1493 ~ "high"))
table(nyc_acs$poverty_level)

# Run a z-test for the math and ELA 
scale_scores <- nyc_schools_2 %>% 
    group_by("year") %>% 
    mutate(z1 = scale(mean_ela_score, center = TRUE, scale = TRUE), z2 = scale(mean_math_score, center = TRUE, scale = TRUE))

hist(scale_scores$z1)
hist(scale_scores$z2)

# merge the 2 datasets 
merged_data <- merge(scale_scores,nyc_acs)

# Analyze data 
merged_data %>% 

# visualization
plot(merged_data$total_enroll, )

merged_data %>% 
ggplot() + 
    geom_line(aes(x = year, y = povertylevel)) + 
    labs(title="Powerty level over time ", x="Year", y="Poverty") 
   
