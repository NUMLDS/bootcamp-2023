# Load packages
library(tidyverse)
library(here)
library(reshape2)
library(dplyr)

## read data files
nys_school = read_csv(here("data/nys_schools.csv"))
nys_acs = read_csv(here("data/nys_acs.csv"))

summary(nys_school) #school has a lot of -99 minimums
summary(nys_acs)

## check missing data
names(which(colSums(is.na(nys_school)) > 0))
names(which(colSums(is.na(nys_acs)) > 0))

## check negative values in nys_school dataset
nrow(nys_school[nys_school$district_name<=0,]) ##1725

## count number of na in column(s)
sum(is.na(nys_school$district_name)) ##1706

## omit NAs and -99s
nys_school = na.omit(nys_school)
nys_school = nys_school[nys_school$district_name >= 0, ]


## create a categorical value that groups counties into high, medium and low
nys_acs = nys_acs %>%
  mutate(counties_group = case_when(county_per_poverty < quantile(county_per_poverty, 1/3) ~ 'low',
                                    county_per_poverty >= quantile(county_per_poverty, 1/3) & county_per_poverty <= quantile(county_per_poverty, 2/3) ~ 'med',
                                    county_per_poverty > quantile(county_per_poverty, 2/3) ~ 'high'))

## merge datasets
merged = merge(nys_school, nys_acs, by.nys_school = c("county_name","year"), by.nys_acs = c("name","year"))

merged %>%
  group_by("county_name") %>%
  select("total_enroll","per_free_lunch","per_reduced_lunch","")

ggplot(nys_school) + 
  geom_point(aes(x=per_free_lunch,y=mean_ela_score))
