library(tidyverse)
library(here)
library(dplyr)
library(reshape2)
school = read.csv(here::here("data/nys_schools.csv"))
acs = read.csv(here::here("data/nys_acs.csv"))

school[school==-99] <- NA
school <- na.omit(school)
acs <- na.omit (acs)


school<-school %>%
    mutate(mean_score=(mean_ela_score+mean_math_score)/2)

mean_byTime = school %>%
    group_by(year) %>%
    summarise(mean_byTime=mean(mean_score))

sd_byTime = school %>%
    group_by(year) %>%
    summarise(sd_byTime=sd(mean_score))

school["level"]<-"medium"
school$level[school$mean_score>mean_byTime+sd_byTime]="high"
school$level[school$mean_score<mean_byTime-sd_byTime]="low"

mean_income_byTime = acs %>%
    group_by(year) %>%
    summarise(mean_income_byTime=mean(median_household_income))

sd_income_byTime = acs %>%
    group_by(year) %>%
    summarise(sd_income_byTime=sd(median_household_income))
acs<- left_join(left_join(acs,mean_income_byTime,by="year"),sd_income_byTime,by="year")
acs["poverty_level"]<-"medium"
acs$poverty_level[acs$median_household_income>acs$mean_income_byTime+acs$sd_income_byTime]="low"
acs$poverty_level[acs$median_household_income<acs$mean_income_byTime-acs$sd_income_byTime]="high"

final = left_join(school,acs,by=c("county_name","year"))
final %>%
    filter(year==2009) %>%
    ggplot(data = .data) + 
    geom_point(mapping = aes(x = poverty_level, y = mean_score))
