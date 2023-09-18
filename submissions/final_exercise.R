#import the data
nys_schools <- read.csv("/Users/lky/data/nys_schools.csv")
nys_acs<- read.csv("/Users/lky/data/nys_acs.csv")

#data manipulation
#filter the values we needed
nys_schools %>% filter_all(any_vars(. %in% c(-99)))
nys_schools_2<- na_if(nys_schools, -99)
#test whether the new df works
nys_schools_2 %>% filter_all(any_vars(. %in% c(-99))) #yes,it works

#create categorical variables 
#first quarter=0.10903;median=0.12884;3rd quarter=0.14929
nys_acs$county_poverty_level <- NA  # Initialize a variable containing all "NA" values
nys_acs$county_poverty_level[nys_acs$county_per_poverty>=0.14929] <- "High"
nys_acs$county_poverty_level[nys_acs$county_per_poverty<0.14929&nys_acs$county_per_poverty>0.10903] <- "Medium"
nys_acs$county_poverty_level[nys_acs$county_per_poverty<=0.10903] <- "Low"
table(nys_acs$county_poverty_level)

#compute z-score
# Make calculation manually
scores_std <- nys_schools_2 %>%
  select(year, contains("score")) %>%
  group_by(year) %>%
  summarize(ela_mean = mean(mean_ela_score, na.rm=TRUE),
            math_mean = mean(mean_math_score, na.rm=TRUE),
            ela_sd = sd(mean_ela_score, na.rm=TRUE),
            math_sd = sd(mean_math_score, na.rm=TRUE))

schools_all = inner_join( nys_schools_2,scores_std, by="year")
schools_all = mutate(schools_all,
                     ela_z_score = (mean_ela_score-ela_mean)/ela_sd,
                     math_z_score = (mean_math_score-math_mean)/math_sd)

View(schools_all)
View(filter(schools_all, year==2017))
view(scores_std)
