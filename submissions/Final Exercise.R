# Final Bootcamp Project

# Task 1. import dataseet:
nys_schools <- read.csv("nys_schools.csv", stringsAsFactors=FALSE)
nys_acs <- read.csv("nys_acs.csv", stringsAsFactors=FALSE)

# Tasl 2. Explore Data
summary(nys_schools)
summary(nys_acs)

# Task 3.1 Replace missing data:
# Check number of missing data:
sum(nys_schools == -99)   
sum(nys_acs == -99)  
# Replace missing data:
missing <- apply(nys_schools,1,function(row) any(row==-99))
nys_schools <-nys_schools[-which(missing),]

# Task 3.2
nys_acs$rank_poverty[nys_acs$county_per_poverty<=0.10903] <- "Low"
nys_acs$rank_poverty[(nys_acs$county_per_poverty<0.14929)&(nys_acs$county_per_poverty>0.10903)] <- "Medium"
nys_acs$rank_poverty[nys_acs$county_per_poverty>= 0.14929] <- "High"

# Task 3.3
group_by(nys_schools$year)
zscore<-nys_schools %>% 
  group_by(year) %>% 
  mutate(zscore_math=(mean_math_score-mean(mean_math_score))/sd(mean_math_score)) %>% 
  mutate(zscore_ela=(mean_ela_score-mean(mean_ela_score))/sd(mean_ela_score)) 
  
# Task 4. Merge datasets
merged_df <- merge(nys_acs, zscore, by.x = c("county_name","year"), by.y = c("county_name","year"),all.x = TRUE)
merged_df

#Task 5.1
long_merged_energy %>%
  ggplot() + 
  geom_line(aes(x=datetime, y=output, group=source, col=source)) + 
  labs(title="Output by energy source over time", subtitle="Hourly data from September 3-9, 2018", x="Hour", y="Output (MW)")

#Create a line plot that compares generation of wind, solar, and geothermal energy over time.
# Bonus: Set the line size to 1.5.
long_merged_energy %>%
  filter(source=="wind"|source=="solar"|source=="geothermal") %>% 
  ggplot() + 
  geom_line(aes(x=datetime, y=output, group=source, col=source), size=1.5) + 
  labs(title="Wind vs. Solar vs. Geothermal generation", subtitle="Hourly data from September 3-9, 2018", x="Hour", y="Output (MW)")

