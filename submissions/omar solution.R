

library(tidyverse)
library(here)
library(reshape2)

ny_schools <- read_csv(here("Documents/data/nys_schools.csv"))
ny_survey <- read_csv(here("Documents/data/nys_acs.csv"))

summary(ny_schools)

joined_data <- inner_join(ny_schools, ny_survey, by = c('county_name', 'year'))

summary(joined_data)

num_cols <- sapply(joined_data, is.numeric)
numerics <- names(joined_data)[num_cols]

joined_data %>%
  ggplot() +
  geom_density(aes(x = total_enroll)) +
  facet_wrap(~year)

summary(joined_data)
cleaned_cols = c('total_enroll', 'per_free_lunch', 'per_reduced_lunch',
                 'per_lep', 'mean_ela_score', 'mean_math_score') 


filtered_df <- joined_data %>%
  filter(across(all_of(numerics), ~ . > 0))

filtered_df %>%
  ggplot() +
  geom_histogram(aes(x = county_per_poverty)) 
  #+ facet_wrap(~year)

#ctrl shift m gets auto pipe %>% %>% %>% 

# Calculate the percentiles for the 'Value' column
percentile_25 <- quantile(filtered_df$county_per_poverty, probs = 0.25)
percentile_75 <- quantile(filtered_df$county_per_poverty, probs = 0.75)

# Create a new column 'Category' based on percentiles
filtered_df$poverty_class <- cut(filtered_df$county_per_poverty,
                   breaks = c(-Inf, percentile_25, percentile_75, Inf),
                   labels = c("Low", "Medium", "High"),
                   include.lowest = TRUE)

'Generate a few summary tables to help answer the questions you were originally asked. For example:
For each county: total enrollment, percent of students qualifying for free or reduced price lunch,
and percent of population in poverty. For the counties with the top 5 and bottom 5 poverty rate: 
percent of population in poverty, percent of students qualifying for free or reduced price lunch,
mean reading score, and mean math score.'

filtered_df %>% 
  group_by(county_name) %>% 
  summarise(output = mean(county_per_poverty)) %>% 
  ggplot() +
  geom_density(aes(x = output))


filtered_df %>% 
  ggplot() +
  geom_density(aes(x = county_per_poverty), fill = 'black') +
  facet_wrap(~ county_name)

