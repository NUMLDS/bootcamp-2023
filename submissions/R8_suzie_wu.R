nys_school<-read_csv("/Users/apple/Desktop/bootcamp/bootcamp-2023/data/nys_schools.csv")
nys_acs <- read.csv("/Users/apple/Desktop/bootcamp/bootcamp-2023/data/nys_acs.csv")                      

summary(nys_school)
summary(nys_acs)
nys_school<-nys_school %>% filter(county_name != -99)
for(i in 1:nrow(nys_school))if(nys_school$total_enroll[i]==-99) {nys_school$total_enroll[i]=mean(nys_school$total_enroll[nys_school$total_enroll!=-99],)}
for(i in 1:nrow(nys_school))if(nys_school$mean_math_score[i]==-99) {nys_school$mean_math_score[i]=mean(nys_school$mean_math_score[nys_school$mean_math_score!=-99],)}
for(i in 1:nrow(nys_school))if(nys_school$mean_ela_score[i]==-99) {nys_school$mean_ela_score[i]=mean(nys_school$mean_ela_score[nys_school$mean_ela_score!=-99],)}
for(i in 1:nrow(nys_school))if(nys_school$per_free_lunch[i]==-99) {nys_school$per_free_lunch[i]=mean(nys_school$per_free_lunch[nys_school$per_free_lunch!=-99],)}
for(i in 1:nrow(nys_school))if(nys_school$per_reduced_lunch [i]==-99) {nys_school$per_reduced_lunch [i]=mean(nys_school$per_reduced_lunch [nys_school$per_reduced_lunch !=-99],)}
for(i in 1:nrow(nys_school))if(nys_school$per_lep [i]==-99) {nys_school$per_lep [i]=mean(nys_school$per_lep [nys_school$per_lep !=-99],)}

nys_acs <- nys_acs %>%
  mutate(
    category = case_when(
      county_per_poverty <=  0.04689 ~ "low",
      county_per_poverty > 0.04689 &  county_per_poverty < 0.14929 ~ "medium",
      county_per_poverty >=0.14929  ~ "high"
    )
  )

nys_school$mean_ela_score<-scale(nys_school$mean_ela_score)
nys_school$mean_math_score<-scale(nys_school$mean_math_score)

ny_merged<-merge(nys_school, nys_acs,by = "county_name",all.x = TRUE)