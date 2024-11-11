library(dplyr)
library(arrow)
library(readr)
library(lubridate)
library(jsonlite)
library(stringr)
library(tidyr)
library(psych)
library(AER)
library(stargazer)
library(ggplot2)
library(scales)
library(ggpubr)

setwd("")
figdir <- ""

#########################
#####
#####               import survey data
#####
#########################


dat_survey <- read_csv("survey_data.csv") %>%
## recode demographics variables
  mutate(age = 2022 - birthyr,
         race = if_else(race == 1, 'white', 'non-white'),
         gender = if_else(gender4 == 1, 'male', 'non-male'),
         edu = case_when(
           educ <= 2 ~ 'low',
           educ >= 3 & educ <= 5 ~ 'middle',
           educ == 6 ~ 'high'),
         party = case_when(
           pid7 >= 1 & pid7 <= 3 ~ "Democrat",
           pid7 == 4 | pid7 >= 8 | is.na(pid7) ~ "Other",
           pid7 >= 5 & pid7 <= 7 ~ "Republican"), 
         ideology =  case_when(
           ideo5 <= 2 ~ "liberal",
           ideo5 == 4 | ideo5 == 5 ~ "conservative"), 
         
         # create a imputed pid variable
         pid_imputed = case_when(
           party == "Democrat" ~ "Democrat",
           party == "Republican" ~ "Republican",
           party == "Other" & ideo5 <= 2 ~ "Democrat",
           party == "Other" & (ideo5 == 4 | ideo5 == 5) ~ "Republican"),
         pid_imputed = case_when(
           is.na(pid_imputed) & presvote20post == 1 ~ "Democrat",
           is.na(pid_imputed) & presvote20post == 2 ~ "Republican",
           TRUE ~ pid_imputed),
         
         # create pid strength
         pid_strength = if_else(pid7 <= 7, abs(pid7-4)+1, 1),
         ideo_strength = if_else(ideo5 <= 5, abs(ideo5-3)+1, NA_real_),
         party_no_imputation = case_when(
           party == "Democrat" ~ "Democrat",
           party == "Republican" ~ "Republican")
         ) %>%
  mutate_at(vars(#NEWSFOL_1_W2, NEWSFOL_2_W2, NEWSFOL_3_W2, NEWSFOL_4_W2,
                 Q3_1_12_21_22, Q3_2_12_21_22, Q3_3_12_21_22,
                 Q3_4_12_28_22, Q3_5_12_28_22, Q3_6_12_28_22,
                 Q3_4_01_09_23, Q3_5_01_09_23, Q3_6_01_09_23,
                 Q3_4_12_21_22, Q3_5_12_21_22, Q3_6_12_21_22,
                 Q3_1_12_28_22, Q3_2_12_28_22, Q3_3_12_28_22,
                 Q3_1_01_09_23, Q3_2_01_09_23, Q3_3_01_09_23,
                 POLPART_1_W2, POLPART_2_W2, POLPART_3_W2, POLPART_4_W2, POLPART_5_W2, POLPART_6_W2, POLPART_8_W2,
                 PERCEIVEDPOL1_1_W2, PERCEIVEDPOL1_2_W2, PERCEIVEDPOL1_3_W2, PERCEIVEDPOL1_4_W2,
                 PERCEIVEDPOL2_1_W2, PERCEIVEDPOL2_2_W2, PERCEIVEDPOL2_3_W2, PERCEIVEDPOL2_4_W2,
                 PARTISANENDS_1_new_W2, PARTISANENDS_2_new_W2, PARTISANENDS_3_new_W2, PARTISANENDS_4_new_W2, PARTISANENDS_5_new_W2
                 ), ~replace(., . == -1, NA)) %>%
  #######################recode some variables (9 = no data)
  mutate_at(vars(POLPART_1, POLPART_2, POLPART_3, POLPART_4, POLPART_5, POLPART_6, POLPART_8,
                 IDEOLOGY_GROUP_1, IDEOLOGY_GROUP_2, IDEOLOGY_GROUP_3, IDEOLOGY_GROUP_4,
                 PERCEIVEDPOL2_1, PERCEIVEDPOL2_2, PERCEIVEDPOL2_3, PERCEIVEDPOL2_4,
                 PARTISANENDS_1_new, PARTISANENDS_2_new, PARTISANENDS_3_new, PARTISANENDS_4_new, PARTISANENDS_5_new,
                 PARTISANENDS_1_new_W2, PARTISANENDS_2_new_W2, PARTISANENDS_3_new_W2, PARTISANENDS_4_new_W2, PARTISANENDS_5_new_W2), ~replace(., . == 9, NA))
  

## recode variables
dat_survey <- dat_survey %>%
  mutate_at(vars(FT_PEOPLEGROUPS_1, FT_PEOPLEGROUPS_2, FT_PEOPLEGROUPS_3, FT_PEOPLEGROUPS_4,
                 FT_PEOPLEGROUPS_1_W2, FT_PEOPLEGROUPS_2_W2, FT_PEOPLEGROUPS_3_W2, FT_PEOPLEGROUPS_4_W2), as.numeric) %>%
  mutate(
    
    # Perceived accuracy of claims about current events 
    perceived_acc_false = case_when(
      knowledge_version == 1 ~ rowMeans(select(., Q3_1_12_21_22, Q3_2_12_21_22, Q3_3_12_21_22), na.rm = T),
      knowledge_version == 2 ~ rowMeans(select(., Q3_4_12_28_22, Q3_5_12_28_22, Q3_6_12_28_22), na.rm = T),
      knowledge_version == 3 ~ rowMeans(select(., Q3_4_01_09_23, Q3_5_01_09_23, Q3_6_01_09_23), na.rm = T),
    ),
    perceived_acc_true = case_when(
      knowledge_version == 1 ~ rowMeans(select(., Q3_4_12_21_22, Q3_5_12_21_22, Q3_6_12_21_22), na.rm = T),
      knowledge_version == 2 ~ rowMeans(select(., Q3_1_12_28_22, Q3_2_12_28_22, Q3_3_12_28_22), na.rm = T),
      knowledge_version == 3 ~ rowMeans(select(., Q3_1_01_09_23, Q3_2_01_09_23, Q3_3_01_09_23), na.rm = T),
    ),
    perceived_acc_diff = perceived_acc_true - perceived_acc_false,
    
    # Intended Political Participation
    pol_part_pre = if_else(is.na(POLPART_8), NA_real_, rowSums(select(., POLPART_1, POLPART_2, POLPART_3, POLPART_4, POLPART_5, POLPART_6) == 1)),
    pol_part_post = if_else(is.na(POLPART_8_W2), NA_real_, rowSums(select(., POLPART_1_W2, POLPART_2_W2, POLPART_3_W2, POLPART_4_W2, POLPART_5_W2, POLPART_6_W2) == 1)),
    
    # Affective Polarization
    ap_leader_pre = case_when(
      pid_imputed == "Democrat" ~ FT_PEOPLEGROUPS_1 - FT_PEOPLEGROUPS_2, 
      pid_imputed == "Republican" ~ FT_PEOPLEGROUPS_2 - FT_PEOPLEGROUPS_1),
    ap_politician_pre = case_when(
      pid_imputed == "Democrat" ~ FT_PEOPLEGROUPS_3 - FT_PEOPLEGROUPS_4, 
      pid_imputed == "Republican" ~ FT_PEOPLEGROUPS_4 - FT_PEOPLEGROUPS_3),
    ap_leader_post = case_when(
      pid_imputed == "Democrat" ~ FT_PEOPLEGROUPS_1_W2 - FT_PEOPLEGROUPS_2_W2, 
      pid_imputed == "Republican" ~ FT_PEOPLEGROUPS_2_W2 - FT_PEOPLEGROUPS_1_W2),
    ap_politician_post = case_when(
      pid_imputed == "Democrat" ~ FT_PEOPLEGROUPS_3_W2 - FT_PEOPLEGROUPS_4_W2, 
      pid_imputed == "Republican" ~ FT_PEOPLEGROUPS_4_W2 - FT_PEOPLEGROUPS_3_W2),
    
    # Perceived Polarization
    perceived_pol_politician_pre = abs(IDEOLOGY_GROUP_1 - IDEOLOGY_GROUP_2),
    perceived_pol_supporter_pre = abs(IDEOLOGY_GROUP_3 - IDEOLOGY_GROUP_4),
    perceived_pol_scale_pre = rowMeans(select(., PERCEIVEDPOL2_1, PERCEIVEDPOL2_2, PERCEIVEDPOL2_3, PERCEIVEDPOL2_4), na.rm = T),
    
    perceived_pol_politician_post = abs(PERCEIVEDPOL1_1_W2 - PERCEIVEDPOL1_2_W2),
    perceived_pol_supporter_post = abs(PERCEIVEDPOL1_3_W2 - PERCEIVEDPOL1_4_W2),
    perceived_pol_scale_post = rowMeans(select(., PERCEIVEDPOL2_1_W2, PERCEIVEDPOL2_2_W2, PERCEIVEDPOL2_3_W2, PERCEIVEDPOL2_4_W2), na.rm = T),
    
    #Prioritizing Partisan Ends Over Democratic Means
    partisan_ends_pre = rowMeans(select(., PARTISANENDS_1_new, PARTISANENDS_2_new, PARTISANENDS_3_new, PARTISANENDS_4_new, PARTISANENDS_5_new), na.rm = T),
    partisan_ends_post = rowMeans(select(., PARTISANENDS_1_new_W2, PARTISANENDS_2_new_W2, PARTISANENDS_3_new_W2, PARTISANENDS_4_new_W2, PARTISANENDS_5_new_W2), na.rm = T)
  )


dat_survey <- dat_survey %>%
  # make missing values consistent
  mutate(across(perceived_acc_false:partisan_ends_post, ~ replace(., is.nan(.) | is.na(.), NA))) %>%
  mutate(across(perceived_acc_false:partisan_ends_post, rescale) *100)


write.csv(dat_survey, file = "dat_survey.csv", row.names = F)


#########################
#####
#####               import YT data
#####
#########################
data_urls <- read_csv("yt_urls.csv")

## calculate N of videos watched, N of active days
organic_video_w1 <- data_urls %>%
  filter(is_injected == FALSE & is_video == TRUE & intervention_stage == 'PRE') %>%
  group_by(visaId) %>%
  summarise(organic_video_w1 = n(), active_day_w1 = length(unique(day))) %>%
  ungroup()

organic_video_w23 <- data_urls %>%
  filter(is_injected == FALSE & is_video == TRUE & intervention_stage == 'MID') %>%
  group_by(visaId) %>%
  summarise(organic_video_w23 = n(), active_day_w23 = length(unique(day))) %>%
  ungroup()

organic_video_w4 <- data_urls %>%
  filter(is_injected == FALSE & is_video == TRUE & intervention_stage == 'POST') %>%
  group_by(visaId) %>%
  summarise(organic_video_w4 = n(), active_day_w4 = length(unique(day))) %>%
  ungroup()

# merge the 3 tables and compute the N of videos watched per active day in week 1, 2 & 3, and 4
organic_video <- organic_video_w1 %>%
  full_join(organic_video_w23, by = "visaId") %>%
  full_join(organic_video_w4, by = "visaId") %>%
  mutate_at(vars(-visaId), ~replace_na(., 0)) %>%
  mutate(average_organic_video_w1 = organic_video_w1/active_day_w1,
         average_organic_video_w23 = organic_video_w23/active_day_w23,
         average_organic_video_w4 = organic_video_w4/active_day_w4)

# keep users who have more than 7 videos in the first 3 weeks and who have organic visits in week 2 & 3
organic_video_02 <- organic_video %>%
  mutate(organic_video_w123 = rowSums(.[, c("organic_video_w1", "organic_video_w23")], na.rm = TRUE)) %>%
  filter(organic_video_w123 >= 7 & organic_video_w23 > 0)



## calculate N of news videos watched in each stage
organic_news <- data_urls %>%
  filter(news == "Y" & is_injected == FALSE) %>%
  group_by(visaId, intervention_stage) %>%
  summarise(organic_news = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = intervention_stage, values_from = organic_news)%>%
  rename(organic_news_w1 = PRE,
         organic_news_w23 = MID,
         organic_news_w4 = POST) %>%
  select(visaId, organic_news_w1, organic_news_w23, organic_news_w4) %>%
  mutate_at(vars(-visaId), ~replace_na(., 0)) 


## calculate N of pol non-news videos watched in each stage
organic_pol_non_news <- data_urls %>%
  filter(pol_prediction == 1 & is_injected == FALSE & news == "N") %>%
  group_by(visaId, intervention_stage) %>%
  summarise(organic_pol_non_news = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = intervention_stage, values_from = organic_pol_non_news)%>%
  rename(organic_pol_non_news_w1 = PRE,
         organic_pol_non_news_w23 = MID,
         organic_pol_non_news_w4 = POST) %>%
  select(visaId, organic_pol_non_news_w1, organic_pol_non_news_w23, organic_pol_non_news_w4) %>%
  mutate_at(vars(-visaId), ~replace_na(., 0))

## calculate N of problematic videos watched in each stage
organic_problem <- data_urls %>%
  filter(problem == "Y" & is_injected == FALSE) %>%
  group_by(visaId, intervention_stage) %>%
  summarise(organic_problem = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = intervention_stage, values_from = organic_problem)%>%
  rename(organic_problem_w1 = PRE,
         organic_problem_w23 = MID,
         organic_problem_w4 = POST) %>%
  select(visaId, organic_problem_w1, organic_problem_w23, organic_problem_w4) %>%
  mutate_at(vars(-visaId), ~replace_na(., 0))

## combine organic video and organic news & organic pol non-news data
organic_video_03 <- organic_video_02 %>%
  left_join(organic_news, by = "visaId") %>%
  ## merge with organic pol non news data
  left_join(organic_pol_non_news, by = "visaId") %>%
  ## merge with organic problematic data 
  left_join(organic_problem, by = "visaId") %>%
  mutate_at(vars(organic_news_w1, organic_news_w23, organic_news_w4, organic_pol_non_news_w1, organic_pol_non_news_w23, organic_pol_non_news_w4,
                 organic_problem_w1, organic_problem_w23, organic_problem_w4), ~replace_na(., 0)) %>%
  # n of news videos watched per active day
  mutate(average_organic_news_w1 = organic_news_w1/active_day_w1,
         average_organic_news_w23 = organic_news_w23/active_day_w23,
         average_organic_news_w4 = organic_news_w4/active_day_w4,
         # diff in w23 and w1
         organic_news_diff = organic_news_w23/2 - organic_news_w1,
         #% of news videos in each stage
         per_organic_news_w1 = organic_news_w1/organic_video_w1*100,
         per_organic_news_w23 = organic_news_w23/organic_video_w23*100,
         per_organic_news_w4 = organic_news_w4/organic_video_w4*100,
         # n of pol non-news videos watched per active day
         average_organic_pol_non_news_w1 = organic_pol_non_news_w1/active_day_w1,
         average_organic_pol_non_news_w23 = organic_pol_non_news_w23/active_day_w23,
         average_organic_pol_non_news_w4 = organic_pol_non_news_w4/active_day_w4,
         #% of pol non-news videos in each stage
         per_organic_pol_non_news_w1 = organic_pol_non_news_w1/organic_video_w1*100,
         per_organic_pol_non_news_w23 = organic_pol_non_news_w23/organic_video_w23*100,
         per_organic_pol_non_news_w4 = organic_pol_non_news_w4/organic_video_w4*100,
         # n of problematic videos watched per active day
         average_organic_problem_w1 = organic_problem_w1/active_day_w1,
         average_organic_problem_w23 = organic_problem_w23/active_day_w23,
         average_organic_problem_w4 = organic_problem_w4/active_day_w4,
         #% of pol non-news videos in each stage
         per_organic_problem_w1 = organic_problem_w1/organic_video_w1*100,
         per_organic_problem_w23 = organic_problem_w23/organic_video_w23*100,
         per_organic_problem_w4 = organic_problem_w4/organic_video_w4*100,
         )

organic_video_04 <- data_urls %>%
  select(visaId, intervention) %>%
  distinct() %>%
  right_join(organic_video_03, by = "visaId")

rm(organic_video, organic_video_02, organic_video_03, organic_video_w1, organic_video_w23, organic_video_w4, 
   organic_news, organic_pol_non_news, organic_problem)



## n of news videos added
news_added <- data_urls %>%
  filter(intervention == "background" & intervention_stage == "MID" & is_video == TRUE & is_injected == TRUE) %>%
  group_by(visaId) %>%
  summarise(news_added = n(), news_added_day = length(unique(day))) %>%
  ungroup()

organic_video_04 <- organic_video_04 %>%
  left_join(news_added, by = "visaId") %>%
  # replace nan with 0s for those in the user nudge and control conditions
  mutate_at(vars(news_added), ~ replace_na(., 0)) %>%
  #% of news videos added/all videos added and watched in w2-3
  mutate(
    per_news_added = news_added/(news_added + organic_video_w23)*100
  )

#########################
#####
#####               import recommendation data
#####
#########################
dat_recommendations_02 <- read_csv("dat_recommendations_02.csv")
# calculate the % of news 

recommendation_news <- dat_recommendations_02 %>%
  group_by(visaId, intervention_stage) %>%
  summarise(per_rec_news = mean(news == "Y")*100) %>%
  ungroup() %>%
  pivot_wider(names_from = intervention_stage, values_from = per_rec_news)%>%
  rename(per_rec_news_w1 = PRE,
         per_rec_news_w23 = MID,
         per_rec_news_w4 = POST) 

# combine it with other data
final_video <- left_join(organic_video_04, recommendation_news, by = "visaId")

## combine video & news data with survey data
final_video_survey <- left_join(final_video %>% select(-intervention),  dat_survey, by ='visaId')


##save the dataset
write.csv(final_video_survey, file = "final_video_survey.csv", row.names = F)



## calculate the % of news watched and recommended by day
news_watched_day <- data_urls %>%
  filter(is_video == TRUE & is_injected == FALSE) %>%
  filter(visaId %in% organic_video_04$visaId) %>%
  group_by(visaId, day) %>%
  summarise(per_watched_news = mean(news == "Y")*100) %>%
  ungroup() %>%
  pivot_wider(names_from = day, values_from = per_watched_news, names_prefix = "day_")
  
news_recommended_day <- dat_recommendations_02 %>%
  group_by(visaId, day) %>%
  summarise(per_rec_news = mean(news == "Y")*100) %>%
  ungroup() %>%
  pivot_wider(names_from = day, values_from = per_rec_news, names_prefix = "day_")


news_added_day <- data_urls %>%
  filter(intervention == "background" & intervention_stage == "MID" & is_video == TRUE) %>%
  filter(visaId %in% organic_video_04$visaId) %>%
  group_by(visaId, day) %>%
  summarise(per_added_news = mean(is_injected == TRUE)*100) %>%
  ungroup() %>%
  pivot_wider(names_from = day, values_from = per_added_news, names_prefix = "day_")


# reorder the datasets by day
news_watched_day <- news_watched_day[, c("visaId", names(news_watched_day)[-1][order(as.numeric(sub("day_", "", names(news_watched_day)[-1])))])] %>%
  select(visaId, day_0:day_27) 

news_recommended_day <- news_recommended_day[, c("visaId", names(news_recommended_day)[-1][order(as.numeric(sub("day_", "", names(news_recommended_day)[-1])))])] %>%
  select(visaId, day_0:day_27)

news_added_day <- news_added_day[, c("visaId", names(news_added_day)[-1][order(as.numeric(sub("day_", "", names(news_added_day)[-1])))])] %>%
  select(visaId, day_2:day_22)

write.csv(news_watched_day, file = "news_watched_day.csv", row.names = F)
write.csv(news_recommended_day, file = "news_recommended_day.csv", row.names = F)
write.csv(news_added_day, file = "news_added_day.csv", row.names = F)

