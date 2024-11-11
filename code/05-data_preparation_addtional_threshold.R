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



dat_survey <- read_csv("dat_survey.csv")

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


organic_video_02 <- organic_video %>%
  mutate(organic_video_w123 = rowSums(.[, c("organic_video_w1", "organic_video_w23")], na.rm = TRUE)) %>%
  # keep users who have at least 1 organic visit in week 2 & 3
  # filter(organic_video_w23 > 0)
  # keep users who have more than 5 videos in the first 3 weeks and who have organic visits in week 2 & 3
  filter(organic_video_w123 >= 5 & organic_video_w23 > 0)



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
#write.csv(final_video_survey, file = "final_video_survey_additional_1.csv", row.names = F)
write.csv(final_video_survey, file = "final_video_survey_additional_5.csv", row.names = F)

