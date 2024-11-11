library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(lme4)
library(lmerTest)
library(xtable)

setwd("")

news_watched_day <- read_csv("news_watched_day.csv")
news_recommended_day <- read_csv("news_recommended_day.csv")
news_added_day <- read_csv("news_added_day.csv")
final_video_survey <- read_csv("final_video_survey.csv")
# Reshape the data from wide to long format and Remove rows with missing values
news_recommended_day_long <- news_recommended_day %>%
  pivot_longer(-visaId, names_to = "day", values_to = "recommended") %>%
  filter(!is.na(recommended))

news_watched_day_long <- news_watched_day %>%
  pivot_longer(-visaId, names_to = "day", values_to = "watched") %>%
  filter(!is.na(watched))

news_added_day_long <- news_added_day %>%
  pivot_longer(-visaId, names_to = "day", values_to = "added") %>%
  filter(!is.na(added))

news_r_w_day_long <- inner_join(news_recommended_day_long, news_watched_day_long, by = c("visaId", "day")) %>%
  left_join(news_added_day_long, by = c("visaId", "day")) %>%
  mutate(day = gsub("day_", "", day))

# add condition assignment and demos
news_r_w_day_long <- final_video_survey %>%
  select(visaId, intervention, age, gender, edu, race, party) %>%
  right_join(news_r_w_day_long, by = "visaId")

news_r_w_day_long <- news_r_w_day_long %>%
  group_by(visaId) %>%
  mutate(lagged_recommended = lag(recommended, n = 1),
         lagged_watched = lag(watched, n = 1),
         lagged_added = lag(added, n = 1),
         lagged_added_2 = lagged_added) %>%
  mutate_at(vars(lagged_added_2), ~replace_na(., 0))







# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                  multi-level regression --- all users.                                                                          ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================



# Fit a multi-level model with a lagged dependent variable
model_wa <- lmer(watched ~ age + gender + edu + race + party + lagged_recommended + lagged_watched + relevel(factor(intervention), ref = 'control') + (1 | visaId), data = news_r_w_day_long)
summary(model_wa)

model_rec <- lmer(recommended ~ age + gender + edu + race + party + lagged_recommended + lagged_watched + relevel(factor(intervention), ref = 'control') + (1 | visaId), data = news_r_w_day_long)
summary(model_rec)

## compare the coefs from 2 models
beta1 <- fixef(model_wa)['lagged_recommended'] # coefficient from model 1
se1 <- summary(model_wa)$coefficients[, "Std. Error"]['lagged_recommended']# standard error of coefficient from model 1

beta2 <- fixef(model_rec)['lagged_watched']# coefficient from model 2
se2 <- summary(model_rec)$coefficients[, "Std. Error"]['lagged_watched']# standard error of coefficient from model 2

# Calculate the difference in coefficients
delta_beta <- beta1 - beta2

# Calculate the standard error of the difference
se_delta_beta <- sqrt(se1^2 + se2^2)

# Calculate the z-statistic
z <- delta_beta / se_delta_beta

# Calculate the p-value
p_value <- 2 * (1 - pnorm(abs(z)))

# Output the results
cat("Difference in coefficients:", delta_beta, "\n")
cat("Standard error of difference:", se_delta_beta, "\n")
cat("Z-statistic:", z, "\n")
cat("P-value:", p_value, "\n")



################## MAKE TABLE
model_wa_df <- sjPlot::tab_model(model_wa, show.ci = F, show.se = T) %>%
  sjtable2df::mtab2df(n_models = 1)
model_rec_df <- sjPlot::tab_model(model_rec, show.ci = F, show.se = T) %>%
  sjtable2df::mtab2df(n_models = 1)
model_all_df <- bind_rows(model_wa_df, model_rec_df)


# Convert p-values to asterisks
model_all_df$significance <- ifelse(model_all_df[, "p"] < .001, "***", 
                                   ifelse(model_all_df[, "p"] < .01, "**", 
                                          ifelse(model_all_df[, "p"] < .05, "*", "")))

model_all_df <- model_all_df %>%
  mutate(combined = if_else(row_number() <=12 | (row_number() >=20 & row_number() <= 31), paste0(Estimates, " (", `std. Error`, ") ", significance),
                            Estimates))

model_all_df <- model_all_df %>% 
  mutate(Predictors = dplyr::recode(Predictors,
                                    '(Intercept)' = 'Intercept', 
                                    'age' = 'Age',
                                    `gender [non-male]` = 'Gender (non-male)', 
                                    `edu [low]` = 'Education (low)', 
                                    `edu [middle]` = 'Education (middle)', 
                                    `race [white]` = 'Ethnicity (White)', 
                                    `party [Other]` = 'Party (other)', 
                                    `party [Republican]` = 'Party (Republican)',
                                    'lagged recommended' = 'Prop. of news recommended (lagged)',
                                    'lagged watched' = 'Prop. of news watched (lagged)',
                                    `relevel(intervention, ref= "control")background` = 'Condition (algorithm)',
                                    `relevel(intervention, ref= "control")banner` = 'Condition (user)',
                                    'τ00visaId' = 'τ00 Participant',
                                    'N visaId' = 'N Participant'
                                    )) %>%
  select(Predictors, combined)

model_all_df_wide <- bind_cols(model_all_df[1:19,], model_all_df[20:38, 2]) %>%
  rename('Prop. of news watched' = `combined...2`,
         'Prop. of news recommended' = `combined...3`)

# Convert to LaTeX table
model_all_latex_table <- xtable(model_all_df_wide, caption="Multilevel Regression Testing the Relationship Between News Watching and News Recommendation")

# Print the LaTeX code
print(model_all_latex_table, type = "latex", caption.placement = "top", include.rownames=F,
      add.to.row = list(pos=list(19), command=c("\\hline \n \\multicolumn{2}{l}{Note: *** $<$ .001 ** $<$ .01 * $<$ .05. Lagged denotes data sourced from the previous day.}\n")),)



# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                          multi-level regression --- users in the algorithm condition.                                                                          ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================
news_r_w_day_long_bg <- news_r_w_day_long[which(news_r_w_day_long$intervention == "background"), ]
model_bg <- lmer(recommended ~ age + gender + edu + race + party + lagged_recommended + lagged_watched + lagged_added + (1 | visaId), data = news_r_w_day_long_bg)
summary(model_bg)







################## MAKE TABLE
# Extract coefficients, standard errors, t-values, and p-values
model_bg_df <- sjPlot::tab_model(model_bg, show.ci = F, show.se = T) %>%
  sjtable2df::mtab2df(n_models = 1)

# Convert p-values to asterisks
model_bg_df$significance <- ifelse(model_bg_df[, "p"] < .001, "***", 
                                   ifelse(model_bg_df[, "p"] < .01, "**", 
                                          ifelse(model_bg_df[, "p"] < .05, "*", "")))

model_bg_df <- model_bg_df %>%
  mutate(combined = if_else(row_number() <=11, paste0(Estimates, " (", `std. Error`, ") ", significance),
                            Estimates))



model_bg_df <- model_bg_df %>% 
  mutate(Predictors = dplyr::recode(Predictors,
                             '(Intercept)' = 'Intercept', 
                             'age' = 'Age',
                             `gender [non-male]` = 'Gender (non-male)', 
                             `edu [low]` = 'Education (low)', 
                             `edu [middle]` = 'Education (middle)', 
                             `race [white]` = 'Ethnicity (White)', 
                             `party [Other]` = 'Party (other)', 
                             `party [Republican]` = 'Party (Republican)',
                             'lagged recommended' = 'Prop. of news recommended (lagged)',
                             'lagged watched' = 'Prop. of news watched (lagged)',
                             'lagged added' = 'Prop. of news played by the extension (lagged)',
                             'τ00visaId' = 'τ00 Participant',
                             'N visaId' = 'N Participant'
                             )) %>%
  select(Predictors, combined) %>%
  rename('Prop. of news recommended' = 'combined')

# Convert to LaTeX table
model_bg_latex_table <- xtable(model_bg_df, caption="Multilevel Regression Predicting the Effects of Background Playing of News Videos on News Recommendation")

# Print the LaTeX code
print(model_bg_latex_table, type = "latex", caption.placement = "top", include.rownames=F,
      add.to.row = list(pos=list(18), command=c("\\hline \n \\multicolumn{2}{l}{Note: *** $<$ .001 ** $<$ .01 * $<$ .05. Lagged denotes data sourced from the previous day. The Prop. of news played by the extension (lagged) represents the ratio of the number of news videos played by the extension on the previous day to the total number of videos - both played by the extension and watched by the participant - on the previous day. }\n")))



