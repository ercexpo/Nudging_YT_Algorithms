library(dplyr)
library(arrow)
library(readr)
library(tibble)
library(jsonlite)
library(stringr)
library(tidyr)
library(psych)
library(AER)
library(xtable)
library(ggplot2)
library(scales)
library(ggpubr)
library(psych)

options(scipen = 10)
options(digits = 10)

setwd("")

#final_video_survey <- read_csv("final_video_survey_additional_1.csv")
final_video_survey <- read_csv("final_video_survey_additional_5.csv")


# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 Effects on engagement                                                                           ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================
## import the time dataset
dat_watch_time <- read_csv("dat_watch_time_all.csv")

final_video_survey <- final_video_survey %>%
  left_join(dat_watch_time, by = "visaId") %>%
  mutate(average_time_w1 = time_sum_w1/active_day_w1,
         average_time_w23 = time_sum_w23/active_day_w23,
         average_time_w4 = time_sum_w4/active_day_w4
  )



model_to_table <-  function(model){
  x <-  summary(model)$coefficients %>% 
    data.frame() %>%
    mutate(variable = row.names(.)) %>%
    setNames(c("est", "se", "t", "p", "coeff")) %>% 
    select(., c("coeff", "est", "se", "p")) %>% 
    mutate(lb = est - (1.96*se)) %>%
    mutate(ub = est + (1.96*se))
  
  return(x)
}


dvs <- c('average_organic_video_w23', 'active_day_w23', 'average_time_w23')

models = list()

for (i in 1:3){
  if (i == 1){
    eq_full <- lm(average_organic_video_w23 ~ age + gender + edu + race + party + average_organic_video_w1 + relevel(factor(intervention), ref = 'control'), data = final_video_survey)
  } else if (i == 2) {
    eq_full <- lm(active_day_w23/2 ~ age + gender + edu + race + party + active_day_w1 + relevel(factor(intervention), ref = 'control'), data = final_video_survey)
  } else {
    eq_full <- lm(log1p(average_time_w23) ~ age + gender + edu + race + party + log1p(average_time_w1) + relevel(factor(intervention), ref = 'control'), data = final_video_survey)
  }
  
  models[[length(models) + 1]] <- eq_full %>%
    model_to_table() %>%
    mutate(var = dvs[i]) %>%
    mutate(n = nobs(eq_full),
           R2 = summary(eq_full)$r.squared)
}

models <- do.call(rbind, models)

#### create regression tables

####prepare the dataframes
models_table_1 <- models %>%
  mutate(
    var = dplyr::recode(var,
                        "average_organic_video_w23" = "Videos watched per active day (W2-3)",
                        "active_day_w23" = "Number of active days per week (W2-3)",
                        "average_time_w23" = "Time spent per active day (W2-3)"
    )) %>%
  mutate(
    coeff = dplyr::recode(coeff,
                          '(Intercept)' = 'Intercept', 
                          'age' = 'Age',
                          'gendernon-male' = 'Gender (non-male)', 
                          'edulow' = 'Education (low)', 
                          'edumiddle' = 'Education (middle)', 
                          'racewhite' = 'Ethnicity (White)', 
                          'partyOther' = 'Party (other)', 
                          'partyRepublican' = 'Party (Republican)',
                          'average_organic_video_w1' = 'Videos watched per active day (W1)',
                          'active_day_w1' = 'Number of active days (W1)',
                          'log1p(average_time_w1)' = 'Time spent per active day (W1)',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)')
  )


# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex <- function(model_df, caption) {
  
  # Convert p-values to asterisks
  model_df$significance <- ifelse(model_df$p < .001, "***", 
                                  ifelse(model_df$p < .01, "**", 
                                         ifelse(model_df$p < .05, "*", "")))
  
  # Add column combining estimates, standard errors and significance
  model_df$combined <- paste0(finalfit::round_tidy(model_df$est, 2), " (", finalfit::round_tidy(model_df$se, 2), ") ", model_df$significance)
  
  # Spread to wide format so that models are in columns
  wide_df <- model_df %>% 
    select(coeff, var, combined) %>% 
    spread(key = var, value = combined) %>%
    mutate(order = c(2, 12, 13, 4, 5, 6, 3, 1, 10, 7, 8, 11, 9)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `Videos watched per active day (W2-3)` = as.character(model_df[model_df$var == 'Videos watched per active day (W2-3)', 'n'][1]),
            `Number of active days per week (W2-3)` = as.character(model_df[model_df$var == 'Number of active days per week (W2-3)', 'n'][1]),
            `Time spent per active day (W2-3)` = as.character(model_df[model_df$var == 'Time spent per active day (W2-3)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Videos watched per active day (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Videos watched per active day (W2-3)', 'R2'][1], 2)),
            `Number of active days per week (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Number of active days per week (W2-3)', 'R2'][1], 2)),
            `Time spent per active day (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Time spent per active day (W2-3)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-"))) %>%
    select(coeff, `Videos watched per active day (W2-3)`, `Number of active days per week (W2-3)`, `Time spent per active day (W2-3)`)
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}

#multiple_lm_to_latex(models_table_1, "OLS Regressions Predicting the Effects of Treatments on YouTube Engagement (Participants Watched at Least One Video During Weeks 2-3)")
multiple_lm_to_latex(models_table_1, "OLS Regressions Predicting the Effects of Treatments on YouTube Engagement (Participants watched at least five YouTube videos over weeks 1-3 and at least one video during weeks 2-3)")

