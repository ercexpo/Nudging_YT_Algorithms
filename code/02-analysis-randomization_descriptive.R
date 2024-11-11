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
library(chisq.posthoc.test)

options(scipen = 10)
options(digits = 10)

setwd("")
figdir <- ""


dat_survey <- read_csv("dat_survey.csv")
final_video_survey <- read_csv("final_video_survey.csv")
dat_uninstall <- read_csv("dat_uninstalls.csv")





## check if condition influences exclusion
# Prepare the data with exclusion counts and inclusion counts
exclusion_data <- dat_survey %>%
  count(intervention) %>%
  left_join(final_video_survey %>% count(intervention), by = "intervention", suffix = c("_eligible", "_included")) %>%
  mutate(n_exclusion = n_eligible - n_included,
         n_exclusion_rate = n_exclusion/n_eligible)

## check if the uninstallation is different across conditions
prop.test(x = exclusion_data$n_exclusion, n = exclusion_data$n_eligible, alternative = "two.sided")
pairwise.prop.test(
  x = exclusion_data$n_exclusion,
  n = exclusion_data$n_eligible,
  p.adjust.method = "fdr"
)


## add uninstallation time
dat_survey <- dat_uninstall %>%
  select(visaId, timestamp) %>%
  group_by(visaId) %>%
  slice_max(order_by = timestamp, n = 1) %>%
  ungroup() %>%
  rename(uninstall_timestamp = timestamp) %>%
  right_join(dat_survey, by = "visaId")

dat_survey <- dat_survey %>%
  mutate(
    # Convert installationTime from milliseconds to POSIXct
    installationTime = as.POSIXct(installationTime / 1000, origin = "1970-01-01", tz = "UTC"),
    # Calculate the difference in hours
    time_diff_hours = as.numeric(difftime(uninstall_timestamp, installationTime, units = "hours")),
    # Categorize into weeks based on the difference in hours
    week = case_when(
      time_diff_hours <= 7 * 24 ~ "1",
      time_diff_hours <= 7 * 24 * 2 ~ "2",
      time_diff_hours <= 7 * 24 * 3 ~ "3",
      time_diff_hours <= 7 * 24 * 4 ~ "4",
      TRUE ~ "5"
    )
  ) 

dat_survey_exclusion_step <- dat_survey %>%
  left_join(final_video_survey %>%
              select(visaId) %>%
              mutate(included = 1), 
            by = "visaId") %>%
  mutate(included = if_else(is.na(included), 0, included),
         early_uninstall = if_else(week == "5", 0, 1),
         inactive = if_else(week == "5" & included == 0, 1, 0))


summary(as.factor(dat_survey_exclusion_step$week))

dat_survey_exclusion_step_summary <- dat_survey_exclusion_step %>%
  group_by(intervention, week, included) %>%
  count()

## convert to wide format
dat_survey_exclusion_step_summary_wide <- dat_survey_exclusion_step_summary %>% 
  pivot_wider(
    names_from = c(week, included), 
    values_from = n, 
    names_sep = "_"
  )


dat_survey_exclusion_step_summary_wide <- dat_survey_exclusion_step_summary_wide %>%
  mutate(total_n = sum(`1_0`, `2_0`, `2_1`, `3_0`, `3_1`, `4_0`, `4_1`, `5_0`, `5_1`),
         uninstall_w1234 = sum(`1_0`, `2_0`, `2_1`, `3_0`, `3_1`, `4_0`, `4_1`),
         uninstall_w234 = sum(`2_0`, `2_1`, `3_0`, `3_1`, `4_0`, `4_1`),
         uninstall_included_w234_n = (`2_1` + `3_1` + `4_1`)
         )

## test if uninstallation is diff in w1 across conditions
prop.test(x = dat_survey_exclusion_step_summary_wide$`1_0`, n = dat_survey_exclusion_step_summary_wide$total_n, alternative = "two.sided")

## test if uninstallation is diff in w1-4 across conditions
prop.test(x = dat_survey_exclusion_step_summary_wide$uninstall_w1234, n = dat_survey_exclusion_step_summary_wide$total_n, alternative = "two.sided")

## test if uninstallation is diff in w2-4 across conditions
prop.test(x = dat_survey_exclusion_step_summary_wide$uninstall_w234, n = dat_survey_exclusion_step_summary_wide$total_n, alternative = "two.sided")

## test if uninstallation & inclusion is diff in w2-4 across conditions
prop.test(x = dat_survey_exclusion_step_summary_wide$uninstall_included_w234_n, n = dat_survey_exclusion_step_summary_wide$total_n, alternative = "two.sided")

## test if inactivity for those who kept the extension installed in w1-4 is diff across conditions
prop.test(x = dat_survey_exclusion_step_summary_wide$`5_0`, n = dat_survey_exclusion_step_summary_wide$total_n, alternative = "two.sided")
pairwise.prop.test(
  x = dat_survey_exclusion_step_summary_wide$`5_0`,
  n = dat_survey_exclusion_step_summary_wide$total_n,
  p.adjust.method = "fdr" 
)


## descriptives of demographics
proportions <- function(data, vars) {
  prop_perc <- list()
  for (var in vars) {
    prop <- round(prop.table(table(data[[var]])) * 100, 2)
    prop_perc[[var]] <- prop
  }
  return(prop_perc)
}

proportions(final_video_survey, c("gender", "race", "edu"))

## re-categorize age
final_video_survey %>%
  select(age) %>%
  mutate(age_category = cut(age, breaks = c(17, 29, 49, 64, Inf), labels = c("18-29", "30-49", "50-64", "65+"))) %>%
  count(age_category) %>%
  mutate(percentage = n / sum(n) * 100)

final_video_survey %>%
  select(age) %>% 
  summarize_all(list(mean = mean, sd = sd))

### repeat for the entire sample
proportions(dat_survey, c("gender", "race", "edu"))

# repeat for the entire sample
dat_survey %>%
  select(age) %>%
  mutate(age_category = cut(age, breaks = c(17, 29, 49, 64, Inf), labels = c("18-29", "30-49", "50-64", "65+"))) %>%
  count(age_category) %>%
  mutate(percentage = n / sum(n) * 100)


## check if the socio-demographics were equally distributed across groups 

# Function to perform chi-square test for categorical variables and ANOVA for numerical variables
test_distribution <- function(df, var_name){
  if(is.numeric(df[[var_name]])){
    # Perform ANOVA for numeric variables
    anova_model <- aov(df[[var_name]] ~ intervention, data = df)
    print(paste("ANOVA result for", var_name, ":"))
    print(summary(anova_model))
  } else {
    # Perform chi-square test for categorical variables
    chisq_result <- chisq.test(table(df[[var_name]], df$intervention))
    print(paste("Chi-square result for", var_name, ":"))
    print(chisq_result)
  }
}

# Variables to test
variables <- c("age", "gender", "edu", "race", "party")

# Apply the function to each variable
for(var in variables){
  test_distribution(final_video_survey, var)
}

## check if the demographics of people who were excluded were evenly distributed across conditions
excluded_participants <- anti_join(dat_survey, final_video_survey, by = "visaId")

for(var in variables){
  test_distribution(excluded_participants, var)
}

## check if the demographics of people who uninstalled in w1-4 were evenly distributed across conditions
for(var in variables){
  test_distribution(dat_survey %>% filter(week != "5"), var)
}

## check if the demographics of people who kept it installed in w1-4 and excluded were evenly distributed across conditions
for(var in variables){
  test_distribution(dat_survey_exclusion_step %>% filter(week == "5" & included == "0"), var)
}



# Function to perform chi-square test for categorical variables and ANOVA for numerical variables
# the IV is whether they uninstalled early OR whether they were active rather than conditions OR whether they were included in analysis
test_distribution_2 <- function(df, var_name, iv_name){
  if(is.numeric(df[[var_name]])){
    # Perform ANOVA for numeric variables
    formula <- as.formula(paste(var_name, "~", iv_name))
    anova_model <- aov(formula, data = df)
    print(paste("ANOVA result for", var_name, ":"))
    print(summary(anova_model))
  } else {
    # Perform chi-square test for categorical variables
    chisq_result <- chisq.test(table(df[[var_name]], df[[iv_name]]))
    print(paste("Chi-square result for", var_name, ":"))
    print(chisq_result)
  }
}

for(var in variables){
  test_distribution_2(dat_survey_exclusion_step, var, "included")
}
summary(lm(age ~ as.factor(included), data = dat_survey_exclusion_step))


for(var in variables){
  test_distribution_2(dat_survey_exclusion_step, var, "early_uninstall")
}

summary(lm(age ~ as.factor(early_uninstall), data = dat_survey_exclusion_step))
chisq.posthoc.test(table(dat_survey_exclusion_step[ ,c("edu", "early_uninstall")]), method = "fdr")

for(var in variables){
  test_distribution_2(dat_survey_exclusion_step, var, "inactive")
}


###### the mean, sd, and alpha of DVs ####

vars_desc <- final_video_survey %>%
  select(pol_part_pre, pol_part_post, perceived_acc_true, perceived_acc_false, perceived_pol_politician_pre, 
         perceived_pol_politician_post, perceived_pol_supporter_pre, perceived_pol_supporter_post, 
         perceived_pol_scale_pre, perceived_pol_scale_post, ap_leader_pre, ap_leader_post, ap_politician_pre, 
         ap_politician_post, partisan_ends_pre, partisan_ends_post) %>% 
  summarise(across(everything(), list(mean = \(x) round(mean(x, na.rm = TRUE), 2), 
                                      sd = \(x) round(sd(x, na.rm = TRUE), 2)), .names = "{.col}_{.fn}"))




# Define scales (list of variables in each scale)
perceived_pol_scale_pre <- c("PERCEIVEDPOL2_1", "PERCEIVEDPOL2_2", "PERCEIVEDPOL2_3", "PERCEIVEDPOL2_4")
perceived_pol_scale_post <- c("PERCEIVEDPOL2_1_W2", "PERCEIVEDPOL2_2_W2", "PERCEIVEDPOL2_3_W2", "PERCEIVEDPOL2_4_W2")
partisan_ends_pre <- c("PARTISANENDS_1_new", "PARTISANENDS_2_new", "PARTISANENDS_3_new", "PARTISANENDS_4_new", "PARTISANENDS_5_new")
partisan_ends_post <- c("PARTISANENDS_1_new_W2", "PARTISANENDS_2_new_W2", "PARTISANENDS_3_new_W2", "PARTISANENDS_4_new_W2", "PARTISANENDS_5_new_W2")


# Put scales in a list
scales <- list(perceived_pol_scale_pre, perceived_pol_scale_post, partisan_ends_pre, partisan_ends_post)

# Calculate alphas for each scale and store in a list
alphas <- lapply(scales, function(scale) psych::alpha(final_video_survey[, scale], na.rm = TRUE))

# Name list elements according to scale names
names(alphas) <- c('perceived_pol_scale_pre', 'perceived_pol_scale_post', 'partisan_ends_pre', 'partisan_ends_post')

# Print the results
print(alphas)






## the distribution of self-reported dvs

survey_dvs <- final_video_survey %>%
  select(
         perceived_acc_false, perceived_acc_true, perceived_acc_diff, 
         pol_part_post,
         perceived_pol_politician_post, perceived_pol_supporter_post, perceived_pol_scale_post,
         ap_leader_post, ap_politician_post, 
         partisan_ends_post)

survey_dvs_long <- tidyr::gather(survey_dvs, key = "variable", value = "value") %>%
  mutate(variable = dplyr::recode_factor(variable,
    'pol_part_post' = 'Intended\npolitical participation',
    'perceived_acc_true' = 'Perceived accuracy\nof true claims', 
    'perceived_acc_false' = 'Perceived accuracy\nof false claims', 
    'perceived_acc_diff' = 'Perceived accuracy\nof claims (difference)', 
    'perceived_pol_politician_post' = 'Perceived polarization\n(politicians)',
    'perceived_pol_supporter_post' = 'Perceived polarization\n(party supporters)', 
    'perceived_pol_scale_post' = 'Perceived polarization\n(4-item scale)',
    'ap_leader_post' = 'Affective polarization\n(presidents)', 
    'ap_politician_post' = 'Affective polarization\n(politicians)', 
    'partisan_ends_post' = 'Prioritizing partisan ends\nover democratic means'
  ))

ggplot(survey_dvs_long, aes(x = value)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~ variable, nrow = 3) +
  theme(
        axis.title = element_blank(), 
        )

ggsave(paste0(figdir, "fig_dv_distribution.png"),
       device = "png", width = 10, height = 8)




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

multiple_lm_to_latex(models_table_1, "OLS Regressions Predicting the Effects of Treatments on YouTube Engagement")
