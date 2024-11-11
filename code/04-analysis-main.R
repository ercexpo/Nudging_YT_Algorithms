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
library(xtable)

options(scipen = 10)
options(digits = 10)

setwd("")
figdir <- ""

final_video_survey <- read_csv("final_video_survey.csv")
# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 Main analysis                                                                                   ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================


##
dvs <- c("average_organic_news_w23", "average_organic_news_w4", "per_organic_news_w23", "per_organic_news_w4",
         "average_organic_pol_non_news_w23", "average_organic_pol_non_news_w4", "per_organic_pol_non_news_w23", "per_organic_pol_non_news_w4",
         "average_organic_problem_w23", "average_organic_problem_w4", "per_organic_problem_w23", "per_organic_problem_w4")


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


# MAIN =========================================================================

## Run regressions ####

models = list()

for (dv in dvs[1:12]){
  
  print(dv)
    if (grepl("average_organic_news", dv) == T){
      baseline <- "average_organic_news_w1"
    } else if (grepl("per_organic_news", dv) == T) {
      baseline <- "per_organic_news_w1"
    } else if (grepl("average_organic_pol_non_news", dv) == T) {
      baseline <- "average_organic_pol_non_news_w1"
    } else if (grepl("per_organic_pol_non_news", dv) == T) {
      baseline <- "per_organic_pol_non_news_w1"
    } else if (grepl("average_organic_problem", dv) == T) {
      baseline <- "average_organic_problem_w1"
    } else if (grepl("per_organic_problem", dv) == T) {
      baseline <- "per_organic_problem_w1"
    }
    
    for (j in 1:2){
      
      eq = c(
        paste0(dv, " ~ age + gender + edu + race + party +", baseline, "+ relevel(factor(intervention), ref = 'control')"), 
        paste0(dv, " ~ age + gender + edu + race + party +", baseline, "+ relevel(factor(intervention), ref = 'background')")
        
      )[j] 
      
      print(eq)
      
      modelname = c("Behavioral baseline", 
                    "Behavioral baseline_bg_ref"
                    )[j]
      
      eq_full <- lm(as.formula(eq), data = final_video_survey)
      
      models[[length(models) + 1]] <- eq_full %>%
        model_to_table() %>%
        mutate(model = paste0("Model ", j)) %>% 
        mutate(modelname = modelname) %>% 
        mutate(var = dv) %>%
        mutate(n = nobs(eq_full),
               R2 = summary(eq_full)$r.squared)
      
    }
}

models <- do.call(rbind, models)


# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 Figure 1                                                                                        ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================


## Figure

fig1_dat <- models %>%
  filter(
    (coeff == "relevel(factor(intervention), ref = \"control\")background" | coeff == "relevel(factor(intervention), ref = \"control\")banner") &
      !grepl("problem", .$var) & 
      modelname == "Behavioral baseline"
    ) %>%
  mutate(
    coeff = dplyr::recode(coeff,
                          "relevel(factor(intervention), ref = \"control\")background" = "Algorithmic Nudge",
                          "relevel(factor(intervention), ref = \"control\")banner" = "User Nudge")) %>%
  mutate(
  var = dplyr::recode(var,
                      "average_organic_news_w23" = "News watched\nper active day (W2-3)",
                      "average_organic_news_w4" = "News watched\nper active day (W4)",
                      "average_organic_pol_non_news_w23" = "Pol. non-news watched\nper active day (W2-3)",
                      "average_organic_pol_non_news_w4" = "Pol. non-news watched\nper active day (W4)",
                      "per_organic_news_w23" = "Prop. of news\nwatched (W2-3)",
                      "per_organic_news_w4" = "Prop. of news\nwatched (W4)",
                      "per_organic_pol_non_news_w23" = "Prop. of pol non-news\nwatched (W2-3)",
                      "per_organic_pol_non_news_w4" = "Prop. of pol non-news\nwatched (W4)"
                      )) %>%
  mutate(varno = case_when(
    var == "News watched\nper active day (W2-3)" ~ 2.5,
    var == "News watched\nper active day (W4)" ~ 2,
    var == "Pol. non-news watched\nper active day (W2-3)" ~ 1.5,
    var == "Pol. non-news watched\nper active day (W4)" ~ 1,
    var == "Prop. of news\nwatched (W2-3)" ~ 2.5,
    var == "Prop. of news\nwatched (W4)" ~ 2,
    var == "Prop. of pol non-news\nwatched (W2-3)" ~ 1.5,
    var == "Prop. of pol non-news\nwatched (W4)" ~ 1
  )) %>%
  mutate(varno_r = case_when(coeff == "Algorithmic Nudge" ~ varno + 0.1,
                                coeff == "User Nudge" ~ varno - 0.1)) 

fig1a_dat <- fig1_dat %>%
  filter(!grepl("Prop.", .$var))

s = 2
s2 = 0.3

fig1a <- ggplot() + 
  geom_vline(xintercept = 0, linewidth = 0.3, 
             linetype = "solid", colour = "grey60") + 
  geom_segment(data = fig1a_dat, 
               aes(y = varno_r, yend = varno_r, 
                   xend = ub, x = lb, colour = coeff), 
               linewidth = s, lineend = "round") + 
  geom_point(data = fig1a_dat, 
             aes(y = varno_r, x = est, 
                 fill = coeff),
             color = "white", size = s2) + 
  scale_y_continuous(
    limits = c(0.8,2.7),
    breaks = c(1, 1.5, 2, 2.5),
    labels = c(
      "Pol. videos watched\nper active day (W4)",
      "Pol. videos watched\nper active day (W2-3)",
      "News watched\nper active day (W4)",
      "News watched\nper active day (W2-3)")
  ) +
  scale_x_continuous(limits = c(-1.3,1.3)) +
  scale_fill_manual(values = c("goldenrod1", "grey60")) + 
  scale_colour_manual(values = c("goldenrod1", "grey60")) +
  theme_minimal() + 
  
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"), 
        axis.title = element_blank(), 
        #axis.text.y = element_blank(), #remove x axis labels
        axis.ticks.y = element_blank(), #remove x axis ticks
        strip.text = element_text(size = 10))


fig1b_dat <- fig1_dat %>%
  filter(grepl("Prop.", .$var))

fig1b <- ggplot() + 
  geom_vline(xintercept = 0, linewidth = 0.3, 
             linetype = "solid", colour = "grey60") + 
  geom_segment(data = fig1b_dat, 
               aes(y = varno_r, yend = varno_r, 
                   xend = ub, x = lb, colour = coeff), 
               linewidth = s, lineend = "round") + 
  geom_point(data = fig1b_dat, 
             aes(y = varno_r, x = est, 
                 fill = coeff),
             color = "white", size = s2) + 
  scale_y_continuous(
    limits = c(0.8,2.7),
    breaks = c(1, 1.5, 2, 2.5),
    labels = c(
      "Prop. of pol. videos\nwatched (W4)",
      "Prop. of pol. videos\nwatched (W2-3)",
      "Prop. of news\nwatched (W4)",
      "Prop. of news\nwatched (W2-3)")
    ) +
  scale_x_continuous(
    limits = c(-20,20),
                     breaks = c(-20, -10, 0, 10, 20),
                     labels = c(
                       "-20%",
                       "-10%",
                       "0%",
                       "10%",
                       "20%")
                     ) +
  scale_fill_manual(values = c("goldenrod1", "grey60")) + 
  scale_colour_manual(values = c("goldenrod1", "grey60")) +
  theme_minimal() + 
  
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"), 
        axis.title = element_blank(), 
        axis.ticks.y = element_blank(), #remove x axis ticks
        strip.text = element_text(size = 10))


## combine these 2 figures
ggarrange(fig1a, fig1b, common.legend = TRUE, legend = "bottom",
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

ggsave(paste0(figdir, "fig1.png"),
       device = "png", width = 4, height = 5)

# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 Table 1                                                                                         ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================

#### create regression tables

####prepare the dataframes
models_table_1 <- models %>%
  filter(!grepl("problem", .$var) & modelname == "Behavioral baseline") %>%
  mutate(
    var = dplyr::recode(var,
                        "average_organic_news_w23" = "News watched per active day (W2-3)",
                        "average_organic_news_w4" = "News watched per active day (W4)",
                        "average_organic_pol_non_news_w23" = "Pol. videos watched per active day (W2-3)",
                        "average_organic_pol_non_news_w4" = "Pol. videos watched per active day (W4)",
                        "per_organic_news_w23" = "Prop. of news watched (W2-3)",
                        "per_organic_news_w4" = "Prop. of news watched (W4)",
                        "per_organic_pol_non_news_w23" = "Prop. of pol. videos watched (W2-3)",
                        "per_organic_pol_non_news_w4" = "Prop. of pol. videos watched (W4)"
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
                          'average_organic_news_w1' = 'News watched per active day (W1)',
                          'average_organic_pol_non_news_w1' = 'Pol. videos watched per active day (W1)',
                          'per_organic_news_w1' = 'Prop. of news watched (W1)',
                          'per_organic_pol_non_news_w1' = 'Prop. of pol. videos watched (W1)',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)')
  )



#### create table 1a ########################################################################################################
models_table_1a <- models_table_1 %>%
  filter(!grepl("Prop.", .$var))

# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex1a <- function(model_df, caption) {
  
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
    mutate(order = c(2, 11, 12, 4, 5, 6, 3, 1, 9, 7, 8, 10)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `News watched per active day (W2-3)` = as.character(model_df[model_df$var == 'News watched per active day (W2-3)', 'n'][1]),
            `News watched per active day (W4)` = as.character(model_df[model_df$var == 'News watched per active day (W4)', 'n'][1]),
            `Pol. videos watched per active day (W2-3)` = as.character(model_df[model_df$var == 'Pol. videos watched per active day (W2-3)', 'n'][1]),
            `Pol. videos watched per active day (W4)` = as.character(model_df[model_df$var == 'Pol. videos watched per active day (W4)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `News watched per active day (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'News watched per active day (W2-3)', 'R2'][1], 2)),
            `News watched per active day (W4)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'News watched per active day (W4)', 'R2'][1], 2)),
            `Pol. videos watched per active day (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Pol. videos watched per active day (W2-3)', 'R2'][1], 2)),
            `Pol. videos watched per active day (W4)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Pol. videos watched per active day (W4)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}

multiple_lm_to_latex1a(models_table_1a, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Number Per Active Day)")






#### create table 1b ########################################################################################################
models_table_1b <- models_table_1 %>%
  filter(grepl("Prop.", .$var))


# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex1b <- function(model_df, caption) {
  
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
    mutate(order = c(2, 11, 12, 4, 5, 6, 3, 1, 7, 8, 9, 10)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `Prop. of news watched (W2-3)` = as.character(model_df[model_df$var == 'Prop. of news watched (W2-3)', 'n'][1]),
            `Prop. of news watched (W4)` = as.character(model_df[model_df$var == 'Prop. of news watched (W4)', 'n'][1]),
            `Prop. of pol. videos watched (W2-3)` = as.character(model_df[model_df$var == 'Prop. of pol. videos watched (W2-3)', 'n'][1]),
            `Prop. of pol. videos watched (W4)` = as.character(model_df[model_df$var == 'Prop. of pol. videos watched (W4)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Prop. of news watched (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prop. of news watched (W2-3)', 'R2'][1], 2)),
            `Prop. of news watched (W4)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prop. of news watched (W4)', 'R2'][1], 2)),
            `Prop. of pol. videos watched (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prop. of pol. videos watched (W2-3)', 'R2'][1], 2)),
            `Prop. of pol. videos watched (W4)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prop. of pol. videos watched (W4)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}



multiple_lm_to_latex1b(models_table_1b, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Percentage)")



#### create table 1c - problematic news channels ########################################################################################################

models_table_1_problem <- models %>%
  filter(grepl("problem", .$var) & modelname == "Behavioral baseline") %>%
  mutate(
    var = dplyr::recode(var,
                        "average_organic_problem_w23" = "Problematic videos watched per active day (W2-3)",
                        "average_organic_problem_w4" = "Problematic videos watched per active day (W4)",
                        "per_organic_problem_w23" = "Prop. of problematic videos watched (W2-3)",
                        "per_organic_problem_w4" = "Prop. of problematic videos watched (W4)"
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
                          'average_organic_problem_w1' = 'Problematic videos watched per active day (W1)',
                          'per_organic_problem_w1' = 'Prop. of problematic videos watched (W1)',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)')
  )


# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex1problem <- function(model_df, caption) {
  
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
    mutate(order = c(2, 11, 12, 4, 5, 6, 3, 1, 7, 8, 9, 10)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `Problematic videos watched per active day (W2-3)` = as.character(model_df[model_df$var == 'Problematic videos watched per active day (W2-3)', 'n'][1]),
            `Problematic videos watched per active day (W4)` = as.character(model_df[model_df$var == 'Problematic videos watched per active day (W4)', 'n'][1]),
            `Prop. of problematic videos watched (W2-3)` = as.character(model_df[model_df$var == 'Prop. of problematic videos watched (W2-3)', 'n'][1]),
            `Prop. of problematic videos watched (W4)` = as.character(model_df[model_df$var == 'Prop. of problematic videos watched (W4)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Problematic videos watched per active day (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Problematic videos watched per active day (W2-3)', 'R2'][1], 2)),
            `Problematic videos watched per active day (W4)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Problematic videos watched per active day (W4)', 'R2'][1], 2)),
            `Prop. of problematic videos watched (W2-3)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prop. of problematic videos watched (W2-3)', 'R2'][1], 2)),
            `Prop. of problematic videos watched (W4)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prop. of problematic videos watched (W4)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}

multiple_lm_to_latex1problem(models_table_1_problem, "OLS Regressions Predicting the Effects of Treatments on Consumption of Problematic Videos")




# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                        Table 2 - background condition as the ref                                                                ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================

models_table_2 <- models %>%
  filter(!grepl("problem", .$var) & modelname == "Behavioral baseline_bg_ref") %>%
  mutate(
    var = dplyr::recode(var,
                        "average_organic_news_w23" = "News watched per active day (W2-3)",
                        "average_organic_news_w4" = "News watched per active day (W4)",
                        "average_organic_pol_non_news_w23" = "Pol. videos watched per active day (W2-3)",
                        "average_organic_pol_non_news_w4" = "Pol. videos watched per active day (W4)",
                        "per_organic_news_w23" = "Prop. of news watched (W2-3)",
                        "per_organic_news_w4" = "Prop. of news watched (W4)",
                        "per_organic_pol_non_news_w23" = "Prop. of pol. videos watched (W2-3)",
                        "per_organic_pol_non_news_w4" = "Prop. of pol. videos watched (W4)"
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
                          'average_organic_news_w1' = 'News watched per active day (W1)',
                          'average_organic_pol_non_news_w1' = 'Pol. videos watched per active day (W1)',
                          'per_organic_news_w1' = 'Prop. of news watched (W1)',
                          'per_organic_pol_non_news_w1' = 'Prop. of pol. videos watched (W1)',
                          `relevel(factor(intervention), ref = "background")control` = 'Condition (control)',
                          `relevel(factor(intervention), ref = "background")banner` = 'Condition (user)')
  )




#### create table 2a ########################################################################################################
models_table_2a <- models_table_2 %>%
  filter(!grepl("Prop.", .$var))

multiple_lm_to_latex1a(models_table_2a, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Number Per Active Day, Algorithmic Nudge Condition As the Reference)")

#### create table 2b ########################################################################################################
models_table_2b <- models_table_2 %>%
  filter(grepl("Prop.", .$var))

multiple_lm_to_latex1b(models_table_2b, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Percentage, Algorithmic Nudge Condition As the Reference)")




##### 2SLS
final_video_survey <- final_video_survey %>%
  mutate(gender_recode = if_else(gender == "non-male", "non_male", gender))
# H1M =========================================================================
md1m <- ivreg(per_organic_news_w23 ~ age + gender_recode + edu + race + party + relevel(factor(intervention), ref = 'control') + per_organic_news_w1 + per_rec_news_w23 | . - per_rec_news_w23 + per_rec_news_w1, data = final_video_survey)

# H1N =========================================================================
md1n <- ivreg(per_rec_news_w23 ~ age + gender_recode + edu + race + party + relevel(factor(intervention), ref = 'control') + per_rec_news_w1 + per_organic_news_w23 | . - per_organic_news_w23 + per_organic_news_w1, data = final_video_survey)


################## MAKE TABLE
md1m_df <- sjPlot::tab_model(md1m, show.ci = F, show.se = T) %>%
  sjtable2df::mtab2df(n_models = 1)
md1n_df <- sjPlot::tab_model(md1n, show.ci = F, show.se = T) %>%
  sjtable2df::mtab2df(n_models = 1)
md_1mn_df <- bind_rows(md1m_df, md1n_df)


# Convert p-values to asterisks
md_1mn_df$significance <- ifelse(md_1mn_df[, "p"] < .001, "***", 
                                    ifelse(md_1mn_df[, "p"] < .01, "**", 
                                           ifelse(md_1mn_df[, "p"] < .05, "*", "")))

md_1mn_df <- md_1mn_df %>%
  mutate(combined = if_else(row_number() <=12 | (row_number() >=15 & row_number() <= 26), paste0(Estimates, " (", `std. Error`, ") ", significance),
                            Estimates))

md_1mn_df <- md_1mn_df %>% 
  mutate(Predictors = dplyr::recode(Predictors,
                                    '(Intercept)' = 'Intercept', 
                                    'age' = 'Age',
                                    'gender_recodenon_male' = 'Gender (non-male)', 
                                    'edulow' = 'Education (low)', 
                                    'edumiddle' = 'Education (middle)', 
                                    'racewhite' = 'Ethnicity (White)', 
                                    'partyOther' = 'Party (other)', 
                                    'partyRepublican' = 'Party (Republican)',
                                    `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                                    `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)',
                                    'per_organic_news_w1' = 'Prop. of news watched (W1)',
                                    'per_rec_news_w23' = 'Prop. of news recommended (W2-3)',
                                    'per_rec_news_w1' = 'Prop. of news recommended (W1)',
                                    'per_organic_news_w23' = 'Prop. of news watched (W2-3)',
                                    'Observations' = 'N'
  )) %>%
  select(Predictors, combined)

md_1mn_df_wide <- full_join(md_1mn_df[1:14,], md_1mn_df[15:28, ], by = "Predictors") %>%
  mutate(order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15 ,16, 13, 14)) %>%
  arrange(order) %>%
  select(-order) %>%
  rename('Prop. of news watched (W2-3)' = `combined.x`,
         'Prop. of news recommended (W2-3)' = `combined.y`)

# Convert to LaTeX table
md_1mn_latex_table <- xtable(md_1mn_df_wide, caption="2SLS Regressions Testing the Relationship Between News Watching and News Recommendation")

# Print the LaTeX code
print(md_1mn_latex_table, type = "latex", caption.placement = "top", include.rownames=F,
      add.to.row = list(pos=list(16), command=c("\\hline \n \\multicolumn{2}{l}{Note: *** $<$ .001 ** $<$ .01 * $<$ .05.}\n")))


# H3-5A =========================================================================
dvs02 <- c("perceived_acc_false", "perceived_acc_true", "perceived_acc_diff", 
           "pol_part_post",
           "perceived_pol_politician_post", "perceived_pol_supporter_post", "perceived_pol_scale_post")

models_h345 = list()

for (dv in dvs02[1:7]){
  
  if (grepl("acc", dv) == T){
    dv <- dv
    iv <- ""
  } else {
    dv <- dv
    iv <- gsub("post", "pre", dv)
  }
  print(dv)
  print(iv)
  
  for (j in 1:2){
    
    eq = c(
      paste0(dv, " ~ age + gender + edu + race + party + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_w23"), 
      paste0(dv, " ~ age + gender + edu + race + party + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_diff")
    )[j] 
    
    print(eq)
    
    modelname = c("Overall news consumption", 
                  "Difference in news consumption"
    )[j]
    
    eq_full <- lm(as.formula(eq), data = final_video_survey)
    
    models_h345[[length(models_h345) + 1]] <- eq_full %>%
      model_to_table() %>%
      mutate(model = paste0("Model ", j)) %>% 
      mutate(modelname = modelname) %>% 
      mutate(var = dv) %>%
      mutate(n = nobs(eq_full),
             R2 = summary(eq_full)$r.squared)
    
  }
}

models_h345 <- do.call(rbind, models_h345)

# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 h345 Table                                                                                         ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================

####prepare the dataframes
models_table_345 <- models_h345 %>%
  mutate(var = dplyr::recode_factor(var,
                                         'pol_part_post' = 'Intended\npolitical participation',
                                         'perceived_acc_true' = 'Perceived accuracy\nof true claims', 
                                         'perceived_acc_false' = 'Perceived accuracy\nof false claims', 
                                         'perceived_acc_diff' = 'Perceived accuracy\nof claims (difference)', 
                                         'perceived_pol_politician_post' = 'Perceived polarization\n(politicians)',
                                         'perceived_pol_supporter_post' = 'Perceived polarization\n(party supporters)', 
                                         'perceived_pol_scale_post' = 'Perceived polarization\n(4-item scale)'
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
                          'organic_news_w23' = 'Total number of news videos watched (W2-3)',
                          'organic_news_diff' = 'Change in news consumption between W2-3 and W1',
                          'pol_part_pre' = 'Intended\npolitical participation (pre-test)',
                          'perceived_pol_politician_pre' = 'Perceived polarization\n(politicians, pre-test)',
                          'perceived_pol_supporter_pre' = 'Perceived polarization\n(party supporters, pre-test)', 
                          'perceived_pol_scale_pre' = 'Perceived polarization\n(4-item scale, pre-test)',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)')
  )


#### table 345a ####
models_table_345a <- models_table_345 %>%
  filter(modelname == "Overall news consumption")

# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex345a <- function(model_df, caption) {
  
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
    mutate(order = c(2, 13, 14, 4, 5, 6, 3, 9, 1, 7, 8, 12, 11, 10, 15)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `Intended\npolitical participation` = as.character(model_df[model_df$var == 'Intended\npolitical participation', 'n'][1]),
            `Perceived accuracy\nof true claims` = as.character(model_df[model_df$var == 'Perceived accuracy\nof true claims', 'n'][1]),
            `Perceived accuracy\nof false claims` = as.character(model_df[model_df$var == 'Perceived accuracy\nof false claims', 'n'][1]),
            `Perceived accuracy\nof claims (difference)` = as.character(model_df[model_df$var == 'Perceived accuracy\nof claims (difference)', 'n'][1]),
            `Perceived polarization\n(politicians)` = as.character(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'n'][1]),
            `Perceived polarization\n(party supporters)` = as.character(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'n'][1]),
            `Perceived polarization\n(4-item scale)` = as.character(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Intended\npolitical participation` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Intended\npolitical participation', 'R2'][1], 2)),
            `Perceived accuracy\nof true claims` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived accuracy\nof true claims', 'R2'][1], 2)),
            `Perceived accuracy\nof false claims` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived accuracy\nof false claims', 'R2'][1], 2)),
            `Perceived accuracy\nof claims (difference)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived accuracy\nof claims (difference)', 'R2'][1], 2)),
            `Perceived polarization\n(politicians)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'R2'][1], 2)),
            `Perceived polarization\n(party supporters)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'R2'][1], 2)),
            `Perceived polarization\n(4-item scale)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}


multiple_lm_to_latex345a(models_table_345a, "OLS Regressions Predicting the Effects of Overall News Exposure on Intended Political Participation, Perceived Accuracy of Political Claims, and Perceived Polarization")


#### table 345b ####
models_table_345b <- models_table_345 %>%
  filter(modelname == "Difference in news consumption" & !grepl("accuracy", .$var))

# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex345b <- function(model_df, caption) {
  
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
    mutate(order = c(2, 15, 13, 14, 4, 5, 6, 3, 9, 1, 7, 8, 12, 11, 10)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `Intended\npolitical participation` = as.character(model_df[model_df$var == 'Intended\npolitical participation', 'n'][1]),
            `Perceived polarization\n(politicians)` = as.character(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'n'][1]),
            `Perceived polarization\n(party supporters)` = as.character(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'n'][1]),
            `Perceived polarization\n(4-item scale)` = as.character(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Intended\npolitical participation` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Intended\npolitical participation', 'R2'][1], 2)),
            `Perceived polarization\n(politicians)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'R2'][1], 2)),
            `Perceived polarization\n(party supporters)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'R2'][1], 2)),
            `Perceived polarization\n(4-item scale)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}


multiple_lm_to_latex345b(models_table_345b, "OLS Regressions Predicting the Effects of Change in News Exposure on Intended Political Participation and Perceived Polarization")




# H6-7A =========================================================================

dvs03 <- c("ap_leader_post", "ap_politician_post", 
           "partisan_ends_post")

models_h67 = list()

for (dv in dvs03[1:3]){
  
  dv <- dv
  iv <- gsub("post", "pre", dv)
  
  print(dv)
  print(iv)
  
  for (j in 1:2){
    
    eq = c(
      paste0(dv, " ~ age + gender + edu + race + pid_imputed + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_w23 + pid_strength + organic_news_w23 * pid_strength"), 
      paste0(dv, " ~ age + gender + edu + race + pid_imputed + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_diff + pid_strength + organic_news_diff * pid_strength")
    )[j] 
    
    print(eq)
    
    modelname = c("Overall news consumption", 
                  "Difference in news consumption"
    )[j]
    
    eq_full <- lm(as.formula(eq), data = final_video_survey)
    
    models_h67[[length(models_h67) + 1]] <- eq_full %>%
      model_to_table() %>%
      mutate(model = paste0("Model ", j)) %>% 
      mutate(modelname = modelname) %>% 
      mutate(var = dv) %>%
      mutate(n = nobs(eq_full),
             R2 = summary(eq_full)$r.squared)
    
  }
}

models_h67 <- do.call(rbind, models_h67)

# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 h67 Table                                                                                       ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================

####prepare the dataframes
models_table_67 <- models_h67 %>%
  mutate(var = dplyr::recode_factor(var,
                                    'ap_leader_post' = 'Affective polarization\n(presidents)', 
                                    'ap_politician_post' = 'Affective polarization\n(politicians)', 
                                    'partisan_ends_post' = 'Prioritizing partisan ends\nover democratic means'
  )) %>%
  mutate(
    coeff = dplyr::recode(coeff,
                          '(Intercept)' = 'Intercept', 
                          'age' = 'Age',
                          'gendernon-male' = 'Gender (non-male)', 
                          'edulow' = 'Education (low)', 
                          'edumiddle' = 'Education (middle)', 
                          'racewhite' = 'Ethnicity (White)', 
                          'pid_imputedRepublican' = 'Partisanship (Republican)',
                          'ap_leader_pre' = 'Affective polarization\n(presidents, pre-test)',
                          'ap_politician_pre' = 'Affective polarization\n(politicians, pre-test)', 
                          'partisan_ends_pre' = 'Prioritizing partisan ends\nover democratic means(pre-test)',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)',
                          'organic_news_w23' = 'Total number of news videos watched (W2-3)',
                          'organic_news_diff' = 'Change in news consumption between W2-3 and W1',
                          'pid_strength' = 'Partisan identity strength',
                          'organic_news_w23:pid_strength' = 'Total number of news videos watched (W2-3) × Partisan identity strength',
                          'organic_news_diff:pid_strength' = 'Change in news consumption between W2-3 and W1 × Partisan identity strength'
  ))



# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex67 <- function(model_df, caption) {
  
  # Convert p-values to asterisks
  model_df$significance <- ifelse(model_df$p < .001, "***", 
                                  ifelse(model_df$p < .01, "**", 
                                         ifelse(model_df$p < .05, "*", "")))
  
  # Add column combining estimates, standard errors and significance
  model_df$combined <- paste0(finalfit::round_tidy(model_df$est, 2), " (", finalfit::round_tidy(model_df$se, 2), ") ", model_df$significance)
  model_df$var <- paste0(model_df$var, model_df$model)
  # Spread to wide format so that models are in columns
  wide_df <- model_df %>% 
    select(coeff, var, combined) %>% 
    spread(key = var, value = combined) %>%
    mutate(order = c(9, 8, 2, 14, 17, 11, 12, 4, 5, 6, 3, 1, 15, 7, 10, 13, 16)) %>%
    arrange(order) %>%
    select(-order) %>%
    select(coeff, `Affective polarization\n(presidents)Model 1`, `Affective polarization\n(politicians)Model 1`, `Prioritizing partisan ends\nover democratic meansModel 1`,
           `Affective polarization\n(presidents)Model 2`, `Affective polarization\n(politicians)Model 2`, `Prioritizing partisan ends\nover democratic meansModel 2`) %>%
    add_row(coeff = 'N', 
            `Affective polarization\n(presidents)Model 1` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 1', 'n'][1]),
            `Affective polarization\n(politicians)Model 1` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 1', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 1` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 1', 'n'][1]),
            `Affective polarization\n(presidents)Model 2` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 2', 'n'][1]),
            `Affective polarization\n(politicians)Model 2` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 2', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 2` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 2', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Affective polarization\n(presidents)Model 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 1', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 1', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 1', 'R2'][1], 2)),
            `Affective polarization\n(presidents)Model 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 2', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 2', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 2', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}


multiple_lm_to_latex67(models_table_67, "OLS Regressions Predicting the Heterogeneous Effects Between Partisan Identity Strength and Overall/Change in News Exposure on Affective Polarization and Prioritizing Partisan Ends over Democratic Means")






# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                            Additional tests of H3-7                                                                             ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================


#####
#####   h345 robust
#####

models_h345_robust = list()

for (dv in dvs02[1:7]){
  
  if (grepl("acc", dv) == T){
    dv <- dv
    iv <- ""
  } else {
    dv <- dv
    iv <- gsub("post", "pre", dv)
  }
  print(dv)
  print(iv)
  
  for (j in 1:2){
    
    eq = c(
      paste0(dv, " ~ age + gender + edu + race + party + relevel(factor(intervention), ref = 'control') + organic_news_w23"),
      paste0(dv, " ~ age + gender + edu + race + party + relevel(factor(intervention), ref = 'control') + organic_news_diff")
    )[j] 
    
    print(eq)
    
    modelname = c(
                  "Overall news consumption_No pre", 
                  "Difference in news consumption_No pre"
    )[j]
    
    eq_full <- lm(as.formula(eq), data = final_video_survey)
    
    models_h345_robust[[length(models_h345_robust) + 1]] <- eq_full %>%
      model_to_table() %>%
      mutate(model = paste0("Model ", j)) %>% 
      mutate(modelname = modelname) %>% 
      mutate(var = dv) %>%
      mutate(n = nobs(eq_full),
             R2 = summary(eq_full)$r.squared)
    
  }
}

models_h345_robust <- do.call(rbind, models_h345_robust)


# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 h345 Table- robust1(no pre-test value)                                                          ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================

####prepare the dataframes
models_table_345_robust <- models_h345_robust %>%
  #filter(grepl('No pre', .$modelname)) %>%
  mutate(var = dplyr::recode_factor(var,
                                    'pol_part_post' = 'Intended\npolitical participation',
                                    'perceived_acc_true' = 'Perceived accuracy\nof true claims', 
                                    'perceived_acc_false' = 'Perceived accuracy\nof false claims', 
                                    'perceived_acc_diff' = 'Perceived accuracy\nof claims (difference)', 
                                    'perceived_pol_politician_post' = 'Perceived polarization\n(politicians)',
                                    'perceived_pol_supporter_post' = 'Perceived polarization\n(party supporters)', 
                                    'perceived_pol_scale_post' = 'Perceived polarization\n(4-item scale)'
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
                          'organic_news_w23' = 'Total number of news videos watched (W2-3)',
                          'organic_news_diff' = 'Change in news consumption between W2-3 and W1',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)')
  )




#### table 345a robust ####
models_table_345a_robust <- models_table_345_robust %>%
  filter(modelname == "Overall news consumption_No pre")

# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex345a_robust <- function(model_df, caption) {
  
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
    mutate(order = c(2, 9, 10, 4, 5, 6, 3, 1, 7, 8, 11)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `Intended\npolitical participation` = as.character(model_df[model_df$var == 'Intended\npolitical participation', 'n'][1]),
            `Perceived accuracy\nof true claims` = as.character(model_df[model_df$var == 'Perceived accuracy\nof true claims', 'n'][1]),
            `Perceived accuracy\nof false claims` = as.character(model_df[model_df$var == 'Perceived accuracy\nof false claims', 'n'][1]),
            `Perceived accuracy\nof claims (difference)` = as.character(model_df[model_df$var == 'Perceived accuracy\nof claims (difference)', 'n'][1]),
            `Perceived polarization\n(politicians)` = as.character(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'n'][1]),
            `Perceived polarization\n(party supporters)` = as.character(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'n'][1]),
            `Perceived polarization\n(4-item scale)` = as.character(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Intended\npolitical participation` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Intended\npolitical participation', 'R2'][1], 2)),
            `Perceived accuracy\nof true claims` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived accuracy\nof true claims', 'R2'][1], 2)),
            `Perceived accuracy\nof false claims` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived accuracy\nof false claims', 'R2'][1], 2)),
            `Perceived accuracy\nof claims (difference)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived accuracy\nof claims (difference)', 'R2'][1], 2)),
            `Perceived polarization\n(politicians)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'R2'][1], 2)),
            `Perceived polarization\n(party supporters)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'R2'][1], 2)),
            `Perceived polarization\n(4-item scale)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}


multiple_lm_to_latex345a_robust(models_table_345a_robust, "OLS Regressions Predicting the Effects of Overall News Exposure on Intended Political Participation, Perceived Accuracy of Political Claims, and Perceived Polarization (Not Controlling for Pre-test Values)")




#### table 345b robust ####
models_table_345b_robust <- models_table_345_robust %>%
  filter(modelname == "Difference in news consumption_No pre" & !grepl("accuracy", .$var))

# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex345b_robust <- function(model_df, caption) {
  
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
    mutate(order = c(2, 11, 9, 10, 4, 5, 6, 3, 1, 7, 8)) %>%
    arrange(order) %>%
    select(-order) %>%
    add_row(coeff = 'N', 
            `Intended\npolitical participation` = as.character(model_df[model_df$var == 'Intended\npolitical participation', 'n'][1]),
            `Perceived polarization\n(politicians)` = as.character(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'n'][1]),
            `Perceived polarization\n(party supporters)` = as.character(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'n'][1]),
            `Perceived polarization\n(4-item scale)` = as.character(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Intended\npolitical participation` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Intended\npolitical participation', 'R2'][1], 2)),
            `Perceived polarization\n(politicians)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(politicians)', 'R2'][1], 2)),
            `Perceived polarization\n(party supporters)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(party supporters)', 'R2'][1], 2)),
            `Perceived polarization\n(4-item scale)` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Perceived polarization\n(4-item scale)', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}


multiple_lm_to_latex345b_robust(models_table_345b_robust, "OLS Regressions Predicting the Effects of Change in News Exposure on Intended Political Participation and Perceived Polarization (Not Controlling for Pre-test Values)")



#####
#####   h67 robust
#####




models_h67_robust = list()

for (dv in dvs03[1:3]){
  
  dv <- dv
  iv <- gsub("post", "pre", dv)
  
  print(dv)
  print(iv)
  
  for (j in 1:2){
    
    eq = c(
      paste0(dv, " ~ age + gender + edu + race + pid_imputed + relevel(factor(intervention), ref = 'control') + organic_news_w23 + pid_strength + organic_news_w23 * pid_strength"), 
      paste0(dv, " ~ age + gender + edu + race + pid_imputed + relevel(factor(intervention), ref = 'control') + organic_news_diff + pid_strength + organic_news_diff * pid_strength")
    )[j] 
    
    print(eq)
    
    modelname = c(
                  "Overall news consumption_No pre", 
                  "Difference in news consumption_No pre"
    )[j]
    
    eq_full <- lm(as.formula(eq), data = final_video_survey)
    
    models_h67_robust[[length(models_h67_robust) + 1]] <- eq_full %>%
      model_to_table() %>%
      mutate(model = paste0("Model ", j)) %>% 
      mutate(modelname = modelname) %>% 
      mutate(var = dv) %>%
      mutate(n = nobs(eq_full),
             R2 = summary(eq_full)$r.squared)
    
  }
}

models_h67_robust <- do.call(rbind, models_h67_robust)


# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 h67 Table-  robust1(no pre-test value)                                                         ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================

####prepare the dataframes
models_table_67_robust <- models_h67_robust %>%
  filter(modelname == "Overall news consumption_No pre" | modelname == "Difference in news consumption_No pre") %>%
  mutate(var = dplyr::recode_factor(var,
                                    'ap_leader_post' = 'Affective polarization\n(presidents)', 
                                    'ap_politician_post' = 'Affective polarization\n(politicians)', 
                                    'partisan_ends_post' = 'Prioritizing partisan ends\nover democratic means'
  )) %>%
  mutate(
    coeff = dplyr::recode(coeff,
                          '(Intercept)' = 'Intercept', 
                          'age' = 'Age',
                          'gendernon-male' = 'Gender (non-male)', 
                          'edulow' = 'Education (low)', 
                          'edumiddle' = 'Education (middle)', 
                          'racewhite' = 'Ethnicity (White)', 
                          'pid_imputedRepublican' = 'Partisanship (Republican)',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)',
                          'organic_news_w23' = 'Total number of news videos watched (W2-3)',
                          'organic_news_diff' = 'Change in news consumption between W2-3 and W1',
                          'pid_strength' = 'Partisan identity strength',
                          'organic_news_w23:pid_strength' = 'Total number of news videos watched (W2-3) × Partisan identity strength',
                          'organic_news_diff:pid_strength' = 'Change in news consumption between W2-3 and W1 × Partisan identity strength'
    ))



#### table 67 robust1 ####


# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex67_robust <- function(model_df, caption) {
  
  # Convert p-values to asterisks
  model_df$significance <- ifelse(model_df$p < .001, "***", 
                                  ifelse(model_df$p < .01, "**", 
                                         ifelse(model_df$p < .05, "*", "")))
  
  # Add column combining estimates, standard errors and significance
  model_df$combined <- paste0(finalfit::round_tidy(model_df$est, 2), " (", finalfit::round_tidy(model_df$se, 2), ") ", model_df$significance)
  model_df$var <- paste0(model_df$var, model_df$model)
  # Spread to wide format so that models are in columns
  wide_df <- model_df %>% 
    select(coeff, var, combined) %>% 
    spread(key = var, value = combined) %>%
    mutate(order = c(2, 12 ,14, 8, 9, 4, 5, 6, 3, 1, 10, 7, 11, 13)) %>%
    arrange(order) %>%
    select(-order) %>%
    select(coeff, `Affective polarization\n(presidents)Model 1`, `Affective polarization\n(politicians)Model 1`, `Prioritizing partisan ends\nover democratic meansModel 1`,
           `Affective polarization\n(presidents)Model 2`, `Affective polarization\n(politicians)Model 2`, `Prioritizing partisan ends\nover democratic meansModel 2`) %>%
    add_row(coeff = 'N', 
            `Affective polarization\n(presidents)Model 1` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 1', 'n'][1]),
            `Affective polarization\n(politicians)Model 1` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 1', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 1` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 1', 'n'][1]),
            `Affective polarization\n(presidents)Model 2` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 2', 'n'][1]),
            `Affective polarization\n(politicians)Model 2` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 2', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 2` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 2', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Affective polarization\n(presidents)Model 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 1', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 1', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 1', 'R2'][1], 2)),
            `Affective polarization\n(presidents)Model 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 2', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 2', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 2', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}


multiple_lm_to_latex67_robust(models_table_67_robust, "OLS Regressions Predicting the Heterogeneous Effects Between Partisan Identity Strength and Overall/Change in News Exposure on Affective Polarization and Prioritizing Partisan Ends over Democratic Means (Not Controlling for Pre-test Values)")













# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                            Additional tests of H6-7 (2nd)                                                                       ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================



models_h67_robust2 = list()

for (dv in dvs03[1:3]){
  
  dv <- dv
  iv <- gsub("post", "pre", dv)
  
  print(dv)
  print(iv)
  
  for (j in 1:4){
    
    eq = c(
      paste0(dv, " ~ age + gender + edu + race + ideology + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_w23 + ideo_strength + organic_news_w23 * ideo_strength"), 
      paste0(dv, " ~ age + gender + edu + race + ideology + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_diff + ideo_strength + organic_news_diff * ideo_strength"),
      paste0(dv, " ~ age + gender + edu + race + ideology + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_w23 + organic_news_w23 * ideology"), 
      paste0(dv, " ~ age + gender + edu + race + ideology + ", iv, " + relevel(factor(intervention), ref = 'control') + organic_news_diff + organic_news_diff * ideology")
      
    )[j] 
    
    print(eq)
    
    modelname = c("Overall news consumption_ideo", 
                  "Difference in news consumption_ideo",
                  "Overall news consumption_ideoid", 
                  "Difference in news consumption_ideoid"
                  
    )[j]
    
    eq_full <- lm(as.formula(eq), data = final_video_survey)
    
    models_h67_robust2[[length(models_h67_robust2) + 1]] <- eq_full %>%
      model_to_table() %>%
      mutate(model = paste0("Model ", j)) %>% 
      mutate(modelname = modelname) %>% 
      mutate(var = dv) %>%
      mutate(n = nobs(eq_full),
             R2 = summary(eq_full)$r.squared)
    
  }
}

models_h67_robust2 <- do.call(rbind, models_h67_robust2)


# =======================================================================================================================================================================
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                 h67 Table- robust2(ideology)                                                                    ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# ===                                                                                                                                                                 ===
# =======================================================================================================================================================================

####prepare the dataframes
models_table_67_robust2 <- models_h67_robust2 %>%
  #filter(grepl("ideo", .$modelname)) %>%
  mutate(var = dplyr::recode_factor(var,
                                    'ap_leader_post' = 'Affective polarization\n(presidents)', 
                                    'ap_politician_post' = 'Affective polarization\n(politicians)', 
                                    'partisan_ends_post' = 'Prioritizing partisan ends\nover democratic means'
  )) %>%
  mutate(
    coeff = dplyr::recode(coeff,
                          '(Intercept)' = 'Intercept', 
                          'age' = 'Age',
                          'gendernon-male' = 'Gender (non-male)', 
                          'edulow' = 'Education (low)', 
                          'edumiddle' = 'Education (middle)', 
                          'racewhite' = 'Ethnicity (White)', 
                          'ideologyliberal' = 'Ideology (Liberal)',
                          'ap_leader_pre' = 'Affective polarization\n(presidents, pre-test)',
                          'ap_politician_pre' = 'Affective polarization\n(politicians, pre-test)', 
                          'partisan_ends_pre' = 'Prioritizing partisan ends\nover democratic means(pre-test)',
                          `relevel(factor(intervention), ref = "control")background` = 'Condition (algorithm)',
                          `relevel(factor(intervention), ref = "control")banner` = 'Condition (user)',
                          'organic_news_w23' = 'Total number of news videos watched (W2-3)',
                          'organic_news_diff' = 'Change in news consumption between W2-3 and W1',
                          'ideo_strength' = 'Ideological identity strength',
                          'organic_news_w23:ideo_strength' = 'Total number of news videos watched (W2-3) × Ideological identity strength',
                          'organic_news_diff:ideo_strength' = 'Change in news consumption between W2-3 and W1 × Ideological identity strength',
                          'ideologyliberal:organic_news_w23' = 'Total number of news videos watched (W2-3) × Ideology (Liberal)',
                          'ideologyliberal:organic_news_diff' = 'Change in news consumption between W2-3 and W1  × Ideology (Liberal)'
                          
    ))

models_table_67_robust2_1 <- models_table_67_robust2 %>%
  filter(model == "Model 1" | model == "Model 2")

models_table_67_robust2_2 <- models_table_67_robust2 %>%
  filter(model == "Model 3" | model == "Model 4")

#### table 67 robuust 2 (ideology)####

# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex67_robust2_1 <- function(model_df, caption) {
  
  # Convert p-values to asterisks
  model_df$significance <- ifelse(model_df$p < .001, "***", 
                                  ifelse(model_df$p < .01, "**", 
                                         ifelse(model_df$p < .05, "*", "")))
  
  # Add column combining estimates, standard errors and significance
  model_df$combined <- paste0(finalfit::round_tidy(model_df$est, 2), " (", finalfit::round_tidy(model_df$se, 2), ") ", model_df$significance)
  model_df$var <- paste0(model_df$var, model_df$model)
  # Spread to wide format so that models are in columns
  wide_df <- model_df %>% 
    select(coeff, var, combined) %>% 
    spread(key = var, value = combined) %>%
    mutate(order = c(9, 8, 2, 14, 17, 11, 12, 4, 5, 6, 3, 15, 7, 1, 10, 13, 16)) %>%
    arrange(order) %>%
    select(-order) %>%
    select(coeff, `Affective polarization\n(presidents)Model 1`, `Affective polarization\n(politicians)Model 1`, `Prioritizing partisan ends\nover democratic meansModel 1`,
           `Affective polarization\n(presidents)Model 2`, `Affective polarization\n(politicians)Model 2`, `Prioritizing partisan ends\nover democratic meansModel 2`) %>%
    add_row(coeff = 'N', 
            `Affective polarization\n(presidents)Model 1` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 1', 'n'][1]),
            `Affective polarization\n(politicians)Model 1` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 1', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 1` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 1', 'n'][1]),
            `Affective polarization\n(presidents)Model 2` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 2', 'n'][1]),
            `Affective polarization\n(politicians)Model 2` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 2', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 2` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 2', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Affective polarization\n(presidents)Model 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 1', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 1', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 1` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 1', 'R2'][1], 2)),
            `Affective polarization\n(presidents)Model 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 2', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 2', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 2` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 2', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}


multiple_lm_to_latex67_robust2_1(models_table_67_robust2_1, "OLS Regressions Predicting the Heterogeneous Effects Between Ideological Identity Strength and Overall/Change in News Exposure on Affective Polarization and Prioritizing Partisan Ends over Democratic Means")


# Function to convert multiple lm models to LaTeX table with models in columns
multiple_lm_to_latex67_robust2_2 <- function(model_df, caption) {
  
  # Convert p-values to asterisks
  model_df$significance <- ifelse(model_df$p < .001, "***", 
                                  ifelse(model_df$p < .01, "**", 
                                         ifelse(model_df$p < .05, "*", "")))
  
  # Add column combining estimates, standard errors and significance
  model_df$combined <- paste0(finalfit::round_tidy(model_df$est, 2), " (", finalfit::round_tidy(model_df$se, 2), ") ", model_df$significance)
  model_df$var <- paste0(model_df$var, model_df$model)
  # Spread to wide format so that models are in columns
  wide_df <- model_df %>% 
    select(coeff, var, combined) %>% 
    spread(key = var, value = combined) %>%
    mutate(order = c(9, 8, 2, 14, 16, 11, 12, 4, 5, 6, 3, 7, 1, 10, 13, 15)) %>%
    arrange(order) %>%
    select(-order) %>%
    select(coeff, `Affective polarization\n(presidents)Model 3`, `Affective polarization\n(politicians)Model 3`, `Prioritizing partisan ends\nover democratic meansModel 3`,
           `Affective polarization\n(presidents)Model 4`, `Affective polarization\n(politicians)Model 4`, `Prioritizing partisan ends\nover democratic meansModel 4`) %>%
    add_row(coeff = 'N', 
            `Affective polarization\n(presidents)Model 3` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 3', 'n'][1]),
            `Affective polarization\n(politicians)Model 3` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 3', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 3` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 3', 'n'][1]),
            `Affective polarization\n(presidents)Model 4` = as.character(model_df[model_df$var == 'Affective polarization\n(presidents)Model 4', 'n'][1]),
            `Affective polarization\n(politicians)Model 4` = as.character(model_df[model_df$var == 'Affective polarization\n(politicians)Model 4', 'n'][1]),
            `Prioritizing partisan ends\nover democratic meansModel 4` = as.character(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 4', 'n'][1])
    ) %>%
    add_row(coeff = 'R2', 
            `Affective polarization\n(presidents)Model 3` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 3', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 3` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 3', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 3` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 3', 'R2'][1], 2)),
            `Affective polarization\n(presidents)Model 4` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(presidents)Model 4', 'R2'][1], 2)),
            `Affective polarization\n(politicians)Model 4` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Affective polarization\n(politicians)Model 4', 'R2'][1], 2)),
            `Prioritizing partisan ends\nover democratic meansModel 4` = as.character(finalfit::round_tidy(model_df[model_df$var == 'Prioritizing partisan ends\nover democratic meansModel 4', 'R2'][1], 2))
    ) %>%
    mutate(across(everything(), ~replace(., is.na(.), "-")))
  
  
  
  # Convert to LaTeX table
  model_latex_table <- xtable(wide_df, caption=caption)
  
  # Print the LaTeX code
  print(model_latex_table, type = "latex", caption.placement = "top", include.rownames=F)
}

multiple_lm_to_latex67_robust2_2(models_table_67_robust2_2, "OLS Regressions Predicting the Heterogeneous Effects Between Ideological Identity and Overall/Change in News Exposure on Affective Polarization and Prioritizing Partisan Ends over Democratic Means")

