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

#final_video_survey <- read_csv("final_video_survey_additional_1.csv")
final_video_survey <- read_csv("final_video_survey_additional_5.csv")

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

# ggsave(paste0(figdir, "fig1_additional_1.png"),
#        device = "png", width = 4, height = 5)
ggsave(paste0(figdir, "fig1_additional_5.png"),
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

#multiple_lm_to_latex1a(models_table_1a, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Number Per Active Day, Participants Watched at Least One Video During Weeks 2-3)")
multiple_lm_to_latex1a(models_table_1a, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Number Per Active Day, Participants Watched at Least Five YouTube Videos Over Weeks 1-3 and at Least One Video During weeks 2-3)")





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


#multiple_lm_to_latex1b(models_table_1b, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Percentage, Participants Watched at Least One Video During Weeks 2-3)")
multiple_lm_to_latex1b(models_table_1b, "OLS Regressions Predicting the Effects of Treatments on News Watching and Political Watching (Percentage, Participants Watched at Least Five YouTube Videos Over Weeks 1-3 and at Least One Video During weeks 2-3)")

