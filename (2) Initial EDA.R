# Final Project for 301-2
# EDA script
# John Lee

# Load packages
library(tidyverse)
library(forcats)
library(modelr)
library(haven)
library(skimr)
library(corrplot)
library(gridExtra)


## Part 2: EDA ------------------------------------------------------------

# Set the random seed to 3
set.seed(3)

# Import the df 
EDA_df <- readRDS("data/processed/EDA_df.rds")

# Look up the summary statistics of the vars (and histograms of numeric vars)
EDA_df %>% summary

EDA_df %>% names

# Code to center the plot title as the default option
theme_update(plot.title = element_text(hjust = 0.5))

# Check the distribution of the DV
EDA_df %>%
  ggplot(aes(x = strong_dem)) +
           geom_histogram() +
  ggtitle("Distribution of Democratic Party ID") +
  labs(x = "Strength of Democratic Party ID", y = "Count", fill = "Wave #")

# Create a visual of the correlation matrix
# First, just filter in the numeric vars
EDA_df_numeric <- EDA_df %>%
  select(
    strong_dem, age, dad_yrs_educ, hh_size, ln_famincome, mom_yrs_educ, 
    num_kids, num_sibs, pol_liberalism, year, yrs_educ
  )
  
corr_m_EDA <- cor(EDA_df_numeric)

# Then create a plot of the correlation matrix
corrplot(corr_m_EDA, method = "circle")

# Explore some bivariate relationships: Continuous predictors -----------------------------

# Age v. DV
EDA_df %>%
  ggplot(aes(x = age, y = strong_dem)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = "Age", y = "Strength of Democratic Party ID")

# Years v. DV
EDA_df %>%
  ggplot(aes(x = year, y = strong_dem)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = "Year", y = "Strength of Democratic Party ID")

# Political Liberalism v. DV
EDA_df %>%
  ggplot(aes(x = pol_liberalism, y = strong_dem)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  labs(x = "Political Liberalism", y = "Strength of Democratic Party ID")

# Logged Family Income v. DV
EDA_df %>%
  ggplot(aes(x = ln_famincome, y = strong_dem)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = "Ln(Family Income)", y = "Strength of Democratic Party ID")

# Years of Education v. DV
EDA_df %>%
  ggplot(aes(x = yrs_educ, y = strong_dem)) +
  geom_point() +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = "Years of Education", y = "Strength of Democratic Party ID")
             

# Explore some categorical relationships: -----------------------------
             
EDA_df %>%
  ggplot(aes(x = female, y = strong_dem)) +
  geom_boxplot() +
  labs(x = "Female", y = "Strength of Democratic Party ID")

EDA_df %>%
  ggplot(aes(x = fundamentalist, y = strong_dem)) +
  geom_boxplot() +
  labs(x = "Religious Fundamentalist", y = "Strength of Democratic Party ID")

EDA_df %>%
  ggplot(aes(x = geo_region, y = strong_dem)) +
  geom_boxplot() +
  labs(x = "Geographic Region", y = "Strength of Democratic Party ID") +
  coord_flip()

EDA_df %>%
  ggplot(aes(x = religion, y = strong_dem)) +
  geom_boxplot() +
  labs(x = "Religious Group", y = "Strength of Democratic Party ID") +
  coord_flip()

EDA_df %>%
  ggplot(aes(x = subj_classid_text, y = strong_dem)) +
  geom_boxplot() +
  labs(x = "Subjective Class ID", y = "Strength of Democratic Party ID") +
  coord_flip()

EDA_df %>%
  ggplot(aes(x = race_text, y = strong_dem)) +
  geom_boxplot() +
  labs(x = "Race", y = "Strength of Democratic Party ID") 

EDA_df %>%
  ggplot(aes(x = moresp_natwelfare, y = strong_dem)) +
  geom_boxplot() +
  labs(x = "Increase Public Spending on Welfare", y = "Strength of Democratic Party ID") 

# Explore potential interaction effects -----------------------------

# What's the baseline model? (I got this by using my knowledge of the existing literature + EDA)

# Baseline model
EDA_df %>% 
  lm(strong_dem ~ year + 
       # Demographic + social
       age + race_text + female + religion + 
       # SES
       subj_classid_text + ln_famincome + yrs_educ +
       # Policy prefs
       pol_liberalism + moresp_natwelfare + moresp_natdrug + moresp_natarms + 
       moresp_natenvir + moresp_natrace, data = .) %>%
  summary()

             
EDA_df %>% 
  lm(strong_dem ~ year * pol_liberalism, data = .) %>%
  summary()


EDA_df %>% skim


  


