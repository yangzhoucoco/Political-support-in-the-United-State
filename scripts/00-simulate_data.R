#### Preamble ####
# Purpose: Simulates political support in United States data based on education, income, and personal gun ownership 
# Author: Yang Zhou
# Date: 30 March 2024 
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(dplyr)

#### Simulate data ####
set.seed(853)

num_obs <- 800

us_political_preferences <- tibble(
  education = sample(0:3, size = num_obs, replace = TRUE),
  income = sample(0:4, size = num_obs, replace = TRUE),
  personal_gun_ownership = sample(0:3, size = num_obs, replace = TRUE),
  support_prob = ((education + income + personal_gun_ownership) / 5),
) |>
  mutate(
    supports_biden = if_else(runif(n = num_obs) < support_prob, "yes", "no"),
    education = case_when(
      education == 0 ~ "High school or less",
      education == 1 ~ "Some college",
      education == 2 ~ "College graduate",
      education == 3 ~ "Postgraduate study",
    ),
    income = case_when(
      income == 0 ~ "Under $30,000",
      income == 1 ~ "$30,000 to $49,999",
      income == 2 ~ "$50,000 to $99,999",
      income == 3 ~ "$100,000 to $199,000",
      income == 4 ~ "$200,000 or more",
    ),
    personal_gun_ownership = case_when(
      personal_gun_ownership == 0 ~ "Personally own a gun",
      personal_gun_ownership == 1 ~ "Don't personally own a gun, but someone in the household owns a gun",
      personal_gun_ownership == 2 ~ "No one in the household owns a gun",
      personal_gun_ownership == 3 ~ "Not sure",
    )
)|>
  select(-support_prob, supports_biden, education, income, personal_gun_ownership)


write.csv(us_political_preferences, "data/simulate_data/simulate_data.csv")
