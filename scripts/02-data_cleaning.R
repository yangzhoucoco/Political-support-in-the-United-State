#### Preamble ####
# Purpose: Cleans the raw plane data by two observers to prepare for analysis. This includes filtering observations, recoding variables, and selecting relevant data.
# Author: Yang Zhou
# Date: 30 March 2024
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT


library(tidyverse)
library(readr)
library(here)


cleaned_data <- ces2022 |>
  filter(votereg == 1, presvote20post %in% c(1, 2)) |>
  mutate(
    voted_for = if_else(presvote20post == 1, "Biden", "Trump"),
    voted_for = as_factor(voted_for),
    educ = case_when(
      educ == 1 ~ "No HS",
      educ == 2 ~ "High school graduate",
      educ == 3 ~ "Some college",
      educ == 4 ~ "2-year",
      educ == 5 ~ "4-year",
      educ == 6 ~ "Post-grad"
    ),
    educ = factor(educ, levels = c(
      "No HS", 
      "High school graduate",
      "Some college",
      "2-year",
      "4-year",
      "Post-grad"
    )),
    gunown = case_when(
      gunown == 1 ~ "Personally own a gun",
      gunown == 2 ~ "Someone in the household owns a gun",
      gunown == 3 ~ "No one in the household owns a gun",
      gunown == 4 ~ "Not sure"
    ),
    gunown = factor(
      gunown,
      levels = c(
        "Personally own a gun",
        "Someone in the household owns a gun",
        "No one in the household owns a gun",
        "Not sure"
      )
    ),
    income_group = case_when(
      faminc_new == 1 ~ "Low",
      faminc_new == 2 ~ "Low",
      faminc_new == 3 ~ "Low",
      faminc_new == 4 ~ "Low",
      faminc_new == 5 ~ "Middle",
      faminc_new == 6 ~ "Middle",
      faminc_new == 7 ~ "Middle",
      faminc_new == 8 ~ "Middle",
      faminc_new == 9 ~ "High",
      faminc_new == 10 ~ "High",
      faminc_new == 11 ~ "High",
      faminc_new == 12 ~ "High",
      faminc_new >= 13 ~ "Very High" 
    )
  ) |>
  mutate(
    income_group = factor(income_group, levels = c("Low", "Middle", "High", "Very High"))
  ) |>
  select(voted_for, educ, income_group, gunown)

# Save the cleaned data
write_csv(cleaned_data, "data/analysis_data/analysis_data.csv")
