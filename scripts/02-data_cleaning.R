#### Preamble ####
# Purpose: Cleans the raw plane data by two observers to prepare for analysis. This includes filtering observations, recoding variables, and selecting relevant data.
# Author: Yang Zhou
# Date: 30 March 2024
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT


library(tidyverse)


cleaned_data <- raw_data |>
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
    faminc_new = case_when(
      faminc_new == 1 ~ "Less than $10,000",
      faminc_new == 2 ~ "$10,000 - $19,999",
      faminc_new == 3 ~ "$20,000 - $29,999",
      faminc_new == 4 ~ "$30,000 - $39,999",
      faminc_new == 5 ~ "$40,000 - $49,999",
      faminc_new == 6 ~ "$50,000 - $59,999",
      faminc_new == 7 ~ "$60,000 - $69,999",
      faminc_new == 8 ~ "$70,000 - $79,999",
      faminc_new == 9 ~ "$80,000 - $99,999",
      faminc_new == 10 ~ "$100,000 - $119,999",
      faminc_new == 11 ~ "$120,000 - $149,999",
      faminc_new == 12 ~ "$150,000 - $199,999",
      faminc_new == 13 ~ "$200,000 - $249,999",
      faminc_new == 14 ~ "$250,000 - $349,999",
      faminc_new == 15 ~ "$350,000 - $499,999",
      faminc_new == 16 ~ "$500,000 or more",
      faminc_new == 17 ~ "Prefer not to say" # Corrected duplicate value for '$500,000 or more' and 'Prefer not to say'
    ),
    faminc_new = factor(
      faminc_new,
      levels = c(
        "Less than $10,000",
        "$10,000 - $19,999",
        "$20,000 - $29,999",
        "$30,000 - $39,999",
        "$40,000 - $49,999",
        "$50,000 - $59,999",
        "$60,000 - $69,999",
        "$70,000 - $79,999",
        "$80,000 - $99,999",
        "$100,000 - $119,999",
        "$120,000 - $149,999",
        "$150,000 - $199,999",
        "$200,000 - $249,999",
        "$250,000 - $349,999",
        "$350,000 - $499,999",
        "$500,000 or more",
        "Prefer not to say"
      )
    ),
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
      faminc_new %in% c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999") ~ "Low",
      faminc_new %in% c("$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999") ~ "Middle",
      faminc_new %in% c("$80,000 - $99,999", "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 - $199,999") ~ "High",
      faminc_new %in% c("$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999", "$500,000 or more", "Prefer not to say") ~ "Very High"
    )
  ) |>
  # Convert the income group to a factor with ordered levels
  mutate(
    income_group = factor(income_group, levels = c("Low", "Middle", "High", "Very High"))
  ) |>
  select(voted_for, educ, income_group, gunown) 

# Save the cleaned data
write_csv(cleaned_data, "data/analysis_data/analysis_data.csv")

