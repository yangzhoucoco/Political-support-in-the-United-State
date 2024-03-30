#### Preamble ####
# Purpose: Cleans the raw plane data by two observers to prepare for analysis. This includes filtering observations, recoding variables, and selecting relevant data.
# Author: Yang Zhou, Yuean Wang, Dong Jun Yoon
# Date: 14 March 2024
# Contact: cocoyang.zhou@mail.utoronto.ca, yuean.wang@mail.utoronto.ca, dongjun.yoon@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/raw_datas/raw_datas.csv")

cleaned_data <-
  raw_data |>
  filter(votereg == 1,
         presvote20post %in% c(1, 2),
         gender4 %in% c(1, 2)) |> 
  mutate(
    voted_for = if_else(presvote20post == 1, "Biden", "Trump"),
    voted_for = as_factor(voted_for),
    gender4 = if_else(gender4 == 1, "Male", "Female"),
    race = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race == 3 ~ "Hispanic",
      race == 4 ~ "Asian",
      race == 5 ~ "Native American",
      race == 6 ~ "Middle Eastern",
      race == 7 ~ "Two or more races",
      race == 8 ~ "Other"
    ),
    race = factor(
      race,
      levels = c(
        "White",
        "Black",
        "Hispanic",
        "Asian",
        "Native American",
        "Middle Eastern",
        "Two or more races",
        "Other"
      )
    )
  ) |>
  select(voted_for, gender4, race)


#### Save data ####
write_csv(cleaned_data, "data/analysis_data/analysis_data.csv")
