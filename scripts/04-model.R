#### Preamble ####
# Purpose: Models the relationship between voting preference and predictors of ducation, income, and personal gun ownership by logistic regression. It will implement with Bayesian methods via the rstanarm package.
# Author: Yang Zhou
# Date: 30 March 2024
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(modelsummary)

#### Read data ####
analysis_data <- read_csv("data/analysis_data/analysis_data.csv")

### Model data ####
set.seed(853)

ces2022_reduced <- 
  analysis_data |> 
  slice_sample(n = 500)

us_political_preferences <-
  stan_glm(
    factor(voted_for) ~ educ + faminc_new + gunown,
    data = ces2022_reduced ,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )



#### Save model ####
saveRDS(
  us_political_preferences,
  file = "model/first_model.rds"
)


modelsummary(
  list(
    "Support Biden" = us_political_preferences
  ),
  statistic = "mad"
)


