#### Preamble ####
# Purpose: Tests on clean data
# Author: Yang Zhou
# Date: 30 March 2024 
# Contact: cocoyang.zhou@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Test data ####
cleaned_data <- read.csv("data/analysis_data/analysis_data.csv")
test_that("Check class", {
  expect_type(cleaned_data$educ, "character")
  expect_type(cleaned_data$faminc_new, "character")
  expect_type(cleaned_data$gunown, "character")
})

test_that("Check complete", {
  expect_true(all(complete.cases(ces2022)))
})


test_that("Check variable",{
  expect_setequal(cleaned_data$voted_for, c("Biden", "Trump"))
  expect_setequal(cleaned_data$educ, c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad"))
  expect_setequal(unique(cleaned_data$faminc_new), c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999", "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999", "$500,000 or more", NA))
})  
