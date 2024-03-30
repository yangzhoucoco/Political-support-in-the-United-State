#### Preamble ####
# Purpose: Tests on clean data
# Author: Yang Zhou, Yuean Wang, Dong Jun Yoon
# Date: 14 March 2024 
# Contact: cocoyang.zhou@mail.utoronto.ca, yuean.wang@mail.utoronto.ca, dongjun.yoon@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Test data ####
cleaned_data <- read.csv("data/analysis_data/analysis_data.csv")
test_that("Check class", {
  expect_type(cleaned_data$race, "character")
  expect_type(cleaned_data$gender4, "character")
  expect_type(cleaned_data$voted_for, "character")
})

test_that("Check complete", {
  expect_true(all(complete.cases(ces2022)))
})


test_that("Check variable",{
  expect_setequal(cleaned_data$voted_for, c("Biden", "Trump"))
  expect_setequal(cleaned_data$gender4, c("Female", "Male"))
  expect_setequal(cleaned_data$race, c("White", "Black", "Hispanic", "Asian", "Native American", "Middle Eastern", "Two or more races", "Other"))
})    
