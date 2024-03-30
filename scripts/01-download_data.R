#### Preamble ####
# Purpose: Downloads and saves the data from the Cooperative Congressional Election Study (CCES) 2022 Survey
# Author: Rohan Alexander 
# Date: 14 March 2024
# Contact: cocoyang.zhou@mail.utoronto.ca, yuean.wang@mail.utoronto.ca, dongjun.yoon@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(dataverse)
library(tidyverse)


#### Download data ####
ces2022 <-
  get_dataframe_by_name(
    filename = "CCES22_Common_OUTPUT_vv_topost.csv",
    dataset = "10.7910/DVN/PR4L8P",
    server = "dataverse.harvard.edu",
    .f = read_csv
  ) |>
  select(votereg, presvote20post, race, gender4)

write_csv(ces2022, "data/raw_datas/raw_datas.csv")



         
