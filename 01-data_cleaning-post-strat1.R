#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from IPUMS USA
# Author: Cathy Yang and Promiti Datta
# Data: 22 October 2020
# Contact: cathym.yang@mail.utoronto.ca; promiti.datta@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/cathyyang/OneDrive - University of Toronto/4th Year Drive/STA304/Problem Set 3")

# Read in the raw data.
raw_data <- read_dta("usa_00002.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(region,
         statefip,
         sex, 
         age, 
         race, 
         hispan,
         marst, 
         bpl,
         #bpld,
         hcovany,
         #hcovpriv,
         #hinsemp,
         #hinscaid,
         #hinscare,
         citizen,
         educd,
         #educ,
         labforce,
         inctot
         #occscore
         )
         
#### What's next? ####

## Creating gender groups, age groups, race groups, education levels

#Filter out subjects under 18
reduced_data <- reduced_data %>% 
  filter(age > 17)

#Re-categorizing gender
reduced_data$gender = case_when(
  reduced_data$sex == 1 ~ "Male",
  reduced_data$sex == 2 ~ "Female")

reduced_data$gender <- as.factor(reduced_data$gender)

#Age groups
reduced_data$age_group = case_when(
  reduced_data$age >= 18 & reduced_data$age <= 29 ~ "18 - 29",
  reduced_data$age >= 30 & reduced_data$age <= 44 ~ "30 - 44",
  reduced_data$age >= 45 & reduced_data$age <= 59 ~ "45 - 59",
  reduced_data$age >= 60 ~ "60+")

reduced_data$age_group <- as.factor(reduced_data$age_group)

#Race groups
reduced_data$race_group = case_when(
  reduced_data$race == 1 ~ "White",
  reduced_data$race == 2 ~ "Black/African American",
  reduced_data$race == 3 ~ "Native American or Alaska Native",
  reduced_data$race == 4 | reduced_data$race == 5 ~ "Chinese or Japanese",
  reduced_data$race == 6 ~ "Other Asian or Pacific Islander",
  reduced_data$race == 7 | reduced_data$race == 8 | reduced_data$race == 9 ~ "Other",
  reduced_data$hispan == 1 | reduced_data$hispan == 2 | reduced_data$hispan == 3
  | reduced_data$hispan == 4 ~ "Hispanic")

reduced_data$race_group <- as.factor(reduced_data$race_group)

#Highest education levels
reduced_data$education_group = case_when(
  reduced_data$educd >= 000 & reduced_data$educd <= 61 ~ "Less than High School",
  reduced_data$educd >= 062 & reduced_data$educd <= 064 ~ "High School Graduate",
  reduced_data$educd >= 065 & reduced_data$educd <= 100 ~ "Some College",
  reduced_data$educd >= 101 & reduced_data$educd <= 116 ~ "College Graduate")

reduced_data$education_group <- as.factor(reduced_data$education_group)

#Creating the cells
reduced_data <- 
  reduced_data %>%
  count(age_group, gender, race_group, statefip, education_group) %>%
  group_by(age_group, gender, race_group, statefip, education_group) 

# Saving the census data as a csv file in working directory
write_csv(reduced_data, "census_data.csv")
