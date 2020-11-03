#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund Voter Study Group
# Author: Cathy Yang and Promiti Datta
# Data: 22 October 2020
# Contact: cathym.yang@mail.utoronto.ca; promiti.datta@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/OneDrive - University of Toronto/4th Year Drive/STA304/Problem Set 3")

# Read in the raw data 
raw_survey_data <- read_dta("ns20200625/ns20200625.dta")

# Add the labels
raw_survey_data <- labelled::to_factor(reduced_survey_data)

# Just keep some variables
reduced_survey_data <- 
  raw_survey_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)

# Make gender, age, race, and education groups

reduced_survey_data$gender <- as.factor(reduced_survey_data$gender)

reduced_survey_data$sex = case_when(
  reduced_survey_data$gender == "Male" ~ 1,
  reduced_survey_data$gender == "Female" ~ 2)

reduced_survey_data$age_group = case_when(
  reduced_survey_data$age >= 18 & reduced_survey_data$age <= 29 ~ "18 - 29",
  reduced_survey_data$age >= 30 & reduced_survey_data$age <= 44 ~ "30 - 44",
  reduced_survey_data$age >= 45 & reduced_survey_data$age <= 59 ~ "45 - 59",
  reduced_survey_data$age >= 60 ~ "60+")

reduced_survey_data$age_group <- as.factor(reduced_survey_data$age_group)

reduced_survey_data$race_group = case_when(
  reduced_survey_data$race_ethnicity == "White" ~ "White",
  reduced_survey_data$race_ethnicity == "Black, or African American" ~ "Black/African American",
  reduced_survey_data$race_ethnicity == "American Indian or Alaska Native" ~ "Native American or Alaska Native",
  reduced_survey_data$race_ethnicity == "Asian (Chinese)" | reduced_survey_data$race_ethnicity == "Asian (Japanese)" ~ "Chinese or Japanese",
  reduced_survey_data$race_ethnicity == "Asian (Asian Indian)" |
  reduced_survey_data$race_ethnicity == "Asian (Filipino)" |
  reduced_survey_data$race_ethnicity == "Asian (Korean)" |
  reduced_survey_data$race_ethnicity == "Asian (Vietnamese)" |
  reduced_survey_data$race_ethnicity == "Asian (Other)" |
  reduced_survey_data$race_ethnicity == "Pacific Islander (Native Hawaiian)" | 
  reduced_survey_data$race_ethnicity == "Pacific Islander (Guamanian)" |
  reduced_survey_data$race_ethnicity == "Pacific Islander (Samoan)" |
  reduced_survey_data$race_ethnicity == "Pacific Islander (Other)"
  ~ "Other Asian or Pacific Islander",
  reduced_survey_data$race_ethnicity == "Some other race" ~ "Other",
  reduced_survey_data$hispanic != "Not Hispanic" ~ "Hispanic")

reduced_survey_data$race_group <- as.factor(reduced_survey_data$race_group)

reduced_survey_data$education_group = case_when(
  reduced_survey_data$education == "3rd Grade or less" |
  reduced_survey_data$education == "Middle School - Grades 4 - 8" |
  reduced_survey_data$education == "Completed some high school" ~ "Less than High School",
  reduced_survey_data$education == "High school graduate" |
  reduced_survey_data$education == "Other post high school vocational training" ~ "High School Graduate",
  reduced_survey_data$education == "Completed some college, but no degree" |
  reduced_survey_data$education == "Associate Degree" ~ "Some College",
  reduced_survey_data$education == "College Degree (such as B.A., B.S.)" |
  reduced_survey_data$education == "Completed some graduate, but no degree" |
  reduced_survey_data$education == "Masters degree" |
  reduced_survey_data$education == "Doctorate degree" ~ "College Graduate")

reduced_survey_data$education_group <- as.factor(reduced_survey_data$education_group)

# Make binary vote columns for Trump and Biden

reduced_survey_data<-
  reduced_survey_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

reduced_survey_data <-
  reduced_survey_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))

# Saving the survey/sample data as a csv file in my working directory
write_csv(reduced_survey_data, "survey_data.csv")

