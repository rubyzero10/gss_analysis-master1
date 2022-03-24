#### Preamble ####
# Purpose: Prepare the 2021 GSS data
# Author: Oluwabusayomi Adekuajo 
# Data: 13 March 2022
# Contact: bussy.adekuajo@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the GSS data and saved it to inputs/data

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- haven::read_dta("inputs/data/2021_stata/gss2021.dta")


#### Prepare data ####
# Just keep some variables that may be of interest
# marital, martype, divorce, widowed, age, educ, degree, childs, AGEKDBRN, income , RELIG, LIFE
# HAPPY, HAPMAR, HAPCOHAB

#Can add and later make tables with ABSINGLE, 
#Can add about children and happiness see the correlateion bwteen kids and happines with martial color 
# cost of raisingkids marriage income and kids 
# Life happy and number kids 
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(marital, divorce, widowed, agekdbrn, happy, hapmar, hapcohab,
         age, degree, income, relig, life) %>% 
  rename(relgion = relig)
#Variables divorce only apply to those that have been married or widowed
#variable widowed apply to those that are currently married have sperated or divorced 
#Var hapmar apply only to those married 
#Var hapcohab only apply if currently unmarried 
#Var agekdbrn only apply if have child 
#So we keep the N/A in these above cases 
reduced_data <- 
  reduced_data %>% 
  drop_na(marital)%>%
  drop_na(happy)%>% 
  drop_na(age)%>% 
  drop_na(degree)%>% 
  drop_na(income)%>% 
  drop_na(relgion)%>% 
  drop_na(life)


rm(raw_data)

#### Recode gender ####
# The question for SEXNOW1 is:
# "Do you describe yourself as male, female, or transgender?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <- 
  reduced_data %>% 
  #mutate(age = replace_na(age, 0))%>%
  mutate(marital = case_when(
    marital == 1 ~ "Married",
    marital == 2 ~ "Widowed",
    marital == 3 ~ "Divorced",
    marital == 4 ~ "Separated",
    marital == 5 ~ "Never Married"))%>%
  mutate(divorce = case_when(
    divorce == 1 ~ "Yes", 
    divorce == 2 ~ "No "))%>%
  mutate(widowed = case_when(
    widowed == 1 ~ "Yes", 
    widowed == 2 ~ "No "))%>%
  mutate(happy = case_when(
    happy == 1 ~ "Very Happy", 
    happy == 2 ~ "Pretyy Happy",
    happy == 3 ~ "Not Too Happy"))%>%
  mutate(hapmar = case_when(
    hapmar == 1 ~ "Very Happy", 
    hapmar == 2 ~ "Pretyy Happy",
    hapmar == 3 ~ "Not Too Happy"))%>%
  mutate(hapcohab = case_when(
    hapcohab == 1 ~ "Very Happy", 
    hapcohab == 2 ~ "Pretyy Happy",
    hapcohab == 3 ~ "Not Too Happy"))%>%
  mutate(degree = case_when(
    degree == 0 ~ "<High School", 
    degree == 1 ~ "High School",
    degree == 2 ~ "Associate",
    degree == 3 ~ "Bachelors", 
    degree == 4 ~ "Graduate"))%>%
  mutate(income = case_when(
    income == 1 ~ "<$1,000", 
    income == 2 ~ "$1,000 to $2,999",
    income == 3 ~ "$3,000 to $3,999",
    income == 4 ~ "$4,000 to $4,999", 
    income == 5 ~ "$5,000 to $5,999",
    income == 6 ~ "$6,000 to $6,999", 
    income == 7 ~ "$7,000 to $7,999",
    income == 8 ~ "$8,000 to $9,999",
    income == 9 ~ "$10,000 to $14,999", 
    income == 10 ~ "$15,000 to $19,999", 
    income == 11 ~ "$20,000 to $24,999", 
    income == 12 ~ "$25,000 or more",
    income == 13 ~ "Refused"))%>%
  mutate(relgion = case_when(
    relgion == 1 ~ "Protestant", 
    relgion == 2 ~ "Catholic ",
    relgion == 3 ~ "Jewish ",
    relgion == 4 ~ "None", 
    relgion == 5 ~ "Other",
    relgion == 6 ~ "Buddhism",
    relgion == 7 ~ "Hinduism",
    relgion == 8 ~ "Other Eatsern Religions",
    relgion == 9 ~ "Muslim",
    relgion == 10 ~ "Orthodox-Christian", 
    relgion == 11 ~ "Christian", 
    relgion == 12 ~ "Native-American", 
    relgion == 13 ~ "Inter-Nondenominational"))%>%
  mutate(life = case_when(
    life == 1 ~ "Exciting", 
    life == 2 ~ "Routine",
    life == 3 ~ "Dull"))




#### Save ####
write_csv(reduced_data, "outputs/data/prepared_gss.csv")
write.csv(reduced_data, file="prepared_gss.csv")




