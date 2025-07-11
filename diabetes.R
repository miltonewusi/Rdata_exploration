#A study to explore the potential causes and identify key risk factors influencing diabetes outcomes.

#importing relevant libraries

library(readxl)
library(tidyverse)
library(dplyr)

#loading the and viewing the dataset R studio

diabetes <- read_excel("diabetes.xlsx")
View(diabetes)

# What is the structure of the dataset? How many rows and column?
str(diabetes)

# The dataset has 500 observations or rows and 8 variables

variables <- colnames(diabetes)
variables

# summary statistic on the Age variable
summary(diabetes$Age)

# mode of Age
mode_age <- diabetes |>
  count(Age, sort = TRUE)
mode_age

# mode of age distribution is 18  

# Histogram for age
ggplot(diabetes,aes(Age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Age Distribution",
    x = "Ages",
    y = "Frequency"
  )

#replacing logical data of Sex with 2 categorical "Male" and "Female"
diabetes <- diabetes |>
  mutate(
    gender = if_else(Sex == 1, "Female", "Male")
      )
#visualization sex with bar plot
diabetes |>
  ggplot(aes(gender)) +
  geom_bar() 

# How many male and female in this study?
diabetes |>
  count(gender, sort = TRUE)

#adding a new variable for residence
diabetes <- diabetes |>
  mutate(
    residence = if_else(Residence == 1, "Rural", "Urban")
  )
diabetes |>
  ggplot(aes(residence)) +
  geom_bar() +
  labs( title = "Residence of study respondents",
        x = "Residence", y = "Frequency")

# adding a new categorical variable for religion
diabetes <- diabetes |>
  mutate(
    religion = case_when(
      Religion == 1 ~ "Christian",
      Religion == 2 ~ "Muslim",
      Religion == 3 ~ "Tradition"
    )
      )
diabetes

diabetes |>
  select(Religion, religion)

ggplot(diabetes, aes(religion)) +
  geom_bar() +
  labs(
    title = "Religion of Study participants",
    x = "Religious group",
    y = "Frequency"
  )

diabetes |>
  count(religion, sort = TRUE) 

#categorical variable for alcohol intake, obesity, and BMI
diabetes <- diabetes |>
  mutate(
    alcohol = if_else(Alcohol_Intake == 1, "Yes", "No"),
    obese = if_else(Obesity == 1, "Yes", "No"),
    bmi = case_when(
      BMI < 18.5 ~ "undwerweight",
      BMI >= 18.5 & BMI <= 24.9 ~ "healthy_weight",
      BMI >= 25 & BMI <= 29.9 ~ "overweight",
      BMI >= 30 & BMI <= 39.9 ~ "obese",
      BMI >= 40 ~ "severely_obese"
    )
  )

diabetes |>
  count(bmi, sort = TRUE)

diabetes |>
  select(BMI, bmi)

diabetes |>
  select(BMI : bmi) |>
  filter(BMI < 18.5)


# categorizing FGB into diabetes status
diabetes <- diabetes |>
  mutate(
    diabetes_status = if_else(FBG >= 3.9 & FBG <= 5.6, "Normal", "Abnormal")
           )

diabetes |>
  select(FBG, diabetes_status)

ggplot(diabetes, aes(diabetes_status)) +
  geom_bar()

diabetes |>
  count(diabetes_status, sort = TRUE)

# boxplot of age and gender to check outlier of distribution
ggplot(diabetes, aes(gender, Age)) +
  geom_boxplot()

# whishker does not show any outlier

# presenting age distribution on a histogram
ggplot(diabetes, aes(Age)) +
  geom_histogram() +
  labs(
    title = "Age distribution survey partcipants",
    subtitle = "Median age for the distribution is around 50 years",
    x = "Ages",
    y = "Frequency"
  )

# scatter plot for age and BMI to check 
ggplot(diabetes, aes(Age,BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

summary(diabetes$Age)

# running correlation for age and BMI
cor(diabetes$Age, diabetes$BMI)

#negative correlation indicationg a weak or no association between age and BMI


# BMI distribution on histogram
ggplot(diabetes, aes(BMI)) +
  geom_histogram(bins = 20) 

summary(diabetes$BMI)

# 2 cat var
ggplot(diabetes, aes(x = gender, y = religion)) +
  geom_count()


diabetes |> 
  count(gender, bmi) |>  
  ggplot(aes(x = gender, y = bmi)) +
  geom_tile(aes(fill = n))

# bmi
ggplot(diabetes, aes(bmi)) +
  geom_bar()

ggplot(diabetes, aes(fct_infreq(bmi))) +
  geom_bar()

#
ggplot(diabetes, aes(x = bmi, color = gender)) +
  geom_density(linewidth = 0.75)

ggplot(diabetes, aes(x = bmi, fill = gender)) +
  geom_bar()

ggplot(diabetes, aes(x = bmi, fill = gender)) +
  geom_bar(position = "fill")

#summary tools

install.packages("summarytools")
library(summarytools)

dfSummary(diabetes)

dfSummary(diabetes$Age)

dfSummary(diabetes$religion)

# Frequency table
freq(diabetes$gender)

# Cross-tabulation
ctable(diabetes$gender, diabetes$diabetes_status)


install.packages("gtsummary")
library(gtsummary)


diabetes |>
  select(gender, Age, diabetes_status) |>
  tbl_summary(by = gender)

diabetes |>
  select(gender, Age, religion) |>
  tbl_summary(by = religion)


install.packages("tableone")
library(tableone)

CreateTableOne(vars = c("Age", "BMI"), strata = "gender", data = diabetes)
