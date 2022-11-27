# Imports
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(estimatr)
library(texreg)
library(tidyverse)
library(magrittr)
library(pwr)
# Get the data
data <- read.csv('./indexed_data.csv')

full_group <- as_tibble(data)



## Balance Tests
join_hypo_balance_test <- 
  lm_robust(
    Z ~ Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=full_group)

texreg(list(join_hypo_balance_test), stars = c(0.001, 0.01, 0.05), digits = 4,)

# Chi Sqr
age_table <- table(full_group$Z, full_group$Age)
age_chisq <- chisq.test(age_table)

age_chisq$observed


"
DO THIS!
Add balance tests lm_robust(sex ~ treatment), etc... making sure that the two groups are interchangeable. Add the tables into the appendix.

You'll have to do this for each covariate. Check your slack, and then you can do a chi-square test for categorical variables.

(The fancy way to do this is a 'joint hypothesis test'. Usually people do both.)
"




full_group$Overall_Index
# Model 1; Control x Treated; Direct Military
a_cxt_overall <-
  lm_robust(
    Overall_Index ~ Z
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income
    + Median_Time,
    data=full_group
  )

texreg(list(a_cxt_overall), stars = c(0.001, 0.01, 0.05))



a_cxt_indirect <-
  lm_robust(
    Indirect ~ Z
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income
    + Median_Time,
    data=full_group
  )

texreg(list(a_cxt_indirect), stars = c(0.001, 0.01, 0.05))



m1_cxt_economic <-
  lm_robust(
    Economic ~ Z
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income
    + Median_Time,
    data=full_group
  )

texreg(list(m1_cxt_economic), stars = c(0.001, 0.01, 0.05))


m1_cxt_political <-
  lm_robust(
    Political ~ Z
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income
    + Median_Time,
    data=full_group
  )


texreg(list(m1_cxt_political), stars = c(0.001, 0.01, 0.05))