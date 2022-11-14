# Imports
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(estimatr)
library(texreg)
library(tidyverse)
library(magrittr)
# Get the data
data <- read.csv('./indexed_data.csv')

full_group <- as_tibble(data)


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