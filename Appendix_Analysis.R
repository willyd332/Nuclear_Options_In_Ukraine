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

# Controls
# Militarism_Index, Internationalism_Index
# Ideology_LR, PoliticalInterest, Knowledge_Index, Time_Spent_Index
# Gender, Age, Ethnicity, Education, Employment, Income

# Create Appendix Models

# Appendix Model 1; Control x Treated; Indirect Military
a_m1_cxt_indirect <-
  lm_robust(
    Indirect ~ Treatment 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=full_group
  )

# Appendix Model 2; Control x Treated; Economic
a_m2_cxt_economic <-
  lm_robust(
    Economic ~ Treatment 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=full_group
  )

# Appendix Model 3; Control x Treated; Political
a_m3_cxt_political <-
  lm_robust(
    Political ~ Treatment 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=full_group
  )

# Appendix Model 4; Control x Treated; Overall Index
a_m4_cxt_overall_index <-
  lm_robust(
    Overall_Index ~ Treatment 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=full_group
  )

# Appendix Model 5; T1 x T2; Indirect Military
a_m5_t1xt2_indirect <-
  lm_robust(
    Indirect ~ TreatmentGroup 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=subset(full_group, TreatmentGroup!="C")
  )

# Appendix Model 6; T1 x T2; Economic
a_m6_t1xt2_economic <-
  lm_robust(
    Economic ~ TreatmentGroup 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=subset(full_group, TreatmentGroup!="C")
  )

# Appendix Model 7; T1 x T2; Political
a_m7_t1xt2_political <-
  lm_robust(
    Political ~ TreatmentGroup 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=subset(full_group, TreatmentGroup!="C")
  )

# Appendix Model 8; T1 x T2; Overall Index
a_m8_t1xt2_overall_index <-
  lm_robust(
    Overall_Index ~ TreatmentGroup 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_LR
    + PoliticalInterest
    + Knowledge_Index
    + Time_Spent_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Employment
    + Income,
    data=subset(full_group, TreatmentGroup!="C")
  )
