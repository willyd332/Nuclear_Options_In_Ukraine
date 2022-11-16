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
# Create Main Models
# Controls
# Militarism_Index, Internationalism_Index
# Ideology_Index, PoliticalInterest, Knowledge_Index, Time_Spent_Index
# Gender, Age, Ethnicity, Education, Employment, Income

# Control vs Treated Models

# Model 1; Control x Treated; Direct Military
m1_cxt_direct <-
  lm_robust(
    Direct ~ Z
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )


summary(m1_cxt_direct)

# Model 2; Control x Treated; Nonmilitary
m2_cxt_nonmilitary <-
  lm_robust(
    Nonmilitary_Index ~ Z
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )

# Model 3; Control x Treated; General
m3_cxt_general <-
  lm_robust(
    General ~ Z
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )

# Model 4; Control x Treated; Threat Perception
m4_cxt_threat <-
  lm_robust(
    Threat ~ Z 
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )

# T1 vs T2 Models

# Model 5; T1 x T2; Direct Military
m5_t1xt2_direct <-
  lm_robust(
    Direct ~ TreatmentGroup
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )

# Model 6; T1 x T2; Nonmilitary
m6_t1xt2_nonmilitary <-
  lm_robust(
    Nonmilitary_Index ~ TreatmentGroup
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )

# Model 7; T1 x T2; General
m7_t1xt2_general <-
  lm_robust(
    General ~ TreatmentGroup
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )

# Model 8; T1 x T2; Threat Perception
m8_t1xt2_threat <-
  lm_robust(
    Threat ~ TreatmentGroup
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + PoliticalInterest
    + Knowledge_Index
    + Gender
    + Age
    + Ethnicity
    + Education
    + Time_Cutoff,
    data=full_group
  )

full_group$
full_group$TreatmentGroup

# All Models
m1_cxt_direct
m2_cxt_nonmilitary
m3_cxt_general
m4_cxt_threat
m5_t1xt2_direct
m6_t1xt2_nonmilitary
m7_t1xt2_general
m8_t1xt2_threat


# Generate Regression Output Tables
# cxt responses
texreg(list(m1_cxt_direct, m2_cxt_nonmilitary, m3_cxt_general,
            m5_t1xt2_direct, m6_t1xt2_nonmilitary, m7_t1xt2_general),
       custom.coef.names = c(
         "Intercept",
         "Z",
         "Militarism",
         "Internationalism",
         "Right Leaning Ideology",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Time Cutoff",
         "T1",
         "T2 (w/ Quote)"
       ),
       digits = 4,
       stars = c(0.01, 0.05, 0.1),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1,2,16,17,3,4,7,6,14,5,8,9,10,11,12,13,15),
       custom.model.names = c("Direct (1)","Nonmilitary (2)","General (3)","Direct (4)","Nonmilitary (5)","General (6)"),
       include.ci = FALSE,
       caption = "Selected US Policy Responses To The War In Ukraine Under Nuclear Rhetoric",
       caption.above = TRUE,
       # include.adjrs = FALSE,
       # include.rmse = FALSE,
       dcolumn = TRUE,
       booktabs = TRUE,
       sideways = TRUE,
       scalebox = 0.85,
       float.pos = "t",
       use.packages = FALSE,
       # table = FALSE,
       file = "./regression_outputs/policy_responses.tex"
)

# cxt perceived threat
texreg(list(m4_cxt_threat, m8_t1xt2_threat),
       custom.coef.names = c(
         "Intercept",
         "Z",
         "Militarism",
         "Internationalism",
         "Right Leaning Ideology",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Time Cutoff",
         "T1",
         "T2 (w/ Quote)"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1,2,16,17,3,4,7,6,14,5,8,9,10,11,12,13,15),
       custom.model.names = c("Perceived Threat (1)","Perceived Threat (2)"),
       include.ci = FALSE,
       caption = "Perceived Russian Threat Under Nuclear Rhetoric",
       caption.above = TRUE,
       # include.adjrs = FALSE,
       # include.rmse = FALSE,
       dcolumn = TRUE,
       booktabs = TRUE,
       sideways = FALSE,
       scalebox = 0.85,
       float.pos = "t",
       use.packages = FALSE,
       # table = FALSE,
       file = "./regression_outputs/threat.tex"
)