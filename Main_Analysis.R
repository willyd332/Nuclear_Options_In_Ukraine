# Imports
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(estimatr)
library(texreg)
library(tidyverse)
library(magrittr)
library(car)
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

# Model 3; Control x Treated; Overall
m3_cxt_overall <-
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
    + Time_Cutoff,
    data=full_group
  )

# Model 3b; Control x Treated; General
m3b_cxt_general <-
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

# Model 7; T1 x T2; Overall
m7_t1xt2_overall <-
  lm_robust(
    Overall_Index ~ TreatmentGroup
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

# Model 7b; T1 x T2; General
m7b_t1xt2_general <-
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
m3_cxt_overall
m3b_cxt_general
m4_cxt_threat
m5_t1xt2_direct
m6_t1xt2_nonmilitary
m7_t1xt2_overall
m7b_t1xt2_general
m8_t1xt2_threat


# Testing for significant differences between T1 and T2
linearHypothesis(m5_t1xt2_direct, c("TreatmentGroupT1 - TreatmentGroupT2 = 0")) # No Difference
linearHypothesis(m6_t1xt2_nonmilitary, c("TreatmentGroupT1 - TreatmentGroupT2 = 0")) # No Difference
linearHypothesis(m7_t1xt2_overall, c("TreatmentGroupT1 - TreatmentGroupT2 = 0")) # No Difference
linearHypothesis(m7b_t1xt2_general, c("TreatmentGroupT1 - TreatmentGroupT2 = 0")) # No Difference
linearHypothesis(m8_t1xt2_threat, c("TreatmentGroupT1 - TreatmentGroupT2 = 0")) # No Difference

# Generate Regression Output Tables

# texreg(list(m1_cxt_direct, m2_cxt_nonmilitary, m3_cxt_overall,
#             m5_t1xt2_direct, m6_t1xt2_nonmilitary, m7_t1xt2_overall),
texreg(list(m1_cxt_direct, m2_cxt_nonmilitary, m3_cxt_overall),       
       custom.coef.names = c(
         "Intercept",
         "Nuclear Rhetoric",
         "Military Assertiveness",
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
         "Time Cutoff"
         # "T1",
         # "T2 (w/ Quote)"
       ),
       digits = 4,
       stars = c(0.01, 0.05, 0.1),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1,2,3,4,7,6,14,5,8,9,10,11,12,13,15),
       custom.model.names = c("Direct (1)","Nonmilitary (2)","All Policies  (3)"), # ,"Direct (4)","Nonmilitary (5)","All Policies (6)"),
       include.ci = FALSE,
       caption = "Policy-Specific US Responses To The War In Ukraine",
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
       file = "./regression_outputs/policy_responses.tex"
)

# cxt perceived threat
# texreg(list(m3b_cxt_general, m7b_t1xt2_general),
texreg(list(m3b_cxt_general),
       custom.coef.names = c(
         "Intercept",
         "Nuclear Rhetoric",
         "Military Assertiveness",
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
         "Time Cutoff"
         # "T1",
         # "T2 (w/ Quote)"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1,2,3,4,7,6,14,5,8,9,10,11,12,13,15),
       custom.model.names = c("General Response (1)"), #,"General Response (2)"),
       include.ci = FALSE,
       caption = "General (Not Policy Specific) Preferred Extremity Of US Response",
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
       file = "./regression_outputs/general.tex"
)

# cxt perceived threat
# texreg(list(m4_cxt_threat, m8_t1xt2_threat),
texreg(list(m4_cxt_threat),
       custom.coef.names = c(
         "Intercept",
         "Nuclear Rhetoric",
         "Military Assertiveness",
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
         "Time Cutoff"
         # "T1",
         # "T2 (w/ Quote)"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1,2,3,4,7,6,14,5,8,9,10,11,12,13,15),
       custom.model.names = c("Perceived Threat (1)"), # ,"Perceived Threat (2)"),
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