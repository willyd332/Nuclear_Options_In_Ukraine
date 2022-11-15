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
    + Employment
    + Income
    + Time_Cutoff,
    data=full_group
  )


summary(m1_cxt_direct)

# Model 2; Control x Treated; Nonmilitary
m2_cxt_nonmilitary <-
  lm_robust(
    Nonmilitary_Index ~ Z
    + Education
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
    + Time_Cutoff,
    data=full_group
  )

# Model 3; Control x Treated; General
m3_cxt_general <-
  lm_robust(
    General ~ Z
    + Education
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
    + Employment
    + Income
    + Median_Time,
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
    + Employment
    + Income
    + Median_Time,
    data=subset(full_group, TreatmentGroup!="C")
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
    + Employment
    + Income
    + Median_Time,
    data=subset(full_group, TreatmentGroup!="C")
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
    + Employment
    + Income
    + Median_Time,
    data=subset(full_group, TreatmentGroup!="C")
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
    + Employment
    + Income
    + Median_Time,
    data=subset(full_group, TreatmentGroup!="C")
  )

# Heterogeneity Models (T1 v C and T2 v C)

# Model 9; heterogeneity; Direct Military
m9_heterogeneity_direct <-
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
    + Employment
    + Income
    + Median_Time,
    data=full_group
  )

# Model 10; heterogeneity; Nonmilitary
m10_heterogeneity_nonmilitary <-
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
    + Employment
    + Income
    + Median_Time,
    data=full_group
  )

# Model 11; heterogeneity; General
m11_heterogeneity_general <-
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
    + Employment
    + Income
    + Median_Time,
    data=full_group
  )

# Model 12; heterogeneity; Threat Perception
m12_heterogeneity_threat <-
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
    + Employment
    + Income
    + Median_Time,
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
m9_heterogeneity_direct
m10_heterogeneity_nonmilitary
m11_heterogeneity_general
m12_heterogeneity_threat


# Generate Regression Output Tables

# Control vs Treatment Models

# cxt responses
texreg(list(m1_cxt_direct, m2_cxt_nonmilitary, m3_cxt_general),
       custom.coef.names = c(
         "Intercept",
         "Nuclear Rhetoric",
         "Militarism",
         "Internationalism",
         "Right Leaning",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Employed",
         "Median Income",
         "Time Cutoff"
       ),
       digits = 4,
       stars = c(0.01, 0.05, 0.1),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       # reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("Direct Military","Nonmilitary","General"),
       include.ci = FALSE,
       caption = "Control vs Nuclear Rhetoric (US Responses)",
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
       file = "./regression_outputs/cxt_responses.tex"
)

# cxt perceived threat
texreg(list(m4_cxt_threat),
       custom.coef.names = c(
         "Intercept",
         "Nuclear Rhetoric",
         "Militarism",
         "Internationalism",
         "Right Leaning",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Employed",
         "Median Income",
         "Time Cutoff"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       # reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("Perceived Threat"),
       include.ci = FALSE,
       caption = "Control vs Nuclear Rhetoric (Perceived Threat)",
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
       file = "./regression_outputs/cxt_threat.tex"
)

# T1 vs T2 Models

# t1xt2 responses
texreg(list(m5_t1xt2_direct, m6_t1xt2_nonmilitary, m7_t1xt2_general),
       custom.coef.names = c(
         "Intercept",
         "Putin Quote",
         "Militarism",
         "Internationalism",
         "Right Leaning",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Employed",
         "Median Income",
         "Time Cutoff"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       # reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("Direct Military","Nonmilitary","General"),
       include.ci = FALSE,
       caption = "Nuclear Rhetoric vs Putin Quote (US Responses)",
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
      file = "./regression_outputs/t1xt2_responses.tex"
)

# t1xt2 perceived threat
texreg(list(m8_t1xt2_threat),
       custom.coef.names = c(
         "Intercept",
         "Putin Quote",
         "Militarism",
         "Internationalism",
         "Right Leaning",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Employed",
         "Median Income",
         "Time Cutoff"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       # reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("Perceived Threat"),
       include.ci = FALSE,
       caption = "Nuclear Rhetoric vs Putin Quote (Perceived Threat)",
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
       file = "./regression_outputs/t1xt2_threat.tex"
)


# T1 vs C Models (Heterogeneity)
# T2 vs C Models (Heterogeneity)

texreg(list(m9_heterogeneity_direct, m10_heterogeneity_nonmilitary, m11_heterogeneity_general),
       custom.coef.names = c(
         "Intercept",
         "Treatment 1",
         "Treatment 2 (Putin)",
         "Militarism",
         "Internationalism",
         "Right Leaning",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Employed",
         "Median Income",
         "Time Cutoff"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       # reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("Direct","Nonmilitary","General"),
       include.ci = FALSE,
       caption = "Control vs T1 and T2 Individually (US Responses)",
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
       file = "./regression_outputs/heterogeneity_responses.tex"
)


texreg(list(m12_heterogeneity_threat),
       custom.coef.names = c(
         "Intercept",
         "Treatment 1",
         "Treatment 2 (Putin)",
         "Militarism",
         "Internationalism",
         "Right Leaning",
         "Political Interest",
         "Political Knowledge",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "College Degree",
         "Employed",
         "Median Income",
         "Time Cutoff"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       # reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("Threat"),
       include.ci = FALSE,
       caption = "Control vs T1 and T2 Individually (Perceived Threat)",
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
       file = "./regression_outputs/heterogeneity_threat.tex"
)