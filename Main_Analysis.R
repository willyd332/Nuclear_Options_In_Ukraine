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
# Ideology_LR, PoliticalInterest, Knowledge_Index, Time_Spent_Index
# Gender, Age, Ethnicity, Education, Employment, Income

# Control vs Treated Models

# Model 1; Control x Treated; Direct Military
m1_cxt_direct <-
  lm_robust(
    Direct ~ Treatment 
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

# Model 2; Control x Treated; Nonmilitary
m2_cxt_nonmilitary <-
  lm_robust(
    Nonmilitary_Index ~ Treatment 
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

# Model 3; Control x Treated; General
m3_cxt_general <-
  lm_robust(
    General ~ Treatment 
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

# Model 4; Control x Treated; Threat Perception
m4_cxt_threat <-
  lm_robust(
    Threat ~ Treatment 
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

# T1 vs T2 Models

# Model 5; T1 x T2; Direct Military
m5_t1xt2_direct <-
  lm_robust(
    Direct ~ TreatmentGroup
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

# Model 6; T1 x T2; Nonmilitary
m6_t1xt2_nonmilitary <-
  lm_robust(
    Nonmilitary_Index ~ TreatmentGroup
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

# Model 7; T1 x T2; General
m7_t1xt2_general <-
  lm_robust(
    General ~ TreatmentGroup
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

# Model 8; T1 x T2; Threat Perception
m8_t1xt2_threat <-
  lm_robust(
    Threat ~ TreatmentGroup
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


## Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis
## Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis
## Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis
## Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis Old Analysis

# Generate Regression Output...
# Check coefficient names
texreg(list(direct_model_1, direct_model_1_c, 
            direct_model_2, direct_model_2_c, 
            direct_model_3, direct_model_3_c, 
            direct_model_4, direct_model_4_c),
       include.ci = FALSE,
       digits = 3)


# Generate Custom Outputs
texreg(list(direct_model_1, direct_model_1_c, 
            direct_model_2, direct_model_2_c, 
            direct_model_3, direct_model_3_c, 
            direct_model_4, direct_model_4_c),
       custom.coef.names = c(
         "Intercept",
         "Z",
         "+ Military Assertiveness",
         "+ Internationalism",
         "Treatment 1",
         "Treatment 2"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("T1 or T2","control","T1","control","T2","control","T1 vs T2","control"),
       include.ci = FALSE,
       caption = "DIRECT WARFARE",
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
       file = "./regression_outputs/direct_warfare.tex"
)

texreg(list(indirect_model_1, indirect_model_1_c, 
            indirect_model_2, indirect_model_2_c, 
            indirect_model_3, indirect_model_3_c, 
            indirect_model_4, indirect_model_4_c),
       custom.coef.names = c(
         "Intercept",
         "Z",
         "+ Military Assertiveness",
         "+ Internationalism",
         "Treatment 1",           "Treatment 2"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("T1 or T2","control","T1","control","T2","control","T1 vs T2","control"),
       include.ci = FALSE,
       caption = "INDIRECT AID",
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
       file = "./regression_outputs/indirect_aid.tex"
)


texreg(list(economic_model_1, economic_model_1_c, 
            economic_model_2, economic_model_2_c, 
            economic_model_3, economic_model_3_c, 
            economic_model_4, economic_model_4_c),
       custom.coef.names = c(
         "Intercept",
         "Z",
         "+ Military Assertiveness",
         "+ Internationalism",
         "Treatment 1",
         "Treatment 2"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("T1 or T2","control","T1","control","T2","control","T1 vs T2","control"),
       include.ci = FALSE,
       caption = "ECONOMIC SANCTIONS",
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
       file = "./regression_outputs/economic_sanctions.tex"
)


texreg(list(political_model_1, political_model_1_c, 
            political_model_2, political_model_2_c, 
            political_model_3, political_model_3_c, 
            political_model_4, political_model_4_c),
       custom.coef.names = c(
         "Intercept",
         "Z",
         "+ Military Assertiveness",
         "+ Internationalism",
         "Treatment 1",
         "Treatment 2"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("T1 or T2","control","T1","control","T2","control","T1 vs T2","control"),
       include.ci = FALSE,
       caption = "POLITICAL CONDEMNATION",
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
       file = "./regression_outputs/political_condemnation.tex"
)


texreg(list(general_model_1, general_model_1_c, 
            general_model_2, general_model_2_c, 
            general_model_3, general_model_3_c, 
            general_model_4, general_model_4_c),
       custom.coef.names = c(
         "Intercept",
         "Z",
         "+ Military Assertiveness",
         "+ Internationalism",
         "Treatment 1",
         "Treatment 2"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
       custom.note = "%stars. Robust standard errors are in parentheses.",
       reorder.coef = c(1, 2, 5, 6, 3, 4),
       custom.model.names = c("T1 or T2","control","T1","control","T2","control","T1 vs T2","control"),
       include.ci = FALSE,
       caption = "GENERAL DEGREE OF RESPONSE",
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
       file = "./regression_outputs/general_response.tex"
)


