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
prolific_data <- read.csv('./Prolific_Data.csv')

full_group <- as_tibble(data)


mean(full_group$T2_Direct)
summary(full_group$T2_Indirect)
summary(full_group$T2_Economic)
summary(full_group$T2_Political) 
summary(full_group$T2_General)
summary(full_group$T2_Threat)
summary(full_group$T2_Nonmilitary_Index)
summary(full_group$T2_Overall_Index) 

# Make Barplot To Observe Basic Pattern
mean_control_outcomes <- c(mean(full_group$Control_Direct, na.rm=TRUE),
                   mean(full_group$Control_Indirect, na.rm=TRUE),
                   mean(full_group$Control_Economic, na.rm=TRUE),
                   mean(full_group$Control_Political, na.rm=TRUE))

mean_T1_outcomes <- c(mean(full_group$T1_Direct, na.rm=TRUE),
                           mean(full_group$T1_Indirect, na.rm=TRUE),
                           mean(full_group$T1_Economic, na.rm=TRUE),
                           mean(full_group$T1_Political, na.rm=TRUE))

mean_T2_outcomes <- c(mean(full_group$T2_Direct, na.rm=TRUE),
                           mean(full_group$T2_Indirect, na.rm=TRUE),
                           mean(full_group$T2_Economic, na.rm=TRUE),
                           mean(full_group$T2_Political, na.rm=TRUE))

names <- c("Direct","Indirect","Economic","Political")

# Make Barplots
control_barplot <- barplot(height=mean_control_outcomes, 
                           names=names,
                           space=.05,
                           density=c(100,50,50,50),
                           col=c("black"),
                           main="Control Group",
                           ylab="Proportion Support",
                           xlab="Policy Option",
                           )

T1_barplot <- barplot(height=mean_T1_outcomes, 
                      names=names,
                      space=.05,
                      density=c(100,50,50,50),
                      col=c("black"),
                      main="T1 Group",
                      ylab="Proportion Support",
                      xlab="Policy Option",
                      )

T2_barplot <- barplot(height=mean_T2_outcomes, 
                      names=names,
                      space=.05,
                      density=c(100,50,50,50),
                      col=c("black"),
                      main="T2 Group",
                      ylab="Proportion Support",
                      xlab="Policy Option",
                      )

control_barplot
T1_barplot
T2_barplot

m1_cxt_direct <-
  lm_robust(
    Direct ~ Z
    + Z * Education
    + Z * PoliticalInterest
    + Z * Knowledge_Index
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + Gender
    + Age
    + Ethnicity
    + Z * Time_Cutoff, 
    data=full_group
  )

m2_cxt_nonmilitary <-
  lm_robust(
    Nonmilitary_Index ~ Z
    + Z * Education
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + Gender
    + Age
    + Ethnicity
    + Z * Time_Cutoff, 
    data=full_group
  )

m3_cxt_general <-
  lm_robust(
    General ~ Z
    + Z * Education
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + Gender
    + Age
    + Ethnicity
    + Z * Time_Cutoff, 
    data=full_group
  )


# Model 4; Control x Treated; Threat Perception
m4_cxt_threat <-
  lm_robust(
    Threat ~ Z 
    + Education
    + PoliticalInterest
    + Knowledge_Index
    + Militarism_Index
    + Internationalism_Index
    + Ideology_Index
    + Gender
    + Age
    + Ethnicity
    + Time_Cutoff, 
    data=full_group
  )



texreg(list(m1_cxt_direct, m2_cxt_nonmilitary, m3_cxt_general))

texreg(list(m4_cxt_threat, m4_cxt_threat, m4_cxt_threat),
       custom.coef.names = c(
         "Intercept",
         "Treated",
         "College Educated",
         "Political Interest",
         "Political Knowledge",
         "Militarism",
         "Internationalism",
         "Right Leaning",
         "Male",
         "Age (25-34)",
         "Age (35-44)",
         "Age (45-54)",
         "Age (55+)",
         "Caucasian",
         "Time"
       ),
       digits = 4,
       stars = c(0.001, 0.01, 0.05),
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
       # file = "./regression_outputs/cxt_responses.tex"
)

