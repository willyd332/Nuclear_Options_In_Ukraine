# Imports
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(estimatr)
library(texreg)
library(tidyverse)
library(magrittr)
library(Hmisc)
# Get the data
data <- read.csv('./indexed_data.csv')
prolific_data <- read.csv('./Prolific_Data.csv')
raw_data <- read.csv('./Final_Data.csv')

full_group <- as_tibble(data)


mean(full_group$T2_Direct)
summary(full_group$T2_Indirect)
summary(full_group$T2_Economic)
summary(full_group$T2_Political) 
summary(full_group$T2_General)
summary(full_group$T2_Threat)
summary(full_group$T2_Nonmilitary_Index)
summary(full_group$T2_Overall_Index) 

# Descriptive Statistics
prop.table(table(full_group$Age))
# 18-24 years old 25-34 years old 35-44 years old 45-54 years old   55+ years old 
# 0.1312570       0.3615128       0.2736374       0.1156841       0.1179088

prop.table(table(full_group$Education))
# 0         1 
# 0.3982202 0.6017798 

prop.table(table(raw_data$Q5))
# 0         1 
# 0.2068966 0.7931034 

table(full_group$TreatmentGroup)
# C  T1  T2 
# 299 300 300 

describe(full_group$Time_Spent_Index)


# Make Barplot To Observe Basic Pattern
mean_control_outcomes <- c(mean(full_group$Control_Direct, na.rm=TRUE),
                   mean(full_group$Control_Indirect, na.rm=TRUE),
                   mean(full_group$Control_Economic, na.rm=TRUE),
                   mean(full_group$Control_Political, na.rm=TRUE))

full_group_T <- subset(full_group, Treatment == "T") %>%
  mutate(T_Direct = ifelse(is.na(T2_Direct), ifelse(is.na(T1_Direct),NA,T1_Direct),T2_Direct)) %>%
  mutate(T_Indirect = ifelse(is.na(T2_Indirect), ifelse(is.na(T1_Indirect),NA,T1_Indirect),T2_Indirect)) %>%
  mutate(T_Economic = ifelse(is.na(T2_Economic), ifelse(is.na(T1_Economic),NA,T1_Economic),T2_Economic)) %>%
  mutate(T_Political = ifelse(is.na(T2_Political), ifelse(is.na(T1_Political),NA,T1_Political),T2_Political))

mean_T_outcomes <- c(mean(full_group_T$T_Direct, na.rm=TRUE),
                           mean(full_group_T$T_Indirect, na.rm=TRUE),
                           mean(full_group_T$T_Economic, na.rm=TRUE),
                           mean(full_group_T$T_Political, na.rm=TRUE))


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

T_barplot <- barplot(height=mean_T_outcomes, 
                      names=names,
                      space=.05,
                      density=c(100,50,50,50),
                      col=c("black"),
                      main="Treatment Group",
                      ylab="Proportion Support",
                      xlab="Policy Option",
                      )

control_barplot
T_barplot

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

