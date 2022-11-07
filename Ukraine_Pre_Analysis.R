# # Installs
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("broom")
# install.packages("ggpubr")
# install.packages("estimatr")
# install.packages("textreg")

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
data <- read.csv('./1000_Row_Dummy_Qualtrics_Data_Nov-6-2022.csv')

raw_data <- as_tibble(data)

raw_data

# Rename Columns
raw_data <- raw_data %>%
  rename(
    Consent = consent_q,
    QualtricsStatus = Status,
    Duration = Duration..in.seconds.,
    isFinishedSurvey = Finished,
    Gender = Q3,
    Age = Q4,
    Ethnicity = Q5,
    Education = Q6,
    Employment = Q7,
    Income = Q8,
    Militarism_1 = mil_inter_1,
    Militarism_2_reverse = mil_inter_2,
    Internationalism_1 = mil_inter_3,
    Internationalism_2_reverse = mil_inter_4,
    Ideology_LR = ideology_1,
    PoliticalInterest = interest,
    KnowledgeTest_Conservative = conservative,
    KnowledgeTest_NATO = nato,
    KnowledgeTest_UK = uk,
    KnowledgeTest_Zelensky = zelensky,
    AttentionCheck = attention,
    Control_First_Click = Q48_First.Click,
    Control_Last_Click = Q48_Last.Click,
    Control_Page_Submit = Q48_Page.Submit,
    Control_Click_Count = Q48_Click.Count,
    Control_Direct = direct_mil,
    Control_Indirect = indirect_mil,
    Control_Economic = economic,
    Control_Political = political,
    Control_General = general_1,
    Control_Threat = Q40_1,
    T1_First_Click = Q49_First.Click,
    T1_Last_Click = Q49_Last.Click,
    T1_Page_Submit = Q49_Page.Submit,
    T1_Click_Count = Q49_Click.Count,
    T1_Direct = Q47,
    T1_Indirect = Q48,
    T1_Economic = Q49,
    T1_Political = Q50,
    T1_General = Q51_1,
    T1_Threat = Q52_1,
    T2_First_Click = Q50_First.Click,
    T2_Last_Click = Q50_Last.Click,
    T2_Page_Submit = Q50_Page.Submit,
    T2_Click_Count = Q50_Click.Count,
    T2_Direct = Q57,
    T2_Indirect = Q58,
    T2_Economic = Q59,
    T2_Political = Q60,
    T2_General = Q61_1,
    T2_Threat = Q62_1,
  )

colnames(raw_data)

nrow(raw_data)

# Drop Unusable Rows
  # Non Consent | Under 18 | Failed Attention Check
clean_data <- raw_data %>% subset(Consent != "I don't agree" & 
                                    Age != "Less than 18 years old" & 
                                    AttentionCheck == "3")

# !!! Delete For Real Analysis !!! Delete For Real Analysis !!! Delete For Real Analysis !!!
clean_data <- raw_data
# !!! Delete For Real Analysis !!! Delete For Real Analysis !!! Delete For Real Analysis !!!

# Convert Characters To Numeric
clean_data$Control_General <- as.numeric(clean_data$Control_General)
clean_data$Control_Threat <- as.numeric(clean_data$Control_Threat)
clean_data$T1_General <- as.numeric(clean_data$T1_General)
clean_data$T1_Threat <- as.numeric(clean_data$T1_Threat)
clean_data$T2_General <- as.numeric(clean_data$T2_General)
clean_data$T2_Threat <- as.numeric(clean_data$T2_Threat)

clean_data$Control_First_Click <- as.numeric(clean_data$Control_First_Click)
clean_data$Control_Page_Submit <- as.numeric(clean_data$Control_Page_Submit)
clean_data$T1_First_Click <- as.numeric(clean_data$T1_First_Click)
clean_data$T1_Page_Submit <- as.numeric(clean_data$T1_Page_Submit)
clean_data$T2_First_Click <- as.numeric(clean_data$T2_First_Click)
clean_data$T2_Page_Submit <- as.numeric(clean_data$T2_Page_Submit)

clean_data$Ideology_LR <- as.numeric(clean_data$Ideology_LR)

  # Convert Main Outcomes
clean_data <- clean_data %>%
  mutate(Control_Direct = case_when(
    .$Control_Direct == "Send US troops to Ukraine" ~ 1,
    .$Control_Direct == "Do not send US troops to Ukraine" ~ 0,
    .$Control_Direct == "" ~ NaN
  )) %>%
  mutate(Control_Indirect = case_when(
    .$Control_Indirect == "Send military resources to Ukrainian troops" ~ 1,
    .$Control_Indirect == "Do not send military resources to Ukrainian troops" ~ 0,
    .$Control_Indirect == "" ~ NaN
  )) %>%
  mutate(Control_Economic = case_when(
    .$Control_Economic == "Enact economic sanctions on Russia" ~ 1,
    .$Control_Economic == "Do not enact economic sanctions on Russia" ~ 0,
    .$Control_Economic == "" ~ NaN
  )) %>%
  mutate(Control_Political = case_when(
    .$Control_Political == "Condemn Russia's actions in an international court" ~ 1,
    .$Control_Political == "Do not condemn Russia's actions in an international court" ~ 0,
    .$Control_Political == "" ~ NaN
  )) %>%
  mutate(T1_Direct = case_when(
    .$T1_Direct == "Send US troops to Ukraine" ~ 1,
    .$T1_Direct == "Do not send US troops to Ukraine" ~ 0,
    .$T1_Direct == "" ~ NaN
  )) %>%
  mutate(T1_Indirect = case_when(
    .$T1_Indirect == "Send military resources to Ukrainian troops" ~ 1,
    .$T1_Indirect == "Do not send military resources to Ukrainian troops" ~ 0,
    .$T1_Indirect == "" ~ NaN
  )) %>%
  mutate(T1_Economic = case_when(
    .$T1_Economic == "Enact economic sanctions on Russia" ~ 1,
    .$T1_Economic == "Do not enact economic sanctions on Russia" ~ 0,
    .$T1_Economic == "" ~ NaN
  )) %>%
  mutate(T1_Political = case_when(
    .$T1_Political == "Condemn Russia's actions in an international court" ~ 1,
    .$T1_Political == "Do not condemn Russia's actions in an international court" ~ 0,
    .$T1_Political == "" ~ NaN
  )) %>%
  mutate(T2_Direct = case_when(
    .$T2_Direct == "Send US troops to Ukraine" ~ 1,
    .$T2_Direct == "Do not send US troops to Ukraine" ~ 0,
    .$T2_Direct == "" ~ NaN
  )) %>%
  mutate(T2_Indirect = case_when(
    .$T2_Indirect == "Send military resources to Ukrainian troops" ~ 1,
    .$T2_Indirect == "Do not send military resources to Ukrainian troops" ~ 0,
    .$T2_Indirect == "" ~ NaN
  )) %>%
  mutate(T2_Economic = case_when(
    .$T2_Economic == "Enact economic sanctions on Russia" ~ 1,
    .$T2_Economic == "Do not enact economic sanctions on Russia" ~ 0,
    .$T2_Economic == "" ~ NaN
  )) %>%
  mutate(T2_Political = case_when(
    .$T2_Political == "Condemn Russia's actions in an international court" ~ 1,
    .$T2_Political == "Do not condemn Russia's actions in an international court" ~ 0,
    .$T2_Political == "" ~ NaN
  ))

# Convert Military/Internationalism To Numeric
clean_data <- clean_data %>%
  mutate(Militarism_1 = case_when(
    .$Militarism_1 == "Strongly agree" ~ 2,
    .$Militarism_1 == "Somewhat agree" ~ 1,
    .$Militarism_1 == "Neither agree nor disagree" ~ 0,
    .$Militarism_1 == "Somewhat disagree" ~ -1,
    .$Militarism_1 == "Strongly disagree" ~ -2,
  )) %>%
  mutate(Militarism_2_reverse = case_when(
    .$Militarism_2_reverse == "Strongly agree" ~ -2,
    .$Militarism_2_reverse == "Somewhat agree" ~ -1,
    .$Militarism_2_reverse == "Neither agree nor disagree" ~ 0,
    .$Militarism_2_reverse == "Somewhat disagree" ~ 1,
    .$Militarism_2_reverse == "Strongly disagree" ~ 2,
  )) %>%
  mutate(Internationalism_1 = case_when(
    .$Internationalism_1 == "Strongly agree" ~ 2,
    .$Internationalism_1 == "Somewhat agree" ~ 1,
    .$Internationalism_1 == "Neither agree nor disagree" ~ 0,
    .$Internationalism_1 == "Somewhat disagree" ~ -1,
    .$Internationalism_1 == "Strongly disagree" ~ -2,
  )) %>%
  mutate(Internationalism_2_reverse = case_when(
    .$Internationalism_2_reverse == "Strongly agree" ~ -2,
    .$Internationalism_2_reverse == "Somewhat agree" ~ -1,
    .$Internationalism_2_reverse == "Neither agree nor disagree" ~ 0,
    .$Internationalism_2_reverse == "Somewhat disagree" ~ 1,
    .$Internationalism_2_reverse == "Strongly disagree" ~ 2,
  ))
  
# Convert Political Knowledge To Numeric
clean_data <- clean_data %>%
  mutate(KnowledgeTest_Conservative = case_when(
    .$KnowledgeTest_Conservative == "Republicans" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(KnowledgeTest_NATO = case_when(
    .$KnowledgeTest_NATO == "Ukraine" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(KnowledgeTest_UK = case_when(
    .$KnowledgeTest_UK == "Boris Johnson" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(KnowledgeTest_Zelensky = case_when(
    .$KnowledgeTest_Zelensky == "Ukraine" ~ 1,
    TRUE ~ 0,
  ))

# Create an index for political knowledge
indexed_data <- clean_data 
indexed_data <- indexed_data %>%
  mutate(Knowledge_Index =
           (KnowledgeTest_Conservative +
           KnowledgeTest_NATO +
           KnowledgeTest_UK +
           KnowledgeTest_Zelensky) / 4)

# Create an index for non-military outcomes
indexed_data <- indexed_data %>%
  mutate(Control_Nonmilitary_Index = (Control_Economic + Control_Political)/2) %>%
  mutate(T1_Nonmilitary_Index = (T1_Economic + T1_Political)/2) %>%
  mutate(T2_Nonmilitary_Index = (T2_Economic + T2_Political)/2)

# Create an index for overall outcome
indexed_data <- indexed_data %>%
  mutate(Control_Overall_Index = (Control_Direct + Control_Indirect + Control_Economic + Control_Political)/4) %>% 
  mutate(T1_Overall_Index = (T1_Direct + T1_Indirect + T1_Economic + T1_Political)/4) %>%
  mutate(T2_Overall_Index = (T2_Direct + T2_Indirect + T2_Economic + T2_Political)/4)

# Create an index for time spent
indexed_data <- indexed_data %>%
  mutate(Control_Time_Spent_Index = Control_Page_Submit - Control_First_Click) %>%
  mutate(T1_Time_Spent_Index = T1_Page_Submit - T1_First_Click) %>%
  mutate(T2_Time_Spent_Index = T2_Page_Submit - T2_First_Click)

# Create an index for militarism and internationalism
indexed_data <- indexed_data %>%
  mutate(Militarism_Index = (Militarism_1 + Militarism_2_reverse)/2) %>%
  mutate(Internationalism_Index = (Internationalism_1 + Internationalism_2_reverse)/2)

# Create full group (consolidate columns)
full_group <- indexed_data %>%
  mutate(Treatment = ifelse(is.na(T1_Direct) & is.na(T2_Direct),"T","C")) %>%
  mutate(TreatmentGroup = ifelse(is.na(T2_Direct),ifelse(is.na(T1_Direct),"C","T1"),"T2")) %>%
  mutate(Direct = ifelse(is.na(T2_Direct),ifelse(is.na(T1_Direct),Control_Direct,T1_Direct),T2_Direct)) %>%
  mutate(Indirect = ifelse(is.na(T2_Indirect),ifelse(is.na(T1_Indirect),Control_Indirect,T1_Indirect),T2_Indirect)) %>%
  mutate(Economic = ifelse(is.na(T2_Economic),ifelse(is.na(T1_Economic),Control_Economic,T1_Economic),T2_Economic)) %>%
  mutate(Political = ifelse(is.na(T2_Political),ifelse(is.na(T1_Political),Control_Political,T1_Political),T2_Political)) %>%
  mutate(General = ifelse(is.na(T2_General),ifelse(is.na(T1_General),Control_General,T1_General),T2_General)) %>%
  mutate(Threat = ifelse(is.na(T2_Threat),ifelse(is.na(T1_Threat),Control_Threat,T1_Threat),T2_Threat)) %>%
  mutate(Nonmilitary_Index = ifelse(is.na(T2_Nonmilitary_Index),ifelse(is.na(T1_Nonmilitary_Index),Control_Nonmilitary_Index,T1_Nonmilitary_Index),T2_Nonmilitary_Index)) %>%
  mutate(Overall_Index = ifelse(is.na(T2_Overall_Index),ifelse(is.na(T1_Overall_Index),Control_Overall_Index,T1_Overall_Index),T2_Overall_Index)) %>%
  mutate(Time_Spent_Index = ifelse(is.na(T2_Time_Spent_Index),ifelse(is.na(T1_Time_Spent_Index),Control_Time_Spent_Index,T1_Time_Spent_Index),T2_Time_Spent_Index))


# Create treatment groups
control_group <- subset(full_group, Treatment == "C")
t_group <- subset(full_group, Treatment == "T")
t1_group <- subset(full_group, TreatmentGroup == "T1")
t2_group <- subset(full_group, TreatmentGroup == "T2")

# print("Full Group Summary")
# summary(full_group)
# print("Control Group Summary")
# summary(control_group)
# print("Treated Group Summary")
# summary(t_group)
# print("Treatment 1 Group Summary")
# summary(t1_group)
# print("Treatment 2 Group Summary")
# summary(t2_group)  

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

# Create Appendix Models
  # Control vs Treated
    # Indirect
    # Economic
    # Political
    # Overall Index

  # T1 vs T2
    # Indirect
    # Economic
    # Political
    # Overall Index


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


