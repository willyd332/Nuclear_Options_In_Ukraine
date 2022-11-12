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

# Drop Unusable Rows
  # Non Consent | Under 18 | Failed Attention Check
clean_data <- raw_data %>% subset(Consent != "I don't agree" & 
                                    Age != "Less than 18 years old")

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

# Convert Main Outcomes to binary 1-0s where 1 is taking the action and 0 is no action.
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

# Code Military/Internationalism to a 3 point scale.
clean_data <- clean_data %>%
  mutate(Militarism_1 = case_when(
    .$Militarism_1 == "Strongly agree" ~ 1,
    .$Militarism_1 == "Somewhat agree" ~ 1,
    .$Militarism_1 == "Neither agree nor disagree" ~ 0,
    .$Militarism_1 == "Somewhat disagree" ~ -1,
    .$Militarism_1 == "Strongly disagree" ~ -1,
  )) %>%
  mutate(Militarism_2_reverse = case_when(
    .$Militarism_2_reverse == "Strongly agree" ~ -1,
    .$Militarism_2_reverse == "Somewhat agree" ~ -1,
    .$Militarism_2_reverse == "Neither agree nor disagree" ~ 0,
    .$Militarism_2_reverse == "Somewhat disagree" ~ 1,
    .$Militarism_2_reverse == "Strongly disagree" ~ 1,
  )) %>%
  mutate(Internationalism_1 = case_when(
    .$Internationalism_1 == "Strongly agree" ~ 1,
    .$Internationalism_1 == "Somewhat agree" ~ 1,
    .$Internationalism_1 == "Neither agree nor disagree" ~ 0,
    .$Internationalism_1 == "Somewhat disagree" ~ -1,
    .$Internationalism_1 == "Strongly disagree" ~ -1,
  )) %>%
  mutate(Internationalism_2_reverse = case_when(
    .$Internationalism_2_reverse == "Strongly agree" ~ -1,
    .$Internationalism_2_reverse == "Somewhat agree" ~ -1,
    .$Internationalism_2_reverse == "Neither agree nor disagree" ~ 0,
    .$Internationalism_2_reverse == "Somewhat disagree" ~ 1,
    .$Internationalism_2_reverse == "Strongly disagree" ~ 1,
  ))
  
# Convert Political Knowledge To Numeric values
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

# Code Other Dummy Variables
clean_data <- clean_data %>%
  mutate(Gender = case_when(   # Variable For Male
    .$Gender == "Male" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(Ethnicity = case_when(   # Variable for Caucasian
    .$Ethnicity == "White / Caucasian" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(Education = case_when(   # Variable for College
    .$Education == "Bachelor’s degree" ~ 1,
    .$Education == "Master’s degree or above" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(Employment = case_when(   # Variable for employed
    .$Employment == "Full-time" ~ 1,
    .$Employment == "Part-time" ~ 1,
    .$Employment == "Contract/ Temporary" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(Income = case_when(   # Variable for above median income
    .$Income == "$70,000 - $100,000" ~ 1,
    .$Income == "$50,000 - $100,000" ~ 1, # Only exists in test data
    .$Income == "$100,000 - $200,000" ~ 1,
    .$Income == "More than $200,000" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(PoliticalInterest = case_when(   # Variable for interested in politics
    .$PoliticalInterest == "Very interested" ~ 1,
    .$PoliticalInterest == "Somewhat interested" ~ 1,
    TRUE ~ 0,
  )) %>%
  mutate(Age = case_when(   # Variable for interested in politics
    .$Age == "Prefer not to say" ~ "25-34 years old",
    TRUE ~ .$Age,
  ))


# Create Indexes ---------------------------------------------------------------------------------
indexed_data <- clean_data

# Create an index for political knowledge normalizing scores
indexed_data <- indexed_data %>%
  mutate(Knowledge_Score =
           KnowledgeTest_Conservative +
           KnowledgeTest_NATO +
           KnowledgeTest_UK +
           KnowledgeTest_Zelensky) %>%
  mutate(Knowledge_Index = (Knowledge_Score - min(.$Knowledge_Score))/(max(.$Knowledge_Score) - min(.$Knowledge_Score)))

# Create an index for political ideology normalizing scores
indexed_data <- indexed_data %>%
  mutate(Ideology_LR = replace_na(.$Ideology_LR, 5))%>%
  mutate(Ideology_Index = (Ideology_LR - min(.$Ideology_LR))/(max(.$Ideology_LR) - min(.$Ideology_LR)))

# Create an index for militarism and internationalism.
# The more positive, the more militaristic/internationalist
indexed_data <- indexed_data %>%
  mutate(Militarism_Index = Militarism_1 + Militarism_2_reverse) %>%
  mutate(Internationalism_Index = Internationalism_1 + Internationalism_2_reverse)


# Create mean effects indexes for outcomes
# Create an index for non-military outcomes
indexed_data <- indexed_data %>%
  mutate(Control_Nonmilitary_Index = Control_Economic + Control_Political) %>%
  mutate(T1_Nonmilitary_Index = T1_Economic + T1_Political) %>%
  mutate(T2_Nonmilitary_Index = T2_Economic + T2_Political)

# Create an index for overall outcome
indexed_data <- indexed_data %>%
  mutate(Control_Overall_Index = Control_Direct + Control_Indirect + Control_Economic + Control_Political) %>% 
  mutate(T1_Overall_Index = T1_Direct + T1_Indirect + T1_Economic + T1_Political) %>%
  mutate(T2_Overall_Index = T2_Direct + T2_Indirect + T2_Economic + T2_Political)

# Create an index for time spent
indexed_data <- indexed_data %>%
  mutate(Control_Time_Spent_Index = Control_Page_Submit) %>%
  mutate(T1_Time_Spent_Index = T1_Page_Submit) %>%
  mutate(T2_Time_Spent_Index = T2_Page_Submit)

# Create full group (consolidate columns)
full_group <- indexed_data %>%
  mutate(Treatment = ifelse(is.na(T1_Direct) & is.na(T2_Direct),"T","C")) %>%
  mutate(Z = ifelse(is.na(T1_Direct) & is.na(T2_Direct),1,0)) %>%
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

# Convert to Means Effect Index

# Adapted From Alexander Coppock "10 Things to Know About Multiple Comparisons"
# https://egap.org/resource/10-things-to-know-about-multiple-comparisons/
calculate_mean_effects_index <- function(Z, outcome_mat){
  if(length(Z) != nrow(outcome_mat)) stop("Error: Treatment assignment, outcome matrix require same n!")
  
  c_mean <- apply(X = outcome_mat[Z==0,], MARGIN = 2, FUN = mean, na.rm = T)
  c_sd <- apply(X = outcome_mat[Z==0,], MARGIN = 2, FUN = sd, na.rm = T)
  z_score <- t(t(sweep(outcome_mat, 2, c_mean))/ c_sd)
  index_numerator <- rowSums(z_score)
  n_outcomes <- ncol(outcome_mat)
  index <- index_numerator/n_outcomes
  index <-  (index - mean(index[Z==0], na.rm =T))/sd(index[Z==0], na.rm =T)
  return(index)
}

full_group$Direct = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("Direct")))
full_group$Indirect = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("Indirect")))
full_group$Economic = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("Economic")))
full_group$Political = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("Political"))) 
full_group$General = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("General"))) 
full_group$Threat = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("Threat"))) 
full_group$Nonmilitary_Index = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("Nonmilitary_Index"))) 
full_group$Overall_Index = calculate_mean_effects_index(full_group$Z,subset(full_group, select = c("Overall_Index"))) 

# Export To CSV
write.csv(full_group,"indexed_data.csv", row.names = FALSE)

