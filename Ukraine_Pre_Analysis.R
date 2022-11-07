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
clean_data$Control_General <- as.numeric(clean_data$Control_General) %>%
  replace_na(-1)
clean_data$Control_Threat <- as.numeric(clean_data$Control_Threat) %>%
  replace_na(-1)
clean_data$T1_General <- as.numeric(clean_data$T1_General) %>%
  replace_na(-1)
clean_data$T1_Threat <- as.numeric(clean_data$T1_Threat) %>%
  replace_na(-1)
clean_data$T2_General <- as.numeric(clean_data$T2_General) %>%
  replace_na(-1)
clean_data$T2_Threat <- as.numeric(clean_data$T2_Threat) %>%
  replace_na(-1)

clean_data$Ideology_LR <- as.numeric(clean_data$Ideology_LR) %>%
  replace_na(-1)

  # Convert Main Outcomes
clean_data <- clean_data %>%
  mutate(Control_Direct = case_when(
    .$Control_Direct == "Send US troops to Ukraine" ~ 1,
    .$Control_Direct == "Do not send US troops to Ukraine" ~ 0,
    .$Control_Direct == "" ~ -1
  )) %>%
  mutate(Control_Indirect = case_when(
    .$Control_Indirect == "Send military resources to Ukrainian troops" ~ 1,
    .$Control_Indirect == "Do not send military resources to Ukrainian troops" ~ 0,
    .$Control_Indirect == "" ~ -1
  )) %>%
  mutate(Control_Economic = case_when(
    .$Control_Economic == "Enact economic sanctions on Russia" ~ 1,
    .$Control_Economic == "Do not enact economic sanctions on Russia" ~ 0,
    .$Control_Economic == "" ~ -1
  )) %>%
  mutate(Control_Political = case_when(
    .$Control_Political == "Condemn Russia's actions in an international court" ~ 1,
    .$Control_Political == "Do not condemn Russia's actions in an international court" ~ 0,
    .$Control_Political == "" ~ -1
  )) %>%
  mutate(T1_Direct = case_when(
    .$T1_Direct == "Send US troops to Ukraine" ~ 1,
    .$T1_Direct == "Do not send US troops to Ukraine" ~ 0,
    .$T1_Direct == "" ~ -1
  )) %>%
  mutate(T1_Indirect = case_when(
    .$T1_Indirect == "Send military resources to Ukrainian troops" ~ 1,
    .$T1_Indirect == "Do not send military resources to Ukrainian troops" ~ 0,
    .$T1_Indirect == "" ~ -1
  )) %>%
  mutate(T1_Economic = case_when(
    .$T1_Economic == "Enact economic sanctions on Russia" ~ 1,
    .$T1_Economic == "Do not enact economic sanctions on Russia" ~ 0,
    .$T1_Economic == "" ~ -1
  )) %>%
  mutate(T1_Political = case_when(
    .$T1_Political == "Condemn Russia's actions in an international court" ~ 1,
    .$T1_Political == "Do not condemn Russia's actions in an international court" ~ 0,
    .$T1_Political == "" ~ -1
  )) %>%
  mutate(T2_Direct = case_when(
    .$T2_Direct == "Send US troops to Ukraine" ~ 1,
    .$T2_Direct == "Do not send US troops to Ukraine" ~ 0,
    .$T2_Direct == "" ~ -1
  )) %>%
  mutate(T2_Indirect = case_when(
    .$T2_Indirect == "Send military resources to Ukrainian troops" ~ 1,
    .$T2_Indirect == "Do not send military resources to Ukrainian troops" ~ 0,
    .$T2_Indirect == "" ~ -1
  )) %>%
  mutate(T2_Economic = case_when(
    .$T2_Economic == "Enact economic sanctions on Russia" ~ 1,
    .$T2_Economic == "Do not enact economic sanctions on Russia" ~ 0,
    .$T2_Economic == "" ~ -1
  )) %>%
  mutate(T2_Political = case_when(
    .$T2_Political == "Condemn Russia's actions in an international court" ~ 1,
    .$T2_Political == "Do not condemn Russia's actions in an international court" ~ 0,
    .$T2_Political == "" ~ -1
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
  

# Convert Political Knowlege To Numeric
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

colnames(clean_data)

# Create New Columns/Indexes
  # Non-Military Index (For each group)
  # Overall Index (For each group)
  # Time Spent (For each group)
  # Militarism Index (Average)
  # Internationalism Index (Average)
  # Political Knowledge Index (Total)


## Old Analysis

control_group <- data[data$treatment == "T0",]
treated_group <- data[data$Z == 1,]
treatment1_group <- data[data$treatment == "T1",]
treatment2_group <- data[data$treatment == "T2",]

print("Control Group Summary")
summary(control_group)
print("Treated Group Summary")
summary(treated_group)
print("Treatment 1 Group Summary")
summary(treatment1_group)
print("Treatment 2 Group Summary")
summary(treatment2_group)

# Model 1 -- General Treatment
direct_model_1 <- 
  lm_robust(
   direct ~ Z,
    data=data
    )

indirect_model_1 <- 
  lm_robust(
    indirect ~ Z,
    data=data
    )

economic_model_1 <- 
  lm_robust(
    economic ~ Z,
    data=data
    )

political_model_1 <- 
  lm_robust(
    political ~ Z,
    data=data
    )

general_model_1 <- 
  lm_robust(
    general ~ Z,
    data=data
    )

# ...Model 1 With Controls
direct_model_1_c <-
  lm_robust(
    direct ~ Z 
    + warfare 
    + russia,
    data=data
    )

indirect_model_1_c <- 
  lm_robust(
    indirect ~ Z 
    + warfare 
    + russia,
    data=data
    )

economic_model_1_c <- 
  lm_robust(
    economic ~ Z 
    + warfare 
    + russia,
    data=data
    )

political_model_1_c <- 
  lm_robust(
    political ~ Z 
    + warfare 
    + russia,
    data=data
    )

general_model_1_c <- 
  lm_robust(
    general ~ Z 
    + warfare 
    + russia,
    data=data
    )

# Model 2 -- Treatment 1 Only
direct_model_2 <- 
  lm_robust(
    direct ~ treatment,
    data=subset(data, treatment!="T2")
  )

indirect_model_2 <- 
  lm_robust(
    indirect ~ treatment,
    data=subset(data, treatment!="T2")
  )

economic_model_2 <- 
  lm_robust(
    economic ~ treatment,
    data=subset(data, treatment!="T2")
  )

political_model_2 <- 
  lm_robust(
    political ~ treatment,
    data=subset(data, treatment!="T2")
  )

general_model_2 <- 
  lm_robust(
    general ~ treatment,
    data=subset(data, treatment!="T2")
  )

# Model 2 With Controls
direct_model_2_c <- 
  lm_robust(
    direct ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T2")
  )

indirect_model_2_c <- 
  lm_robust(
    indirect ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T2")
  )

economic_model_2_c <- 
  lm_robust(
    economic ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T2")
  )

political_model_2_c <- 
  lm_robust(
    political ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T2")
  )

general_model_2_c <- 
  lm_robust(
    general ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T2")
  )

# Model 3 -- Treatment 2 Only
direct_model_3 <- 
  lm_robust(
    direct ~ treatment,
    data=subset(data, treatment!="T1")
  )

indirect_model_3 <- 
  lm_robust(
    indirect ~ treatment,
    data=subset(data, treatment!="T1")
  )

economic_model_3 <- 
  lm_robust(
    economic ~ treatment,
    data=subset(data, treatment!="T1")
  )

political_model_3 <- 
  lm_robust(
    political ~ treatment,
    data=subset(data, treatment!="T1")
  )

general_model_3 <- 
  lm_robust(
    general ~ treatment,
    data=subset(data, treatment!="T1")
  )

# Model 3 With Controls
direct_model_3_c <- 
  lm_robust(
    direct ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T1")
  )

indirect_model_3_c <- 
  lm_robust(
    indirect ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T1")
  )

economic_model_3_c <- 
  lm_robust(
    economic ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T1")
  )

political_model_3_c <- 
  lm_robust(
    political ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T1")
  )

general_model_3_c <- 
  lm_robust(
    general ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T1")
  )

# Model 4 -- Treatment 1 vs Treatment 2
direct_model_4 <- 
  lm_robust(
    direct ~ treatment,
    data=subset(data, treatment!="T0")
  )

indirect_model_4 <- 
  lm_robust(
    indirect ~ treatment,
    data=subset(data, treatment!="T0")
  )

economic_model_4 <- 
  lm_robust(
    economic ~ treatment,
    data=subset(data, treatment!="T0")
  )

political_model_4 <- 
  lm_robust(
    political ~ treatment,
    data=subset(data, treatment!="T0")
  )

general_model_4 <- 
  lm_robust(
    general ~ treatment,
    data=subset(data, treatment!="T0")
  )

# Model 3 With Controls
direct_model_4_c <- 
  lm_robust(
    direct ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T0")
  )

indirect_model_4_c <- 
  lm_robust(
    indirect ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T0")
  )

economic_model_4_c <- 
  lm_robust(
    economic ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T0")
  )

political_model_4_c <- 
  lm_robust(
    political ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T0")
  )

general_model_4_c <- 
  lm_robust(
    general ~ treatment
    + warfare 
    + russia,
    data=subset(data, treatment!="T0")
  )

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


