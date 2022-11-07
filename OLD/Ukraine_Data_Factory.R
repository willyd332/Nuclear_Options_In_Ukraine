# Libraries
library(dplyr)
library(tidyverse)

# KEY ASSUMPTIONS
ALPHA = 0.05
TREATMENT_1_EFFECT_SIZE = 0.172 # Kertzer & Zeitoff (2017) *See Power-Calculations.R
TREATMENT_2_EFFECT_SIZE = 0.376 # Kertzer & Zeitoff (2017) *See Power-Calculations.R
TARGET_POWER = 0.8
AVERAGE_OUTCOME = 1.84548 # Kertzer & Zeitoff (2017) *See Power-Calculations.R
STANDARD_DEVIATION = 1.112738 # Kertzer & Zeitoff (2017) *See Power-Calculations.R

# General Variables
sample_size <- 1000

# Set Treatment Biases
# ("DIRECT", "INDIRECT", "ECONOMIC", "POLITICAL", "GENERAL")
t1e <- TREATMENT_1_EFFECT_SIZE
t2e <- TREATMENT_2_EFFECT_SIZE
t1_bias <- c(-t1e, t1e, t1e, t1e, -t1e)
t2_bias <- c(-t2e, t2e, t2e, t2e, -t2e)

# Set Control Biases
pro_russia_bias <- c(-1, -1, -1, -1, -1)
anti_russia_bias <- c(1, 1, 1, 1, 1)
pro_warfare_bias <- c(1, 1, 0, 0, 0)
anti_warfare_bias <- c(-1, -1, 0, 0, 0)

# Generate Data
treatment <- sample(c("T0", "T1", "T2"), sample_size, replace = TRUE)
Z <- modify(treatment, function(x){if(x!="T0"){return(1)}else{return(0)}})
# 1 is anti, 2 is neutral, 3 is pro
russian_control <- sample(c(1, 2, 3), sample_size, replace = TRUE)
warfare_control <- sample(c(1, 2, 3), sample_size, replace = TRUE)

# Create unbiased data
direct_unbiased <- sample(c(0, 1, 2, 3, 4), sample_size, replace = TRUE)
indirect_unbiased <- sample(c(0, 1, 2, 3, 4), sample_size, replace = TRUE)
economic_unbiased <- sample(c(0, 1, 2, 3, 4), sample_size, replace = TRUE)
political_unbiased <- sample(c(0, 1, 2, 3, 4), sample_size, replace = TRUE)
general_unbiased <- sample(c(0, 1, 2, 3, 4), sample_size, replace = TRUE)

# Create the df
ubiased_df <- data.frame(
  id = 1:sample_size,
  treatment = treatment,
  Z = Z,
  russia = russian_control,
  warfare = warfare_control,
  direct = direct_unbiased,
  indirect = indirect_unbiased,
  economic = economic_unbiased,
  political = political_unbiased,
  general = general_unbiased
)

# Perform Transformations
biased_df <- ubiased_df %>%
  mutate(
    across(direct:general, ~ case_when(
      russia == 1 ~ . + 1, # when Russia equals 1, change existing value by +1
      russia == 3 ~ . - 1,
      TRUE ~ .
    ))
  ) %>%
  mutate(
    across(direct:indirect, ~ case_when(
      warfare == 1 ~ . - 1,
      warfare == 3 ~ . + 1,
      TRUE ~ .
    ))
  )

biased_df

# Create the treated DF
treated_df <- biased_df %>%
  transform(direct = case_when(
      treatment == "T0" ~ direct,   
      treatment == "T1" ~ direct - t1e,
      treatment == "T2" ~ direct - t2e
    )
  ) %>%
  transform(indirect = case_when(
      treatment == "T0" ~ indirect,
      treatment == "T1" ~ indirect + t1e,
      treatment == "T2" ~ indirect + t2e
    )
  ) %>%
  transform(economic = case_when(
      treatment == "T0" ~ economic,
      treatment == "T1" ~ economic + t1e,
      treatment == "T2" ~ economic + t2e
    )
  ) %>%
  transform(political = case_when(
      treatment == "T0" ~ political,
      treatment == "T1" ~ political + t1e,
      treatment == "T2" ~ political + t2e
    )
  ) %>%
  transform(general = case_when(
      treatment == "T0" ~ general,
      treatment == "T1" ~ general - t1e,
      treatment == "T2" ~ general - t2e
    )
  ) %>%
  mutate(across(direct:general, ~ case_when(
    . > 4 ~ 4,
    . < 0 ~ 0,
    TRUE ~ .
  )))

apply(treated_df, 2, sd, na.rm = TRUE)
summary(treated_df)
treated_df
# Export To CSV
write.csv(treated_df,"./Dummy_Data_Multi_Arm.csv", row.names = FALSE)
