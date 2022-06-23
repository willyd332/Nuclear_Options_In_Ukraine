# Libraries
library(dplyr)
library(tidyverse)

# KEY ASSUMPTIONS
ALPHA = 0.05
TREATMENT_EFFECT_SIZE = 0.26
TARGET_POWER = 0.8
AVERAGE_OUTCOME = 1.84548
STANDARD_DEVIATION = 1.112738

# General Variables
sample_size <- 1000

# Set Treatment Biases
# ("DIRECT", "INDIRECT", "ECONOMIC", "POLITICAL", "GENERAL")
te <- TREATMENT_EFFECT_SIZE
treatment_bias <- c(-te, te, te, te, -te)

# Set Control Biases
pro_russia_bias <- c(-1, -1, -1, -1, -1)
anti_russia_bias <- c(1, 1, 1, 1, 1)
pro_warfare_bias <- c(1, 1, 0, 0, 0)
anti_warfare_bias <- c(-1, -1, 0, 0, 0)

# Generate Data
treatment <- sample(c(0, 1, 2), sample_size, replace = TRUE)

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
  Z = treatment,
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
      Z == 0 ~ direct,   
      Z == 1 ~ direct - TREATMENT_EFFECT_SIZE,
      Z == 2 ~ direct + TREATMENT_EFFECT_SIZE
    )
  ) %>%
  transform(indirect = case_when(
      Z == 0 ~ indirect,
      Z == 1 ~ indirect + TREATMENT_EFFECT_SIZE,
      Z == 2 ~ indirect - TREATMENT_EFFECT_SIZE
    )
  ) %>%
  transform(economic = case_when(
      Z == 0 ~ economic,
      Z == 1 ~ economic + TREATMENT_EFFECT_SIZE,
      Z == 2 ~ economic - TREATMENT_EFFECT_SIZE
    )
  ) %>%
  transform(political = case_when(
      Z == 0 ~ political,
      Z == 1 ~ political + TREATMENT_EFFECT_SIZE,
      Z == 2 ~ political - TREATMENT_EFFECT_SIZE
    )
  ) %>%
  transform(general = case_when(
      Z == 0 ~ general,
      Z == 1 ~ general - TREATMENT_EFFECT_SIZE,
      Z == 2 ~ general + TREATMENT_EFFECT_SIZE
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
