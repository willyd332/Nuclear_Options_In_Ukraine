# Libraries
library(dplyr)
library(tidyverse)

# General Variables
sample_size <- 1000

# Set Treatment Biases
# ("DIRECT", "INDIRECT", "ECONOMIC", "POLITICAL", "GENERAL")
treatment_bias <- c(-1, 1, 1, 1, -1)

# Set Control Biases
pro_russia_bias <- c(-1, -1, -1, -1, -1)
anti_russia_bias <- c(1, 1, 1, 1, 1)
pro_warfare_bias <- c(1, 1, 0, 0, 0)
anti_warfare_bias <- c(-1, -1, 0, 0, 0)

# Generate Data
treatment <- sample(c(0, 1), sample_size, replace = TRUE)

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
  transform(direct = direct - Z) %>%
  transform(indirect = indirect + Z) %>%
  transform(economic = economic + Z) %>%
  transform(political = political + Z) %>%
  transform(general = general - Z) %>%
  mutate(across(direct:general, ~ case_when(
    . > 4 ~ 4,
    . < 0 ~ 0,
    TRUE ~ .
  )))

apply(treated_df, 2, sd, na.rm = TRUE)
summary(treated_df)

# Export To CSV
write.csv(treated_df,"./Dummy_Data_1.csv", row.names = FALSE)
