# # Installs
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("broom")
# install.packages("ggpubr")
# install.packages("estimatr")

# Imports
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(estimatr)

# Get the data
data <- read.csv('./Dummy_Data_1.csv')
data
control_group <- data[data$Z == 0,]
treatment_group <- data[data$Z == 1,]

print("Control Group Summary")
summary(control_group)
print("Control Group Summary")
summary(treatment_group)


# Perform Basic Regression
direct_model <- 
  lm_robust(
   direct ~ Z,
    data=data
    )

indirect_model <- 
  lm_robust(
    indirect ~ Z,
    data=data
    )

economic_model <- 
  lm_robust(
    economic ~ Z,
    data=data
    )

political_model <- 
  lm_robust(
    political ~ Z,
    data=data
    )

general_model <- 
  lm_robust(
    general ~ Z,
    data=data
    )


# ...With Warfare Control 
direct_w_model <- 
  lm_robust(
    direct ~ Z 
    + warfare,
    data=data
    )

indirect_w_model <- 
  lm_robust(
    indirect ~ Z 
    + warfare,
    data=data
    )

economic_w_model <- 
  lm_robust(
    economic ~ Z 
    + warfare,
    data=data
    )

political_w_model <- 
  lm_robust(
    political ~ Z 
    + warfare,
    data=data
    )

general_w_model <- 
  lm_robust(
    general ~ Z 
    + warfare,
    data=data
    )

# ...With Russia Control 
direct_wr_model <- 
  lm_robust(
    direct ~ Z 
    + warfare 
    + russia,
    data=data
    )

indirect_wr_model <- 
  lm_robust(
    indirect ~ Z 
    + warfare 
    + russia,
    data=data
    )

economic_wr_model <- 
  lm_robust(
    economic ~ Z 
    + warfare 
    + russia,
    data=data
    )

political_wr_model <- 
  lm_robust(
    political ~ Z 
    + warfare 
    + russia,
    data=data
    )

general_wr_model <- 
  lm_robust(
    general ~ Z 
    + warfare 
    + russia,
    data=data
    )


print(summary(direct_wr_model), digits = 3)
print(summary(indirect_wr_model), digits = 3)
print(summary(economic_wr_model), digits = 3)
print(summary(political_wr_model), digits = 3)
print(summary(general_wr_model), digits = 3)

