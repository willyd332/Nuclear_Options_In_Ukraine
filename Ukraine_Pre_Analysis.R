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
control_group <- data[data$treatment == 0,]
treatment_group <- data[data$treatment == 1,]

print("Control Group Summary")
summary(control_group)
print("Control Group Summary")
summary(treatment_group)


# Perform Basic Regression
direct_model <- 
  lm_robust(
    treatment ~ direct,
    data=data
    )

indirect_model <- 
  lm_robust(
    treatment ~ indirect,
    data=data
    )

economic_model <- 
  lm_robust(
    treatment ~ economic,
    data=data
    )

political_model <- 
  lm_robust(
    treatment ~ political,
    data=data
    )

general_model <- 
  lm_robust(
    treatment ~ general,
    data=data
    )


# ...With Warfare Control 
direct_w_model <- 
  lm_robust(
    treatment ~ direct + warfare,
    data=data
    )

indirect_w_model <- 
  lm_robust(
    treatment ~ indirect + warfare,
    data=data
    )

economic_w_model <- 
  lm_robust(
    treatment ~ economic + warfare,
    data=data
    )

political_w_model <- 
  lm_robust(
    treatment ~ political + warfare,
    data=data
    )

general_w_model <- 
  lm_robust(
    treatment ~ general + warfare,
    data=data
    )

# ...With Russia Control 
direct_wr_model <- 
  lm_robust(
    treatment ~ direct + warfare + russia,
    data=data
    )

indirect_wr_model <- 
  lm_robust(
    treatment ~ indirect + warfare + russia,
    data=data
    )

economic_wr_model <- 
  lm_robust(
    treatment ~ economic + warfare + russia,
    data=data
    )

political_wr_model <- 
  lm_robust(
    treatment ~ political + warfare + russia,
    data=data
    )

general_wr_model <- 
  lm_robust(
    treatment ~ general + warfare + russia,
    data=data
    )


print(summary(direct_wr_model), digits=3)
print(summary(indirect_wr_model), digits=3)
print(summary(economic_wr_model), digits=3)
print(summary(political_wr_model), digits=3)
print(summary(general_wr_model), digits=3)

