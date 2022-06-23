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

# Get the data
data <- read.csv('./Dummy_Data_1.csv')

control_group <- data[data$Z == 0,]
treatment_group <- data[data$Z == 1,]

print("Control Group Summary")
summary(control_group)
print("Control Group Summary")
summary(treatment_group)

# (Model 1) Regression without controls...
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

# ...With Controls (Model 2)
direct_model_2 <-
  lm_robust(
    direct ~ Z 
    + warfare 
    + russia,
    data=data
    )

indirect_model_2 <- 
  lm_robust(
    indirect ~ Z 
    + warfare 
    + russia,
    data=data
    )

economic_model_2 <- 
  lm_robust(
    economic ~ Z 
    + warfare 
    + russia,
    data=data
    )

political_model_2 <- 
  lm_robust(
    political ~ Z 
    + warfare 
    + russia,
    data=data
    )

general_model_2 <- 
  lm_robust(
    general ~ Z 
    + warfare 
    + russia,
    data=data
    )

# print(summary(direct_model), digits = 3)
# print(summary(indirect_model), digits = 3)
# print(summary(economic_model), digits = 3)
# print(summary(political_model), digits = 3)
# print(summary(general_model), digits = 3)
# 
# 
# print(summary(direct_model_2), digits = 3)
# print(summary(indirect_model_2), digits = 3)
# print(summary(economic_model_2), digits = 3)
# print(summary(political_model_2), digits = 3)
# print(summary(general_model_2), digits = 3)

# Generate Regression Output...
# Check coefficient names
texreg(list(direct_model_1, direct_model_2),
       include.ci = FALSE,
       digits = 3)

# Generate Custom Outputs
texreg(
  list(direct_model_1, direct_model_2),
  custom.coef.names = c(
    "Intercept",
    "Treatment",
    "+ Opinions On Warfare",
    "+ Opinions On Russia"
  ),
  digits = 4,
  stars = c(0.001, 0.01, 0.05),
  custom.note = "%stars. Robust standard errors are in parentheses.",
  reorder.coef = c(2, 3, 4, 1),
  include.ci = FALSE,
  caption = "DIRECT WARFARE",
  caption.above = TRUE,
  # include.adjrs = FALSE,
  # include.rmse = FALSE,
  dcolumn = TRUE,
  booktabs = TRUE,
  float.pos = "t",
  use.packages = FALSE,
  # table = FALSE,
  file = "./regression_outputs/direct_warfare.tex"
)

texreg(
  list(indirect_model_1, indirect_model_2),
  custom.coef.names = c(
    "Intercept",
    "Treatment",
    "+ Opinions On Warfare",
    "+ Opinions On Russia"
  ),
  caption = "INDIRECT AID",
  caption.above = TRUE,
  digits = 4,
  stars = c(0.001, 0.01, 0.05),
  custom.note = "%stars. Robust standard errors are in parentheses.",
  reorder.coef = c(2, 3, 4, 1),
  include.ci = FALSE,
  # include.adjrs = FALSE,
  # include.rmse = FALSE,
  dcolumn = TRUE,
  booktabs = TRUE,
  float.pos = "t",
  use.packages = FALSE,
  # table = FALSE,
  file = "./regression_outputs/indirect_aid.tex"
)

texreg(
  list(economic_model_1, economic_model_2),
  custom.coef.names = c(
    "Intercept",
    "Treatment",
    "+ Opinions On Warfare",
    "+ Opinions On Russia"
  ),
  caption = "ECONOMIC SANCTIONS",
  caption.above = TRUE,
  digits = 4,
  stars = c(0.001, 0.01, 0.05),
  custom.note = "%stars. Robust standard errors are in parentheses.",
  reorder.coef = c(2, 3, 4, 1),
  include.ci = FALSE,
  # include.adjrs = FALSE,
  # include.rmse = FALSE,
  dcolumn = TRUE,
  booktabs = TRUE,
  float.pos = "t",
  use.packages = FALSE,
  # table = FALSE,
  file = "./regression_outputs/economic_sanctions.tex"
)

texreg(
  list(political_model_1, political_model_2),
  custom.coef.names = c(
    "Intercept",
    "Treatment",
    "+ Opinions On Warfare",
    "+ Opinions On Russia"
  ),
  caption = "POLITICAL CONDEMNATION",
  caption.above = TRUE,
  digits = 4,
  stars = c(0.001, 0.01, 0.05),
  custom.note = "%stars. Robust standard errors are in parentheses.",
  reorder.coef = c(2, 3, 4, 1),
  include.ci = FALSE,
  # include.adjrs = FALSE,
  # include.rmse = FALSE,
  dcolumn = TRUE,
  booktabs = TRUE,
  float.pos = "t",
  use.packages = FALSE,
  # table = FALSE,
  file = "./regression_outputs/political_condemnation.tex"
)

texreg(
  list(general_model_1, general_model_2),
  custom.coef.names = c(
    "Intercept",
    "Treatment",
    "+ Opinions On Warfare",
    "+ Opinions On Russia"
  ),
  caption = "GENERAL DEGREE OF RESPONSE",
  caption.above = TRUE,
  digits = 4,
  stars = c(0.001, 0.01, 0.05),
  custom.note = "%stars. Robust standard errors are in parentheses.",
  reorder.coef = c(2, 3, 4, 1),
  include.ci = FALSE,
  # include.adjrs = FALSE,
  # include.rmse = FALSE,
  dcolumn = TRUE,
  booktabs = TRUE,
  float.pos = "t",
  use.packages = FALSE,
  # table = FALSE,
  file = "./regression_outputs/general_response.tex"
)



# Create the overall table

texreg(  list(
  direct_model_1, direct_model_2,
  indirect_model_1, indirect_model_2,
  economic_model_1, economic_model_2,
  political_model_1, political_model_2,
  general_model_1, general_model_2
),
       include.ci = FALSE,
       digits = 3)

texreg(
  list(
    direct_model_1, direct_model_2,
    indirect_model_1, indirect_model_2,
    economic_model_1, economic_model_2,
    political_model_1, political_model_2,
    general_model_1, general_model_2
  ),
  custom.coef.names = c(
    "Intercept",
    "Treatment",
    "+ Opinions On Warfare",
    "+ Opinions On Russia"
  ),
  caption = "SUPPORT FOR VARIOUS RESPONSES TO RUSSIAN AGGRESSION IN UKRAINE",
  caption.above = TRUE,
  digits = 4,
  stars = c(0.001, 0.01, 0.05),
  custom.note = "%stars. Robust standard errors are in parentheses.",
  custom.model.names = c(
    "DIRECT WARFARE",
    "2",
    "INDIRECT AID",
    "2",
    "ECONOMIC SANCTIONS",
    "2",
    "POLITICAL CONDEMNATION",
    "2",
    "GENERAL DEGREE OF RESPONSE",
    "2"
    ),
  reorder.coef = c(2, 3, 4, 1),
  include.ci = FALSE,
  # include.adjrs = FALSE,
  # include.rmse = FALSE,
  dcolumn = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,
  sideways = TRUE,
  scalebox = 0.55,
  float.pos = "b!",
  # table = FALSE,
  file = "./regression_outputs/overall_table.tex"
)


