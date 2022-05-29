# Imports
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

# Get the data
data = read.csv('./Dummy_Data_1.csv')
control_group = data[data$treatment == 0,]
treatment_group = data[data$treatment == 1,]

print("Control Group Summary")
summary(control_group)
print("Control Group Summary")
summary(treatment_group)


# Perform Regression
direct_model = lm(treatment ~ direct, data=data)
indirect_model = lm(treatment ~ indirect, data=data)
economic_model = lm(treatment ~ economic, data=data)
political_model = lm(treatment ~ political, data=data)
general_model = lm(treatment ~ general, data=data)


# ...With Warfare Control 
direct_w_model = lm(treatment ~ direct + warfare, data=data)
indirect_w_model = lm(treatment ~ indirect + warfare, data=data)
economic_w_model = lm(treatment ~ economic + warfare, data=data)
political_w_model = lm(treatment ~ political + warfare, data=data)
general_w_model = lm(treatment ~ general + warfare, data=data)

# ...With Russia Control 
direct_wr_model = lm(treatment ~ direct + warfare + russia, data=data)
indirect_wr_model = lm(treatment ~ indirect + warfare + russia, data=data)
economic_wr_model = lm(treatment ~ economic + warfare + russia, data=data)
political_wr_model = lm(treatment ~ political + warfare + russia, data=data)
general_wr_model = lm(treatment ~ general + warfare + russia, data=data)

# Calculate Significance




