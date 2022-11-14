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
data <- read.csv('./indexed_data.csv')
prolific_data <- read.csv('./Prolific_Data.csv')

full_group <- as_tibble(data)



colnames(full_group)
