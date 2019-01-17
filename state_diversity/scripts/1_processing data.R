########################################################################################
# 
# State Diversity Analysis
#
########################################################################################

rm(list = ls())
library(dplyr)
library(tidyr)

###############
# Load data
###############
state_data <- read.csv("state_diversity/data/State_Demographic_Data.csv", as.is = T)

###############
# Manipulate data
###############
state_perc <- state_data %>% 
  gather(key = Group, value = Percent, -State, -Total_Pop) %>% 
  mutate(Percent = Percent / Total_Pop)
