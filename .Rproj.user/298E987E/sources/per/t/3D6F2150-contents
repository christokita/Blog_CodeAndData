########################################################################################
# 
# State Diversity Analysis - Plotting data 
#
########################################################################################

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
source("state_diversity/scripts/__Util_GraphingFunctions.R")

###############
# Load data
###############
load("state_diversity/data/State_Demographic_Percents.Rdata")


###############
# Plot Hispanic population by state
###############
hisp <- state_perc %>% 
  filter(Group == "Mixed_Race") %>% 
  filter(!State %in% c("District of Columbia", "Puerto Rico", "United States")) %>% 
  arrange(desc(Percent)) %>%
  mutate(Plot_order = as.factor(1:length(State)))

gg_hisp <- ggplot(data = hisp, aes(x = Plot_order, y = Percent)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic() +
  scale_x_discrete(label = hisp$State) +
  scale_y_continuous(expand = c(0.02, 0)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))
gg_hisp
