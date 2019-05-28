########################################################################################
# 
# State Diversity Analysis - Analyze the data for diversity
#
########################################################################################

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

###############
# Load data
###############
load("state_diversity/data/State_Demographic_Percents.Rdata")


###############
# Calculate Simpson's Index/HHI
###############
simpson_index <- function(x) {
  1 - sum(x^2)
}

diversity_simp <- state_perc %>% 
  group_by(State) %>% 
  summarise(Simp_Ind = simpson_index(Percent))

###############
# Calculate Shannon's entropy
###############
shannon_entropy <- function(x) {
  -1 * sum(x * log(x))
}

diversity_shann <- state_perc %>% 
  group_by(State) %>% 
  summarise(Shann_Ent = shannon_entropy(Percent))

###############
# Merge and compare
###############
# Merge and calcualte normalized metrics
diversity_data <- merge(diversity_simp, diversity_shann) %>% 
  mutate(Rel_Simp = Simp_Ind / max(Simp_Ind),
         Rel_Shann = Shann_Ent / max(Shann_Ent)) %>% 
  arrange(desc(Rel_Simp)) %>% 
  mutate(Simp_rank = 1:length(State)) %>% 
  arrange(desc(Rel_Shann)) %>% 
  mutate(Shann_rank = 1:length(State))

# Compare
gg_div <- ggplot(data = diversity_data, aes(x = Simp_Ind, Shann_Ent)) +
  geom_point(color = "blue") +
  theme_classic()
gg_div




