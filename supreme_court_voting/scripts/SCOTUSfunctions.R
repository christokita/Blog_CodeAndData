########################################################################################
# 
# SCOTUS Utility Functions
#
########################################################################################

# Load in packages
library(igraph)
library(dplyr)
library(reshape)
library(ggplot2)


######################### Split Decision Percentage #########################
SplitPercent <- function(ScotusDf, output = FALSE) {
  split_decisions <-
    ScotusDf %>%
    filter(!is.na(direction)) %>%
    mutate(case_vote = paste(caseId, direction, sep = ";"))
  split_decisions <- unique(split_decisions$case_vote) #get frequency count of vote-case combos
  split_decisions <- gsub(pattern = ";.*", replacement = "", x = split_decisions) #get only case id
  split_decisions <- table(split_decisions)
  split_decisions <- split_decisions[split_decisions > 1] #indicates split vote
  
  splitPercent <- length(split_decisions) / length(unique(scotus$caseId))
  print(splitPercent)
  
  if (output == TRUE) {
    # Create dataframe
    tempDf <- data.frame(Type = c("Split", "Unanimous"), Frequency = c(splitPercent, 1 - splitPercent))
    
    return(tempDf)
    
  }
}

######################### Grab Split Decision Rows #########################
GetSplitCaseRows <- function(ScotusDf) {
  split_decisions <-
    ScotusDf %>%
    filter(!is.na(direction)) %>%
    mutate(case_vote = paste(caseId, direction, sep = ";"))
  split_decisions <- unique(split_decisions$case_vote) #get frequency count of vote-case combos
  split_decisions <- gsub(pattern = ";.*", replacement = "", x = split_decisions) #get only case id
  split_decisions <- table(split_decisions)
  split_decisions <- split_decisions[split_decisions > 1] #indicates split vote
  split_decisions <- names(split_decisions) #get caseIds
  splitDf <- 
    ScotusDf %>%
    filter(caseId %in% split_decisions)
  
  return(splitDf)
}

######################### Grab Split Decision Rows #########################
GetFiveFourCaseRows <- function(ScotusDf) {
  split_decisions <-
    ScotusDf %>%
    filter(!is.na(direction)) %>%
    mutate(case_vote = paste(caseId, direction, sep = ";"))
  split_decisions <- table(split_decisions$case_vote)
  names(split_decisions) <- gsub(pattern = ";.*", replacement = "", x = names(split_decisions)) #get only case id
  split_decisions <- split_decisions[split_decisions == 5 | split_decisions == 4] #indicates 5-4 votes
  split_decisions <- table(names(split_decisions)) #get caseId frequency
  split_decisions <- split_decisions[split_decisions == 2] #entries with one occurence signify 5-3 split with one absention
  split_decisions <- names(split_decisions)
  splitDf <- 
    ScotusDf %>%
    filter(caseId %in% split_decisions)
  
  return(splitDf)
}

######################### Vote Adjacency Matrix #########################
CreateVoteAdj <- function(ScotusDf) {
  # Create bipartite adjacency matrixes
  MajorityDf <- 
    ScotusDf %>%
    filter(majority == 2) %>%
    mutate(majority = 1) %>% #replace with 1s instead of 2s
    select(caseId, justiceName, majority) %>%
    melt(id = c("caseId", "justiceName")) %>%
    cast(caseId ~ justiceName) #reshape into bipartite matrix
  MinorityDf <- 
    ScotusDf %>%
    filter(majority == 1) %>%
    select(caseId, justiceName, majority) %>%
    melt(id = c("caseId", "justiceName")) %>%
    cast(caseId ~ justiceName)#reshape into bipartite matrix
  
  # Replace NAs 
  MajorityDf[is.na(MajorityDf)] <- 0
  MinorityDf[is.na(MinorityDf)] <- 0
  
  # Change from dataframes to matrices
  MajorityDf <- as.matrix(MajorityDf)
  MinorityDf <- as.matrix(MinorityDf)
  
  # Create adjacency from justice to justice
  MajorityDf <- t(MajorityDf) %*% MajorityDf
  MinorityDf <- t(MinorityDf) %*% MinorityDf

  # Create adjacency matrix
  ScotusAdj <- MajorityDf + MinorityDf
  
  # Divide by total votes to get agreement percent
  ScotusAdj <- ScotusAdj / diag(ScotusAdj)
  
  # Turn into graph abject
  ScotusAdj <- graph.adjacency(ScotusAdj, weighted = TRUE, diag = FALSE, mode = "undirected")
  
  # Finish
  return(ScotusAdj)
}

