########################################################################################
# 
# Supreme Court Voting Network - Supreme Court Database
#
########################################################################################

rm(list = ls())
source("scripts/SCOTUSfunctions.R")

# Load in packages
library(igraph)
library(dplyr)
library(stringr)

# Load in data
load("data/SCDB_2014_01_justiceCentered_Citation.Rdata")
scdb <- SCDB_2014_01_justiceCentered_Citation
rm(SCDB_2014_01_justiceCentered_Citation)

######################### Prepare Data #########################
# Grab columns of interest
scotus <- 
  scdb %>%
  select(caseId, naturalCourt, justice, justiceName, direction, majority, dateDecision, caseName) %>%
  mutate(justiceName = gsub(pattern = ".*([A-Z][a-z]+)", replacement = "\\1", x = justiceName, perl = TRUE))

# Select current court
# Justice Kagan was the most recently appointed justice--appointed 08-07-2010
scotus <-
  scotus %>%
  filter(dateDecision > as.Date("2010-08-07"))

# Unique court cases and cases decided by unanimous vote
length(unique(scotus$caseId)) #316
VoteBreakdown <- SplitPercent(scotus, output = TRUE) # Percent of cases decided by split decision
write.csv(VoteBreakdown, file = "graph/VoteBreakdown.csv")


###########################################################################
# Split Vote
###########################################################################

# Filter df to only split decision cases
splitDf <- GetSplitCaseRows(scotus)

######################### Adjacency Matrix Creation #########################
# Split vote
SplitVoteAdj <- CreateVoteAdj(splitDf)

######################### Create graph #########################
# Create edgelist
edgelist <- get.edgelist(graph = SplitVoteAdj)
edgelist <- cbind(edgelist, E(SplitVoteAdj)$weight)
edgelist <- as.data.frame(edgelist, stringsAsFactors = FALSE)
names(edgelist) <- c("Source", "Target", "Weight")

write.csv(edgelist, file = "output/gephi_scotus_edgelist.csv", row.names = FALSE)

# Create Nodelist
nodelist <- read.csv(file = "output/SCOTUSnodetable.csv", header = TRUE)
nodelist <-
  nodelist %>%
  mutate(Label = Id)

write.csv(nodelist, file = "output/gephi_scotus_nodetable.csv")

######################### Output Adjacency #########################
SplitVoteMatrix <- as.matrix(get.adjacency(SplitVoteAdj, attr = "weight"))
write.table(SplitVoteMatrix, file = "graph/SplitVoteMatrix.csv", sep = ",", row.names = TRUE, col.names = NA)

######################### Graph Analyses #########################

# Centrality
centralityScores <- evcent(SplitVoteAdj, scale = TRUE, weights = E(SplitVoteAdj)$weight) 
centralityScores <- centralityScores[[1]]
# names(centralityScores) <- gsub(pattern = "\"([A-Za-z]+)\"", replacement = "\\1", x = names(centralityScores), perl = TRUE)

write.csv(centralityScores, file = "graph/SplitCentrality.csv", row.names = TRUE)

######################### Vote Type (lone disesenter, etc...) #########################


###########################################################################
# 5-4 Votes
###########################################################################

# Filter to only 5-4 vote cases
FiveFour <- GetFiveFourCaseRows(scotus)

######################### Adjacency Matrix Creation #########################
# Split vote
FiveFourVoteAdj <- CreateVoteAdj(FiveFour)

######################### Create graph #########################
# Create edgelist
edgelist <- get.edgelist(graph = FiveFourVoteAdj)
edgelist <- cbind(edgelist, E(FiveFourVoteAdj)$weight)
edgelist <- as.data.frame(edgelist, stringsAsFactors = FALSE)
names(edgelist) <- c("Source", "Target", "Weight")

write.csv(edgelist, file = "output/gephi_5-4_scotus_edgelist.csv", row.names = FALSE)


######################### Output Adjacency #########################
FiveFourVoteMatrix <- as.matrix(get.adjacency(FiveFourVoteAdj, attr = "weight"))
write.table(FiveFourVoteMatrix, file = "graph/FiveFourVoteMatrix.csv", sep = ",", row.names = TRUE, col.names = NA)

######################### Graph Analyses #########################

# Centrality
centralityScores <- evcent(FiveFourVoteAdj, scale = TRUE, weights = E(FiveFourVoteAdj)$weight) 
centralityScores <- centralityScores[[1]]

write.csv(centralityScores, file = "graph/FiveFourCentrality.csv", row.names = TRUE)

 
###########################################################################
# All Votes
###########################################################################

# All vote
AllVoteAdj <- CreateVoteAdj(scotus)

######################### Create graph #########################
# Create edgelist
edgelist <- get.edgelist(graph = AllVoteAdj)
edgelist <- cbind(edgelist, E(AllVoteAdj)$weight)
edgelist <- as.data.frame(edgelist)
names(edgelist) <- c("Source", "Target", "Weight")

write.csv(edgelist, file = "output/gephi_Allscotus_edgelist.csv")

######################### Output Adjacency #########################
AllVoteMatrix <- as.matrix(get.adjacency(AllVoteAdj, attr = "weight"))
write.table(AllVoteMatrix, file = "graph/AllVoteMatrix.csv", sep = ",", row.names = TRUE, col.names = NA)

######################### Graph Analyses #########################

# Centrality
centralityScores <- evcent(AllVoteAdj, scale = TRUE, weights = E(AllVoteAdj)$weight) 
centralityScores <- centralityScores[[1]]

write.csv(centralityScores, file = "graph/AllCentrality.csv", row.names = TRUE)
