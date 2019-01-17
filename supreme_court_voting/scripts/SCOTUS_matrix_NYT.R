########################################################################################
# 
# Supreme Court Voting Network - NYT Data
#
########################################################################################
require(igraph)


# Edgelist creation
scotus <- read.csv("Supreme court voting matrix.csv", header = TRUE, sep = ",") #read in csv
row.names(scotus) <- scotus[,1] #create rownames
scotus <- scotus[,2:10] 
scotus <- as.matrix(scotus)

scotus <- graph.adjacency(scotus, weighted = TRUE, diag = FALSE) #create adjacency matrix
scotus <- as.undirected(graph = scotus)

edgelist <- get.edgelist(graph = scotus)
edgelist <- cbind(edgelist, E(scotus)$weight)
edgelist <- as.data.frame(edgelist)
names(edgelist) <- c("Source", "Target", "Weight")

# Nodetable creation
nodetable <- read.csv("SCOTUS nodetable.csv", header = FALSE, sep = ",")
names(nodetable) <- c("Id", "Label", "President", "Party")
 
# Export for Gephi
write.table(edgelist, "gephi_scotus_edgelist.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(nodetable, "gephi_scotus_nodetable.csv", sep = ",", row.names = FALSE, col.names = TRUE)

