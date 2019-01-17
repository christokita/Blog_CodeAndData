########################################################################################
# 
# Demographics of USA vs. Political Parties
#
########################################################################################

rm(list = ls())
source("scripts/DemographicsUtility.R")

##################################################################
# Calculate party overall errors
##################################################################

###################### Mean Percentage Error ######################
# Load in data
parties <- read.csv("data/EthnicPartyBreakdown.csv", header = TRUE, stringsAsFactors = FALSE)

###################### Calculate Mean Square Error ######################
mSError <- MeanSquaredError(df = parties)

###################### Calculate Proportion Square Error ######################
pSError <- CustomSquaredError(df = parties)

###################### Create table ######################
errorTable <- cbind(mSError, pSError)
write.csv(errorTable, file = "output/ErrorTable.csv")


##################################################################
# Calculate party errors by ethnicity
##################################################################

errorTable <- ErrorTable(df = parties)
republicanError <- errorTable[[1]]
independentError <- errorTable[[2]]
democratError <- errorTable[[3]]

columns <- c("Ethnicity", "USA.Population")

outputTable <- merge(republicanError, independentError, by = columns)
outputTable <- merge(outputTable, democratError, by = columns)

write.csv(outputTable, file = "output/EthnicityErrorTable.csv", row.names = TRUE)
