########################################################################################
# 
# Demographics of USA vs. Political Parties Utility Functions
#
########################################################################################
library(dplyr)

###################### Predictions Error ######################
MeanSquaredError <- function(df) {
  categories <- names(df)[!names(df) %in% c("Ethnicity", "USA.Population")]
  
  out <- lapply(1:length(categories), function(x){
    temp <- df[, c(1, x + 1 , 5)]
    temp$squareError <- (temp[, 2] - temp[, c("USA.Population")])^2 #calculate squared errors
    error <- sum(temp$squareError) / nrow(df)
    names(error) <- categories[x]
    return(error)
  })
  out <- cbind(unlist(out))
  colnames(out) <- "Mean Square Error"
  
  return(out)
}

###################### Custom Proportion Error ######################
CustomSquaredError <- function(df) {
  categories <- names(df)[!names(df) %in% c("Ethnicity", "USA.Population")]
  
  out <- lapply(1:length(categories), function(x){
    temp <- df[, c(1, x + 1 , 5)]
    temp$squareError <- (temp[, 2] - temp[, c("USA.Population")]) 
    temp$squareError <- (temp$squareError / temp[, c("USA.Population")])^2
    error <- sum(temp$squareError) / nrow(df)
    names(error) <- categories[x]
    return(error)
  })
  out <- cbind(unlist(out))
  colnames(out) <- "Proportion Square Error"
  
  return(out)
}

###################### Error Types ######################
ErrorTable <- function(df) {
#   df$squareError <- (df[, 2] - df[, c("USA.Population")])^2 #calculate squared errors
#   df$proportionError <- (df$squareError / df[, c("USA.Population")])^2
  categories <- names(df)[!names(df) %in% c("Ethnicity", "USA.Population")]
  
  out <- lapply(1:length(categories), function(x){
    temp <- df[, c(1, x + 1 , 5)]
    temp$squareError <- (temp[, 2] - temp[, c("USA.Population")])^2 #calculate squared errors
    temp$proportionError <- (sqrt(temp$squareError) / temp[, c("USA.Population")])^2
    return(temp)
  })

  return(out)
}
